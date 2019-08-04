module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import DragDrop
import Duration exposing (Duration, hours, minutes, seconds)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Iso8601
import List.Extra
import Maybe.Extra
import Schedule exposing (Reservation(..), ReservationId(..), Resource, ResourceId(..), Schedule, mapReservations, newResource, newSchedule)
import Sheet exposing (Cell(..), CellRef, Column, ColumnRef, Draggable(..), Droppable(..), Sheet, SubColumn)
import Theme exposing (Theme)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)


type Page
    = InputPage


type Msg
    = SheetMsg Sheet.Msg
    | Reset
    | NoOp


type alias Model =
    { currPage : Page
    , sheet : Sheet
    , theme : Theme
    }


type alias Flags =
    ()


stressTestSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "ZS 672AE" 0)
        [ Schedule.newReservation (ReservationId "r1") (Time.millisToPosix (1000 * 60 * 30)) (hours 4) ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 8127S" 1)
        [ Schedule.newReservation (ReservationId "r2") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r3") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 1234" 2)
        (List.range
            0
            1000
            |> List.map (\i -> Schedule.newReservation (ReservationId "r4") (Time.millisToPosix <| i * 5 * 1000 * 60) (minutes <| toFloat <| 5))
        )
    , newSchedule
        (newResource (ResourceId "id1") "ZS AAAAA" 3)
        [ Schedule.newReservation (ReservationId "r5") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r6") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r7") (Time.millisToPosix (1000 * 60 * 360)) (hours 2)
        , Schedule.newReservation (ReservationId "r7") (Time.millisToPosix (36 * 60 * 60 * 1000 + 1000 * 60 * 360)) (hours 2)
        ]
    ]
        ++ (List.range 0 100
                |> List.map
                    (\i ->
                        newSchedule
                            (newResource (ResourceId "id1") "ZS AAAAA" (4 + i))
                            [ Schedule.newReservation (ReservationId <| "rx" ++ String.fromInt i) (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
                            ]
                    )
           )


sampleSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "ZS 672AE" 0)
        [ Schedule.newReservation (ReservationId "r1.1") (Time.millisToPosix (1000 * 60 * 30)) (hours 4) ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 8127S" 1)
        [ Schedule.newReservation (ReservationId "r2.1") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r2.2") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r2.3") (Time.millisToPosix (23 * 1000 * 60 * 60 + 30 * 60 * 1000)) (hours 4)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 1234" 2)
        [ Schedule.newReservation (ReservationId "r3.1") (Time.millisToPosix 0) (minutes 65)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS AAAAA" 3)
        [ Schedule.newReservation (ReservationId "r4.1") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r4.2") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r4.3") (Time.millisToPosix (1000 * 60 * 360)) (hours 2)
        , Schedule.newReservation (ReservationId "r4.4") (Time.millisToPosix (1000 * 60 * 360)) (minutes 2)
        , Schedule.newReservation (ReservationId "r4.5") (Time.millisToPosix (1000 * 60 * 480)) (minutes 45)
        ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        window =
            -- TimeWindow.make (Time.millisToPosix (30 * 60 * 1000)) (Duration.hours 2)
            TimeWindow.make (Time.millisToPosix (60 * 60 * 1000)) (Duration.hours 24)

        slotCount =
            48
    in
    ( { currPage = InputPage

      -- , sheet = Sheet.make 48 window sampleSchedule
      , sheet = Sheet.make window sampleSchedule
      , theme = Theme.defaultTheme slotCount window
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sheet.subscribe model.sheet
        |> Sub.map SheetMsg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetMsg sheetMsg ->
            let
                ( updatedSheet, cmd ) =
                    Sheet.update sheetMsg model.sheet
            in
            ( { model | sheet = updatedSheet }, Cmd.map SheetMsg cmd )

        Reset ->
            init ()

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-resource"
    , body =
        [ layout [] <|
            column [ width fill, height fill ]
                [ Input.button [] { onPress = Just Reset, label = text "Reset" }
                , viewSheet model.sheet model.theme
                    |> Element.map SheetMsg
                ]
        ]
    }


viewSheet : Sheet -> Theme -> Element Sheet.Msg
viewSheet sheet theme =
    sheet |> Theme.sheetFrame theme


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
