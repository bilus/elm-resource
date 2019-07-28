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
import Sheet exposing (Cell(..), CellRef, CellState, Column(..), ColumnRef, Draggable(..), Droppable(..), Sheet, SubColumn)
import Theme exposing (Theme)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)
import Util.Selectable as Selectable


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


sampleSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "ZS 672AE")
        [ Schedule.newReservation (ReservationId "r1") (Time.millisToPosix (1000 * 60 * 30)) (hours 4) ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 8127S")
        [ Schedule.newReservation (ReservationId "r2") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r3") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 1234")
        [ Schedule.newReservation (ReservationId "r4") (Time.millisToPosix 0) (minutes 15)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS AAAAA")
        [ Schedule.newReservation (ReservationId "r5") (Time.millisToPosix (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r6") (Time.millisToPosix (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r7") (Time.millisToPosix (1000 * 60 * 360)) (hours 2)
        ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        window =
            TimeWindow.make (Time.millisToPosix 0) (Duration.hours 24)
    in
    ( { currPage = InputPage
      , sheet = Sheet.make 48 window sampleSchedule
      , theme = Theme.defaultTheme 48 window
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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


formatTime : Posix -> String
formatTime t =
    let
        zone =
            Time.utc

        -- TODO: Make zone configurable or use CEST
        hours =
            String.fromInt (Time.toHour zone t)
                |> String.padLeft 2 '0'

        minutes =
            String.fromInt (Time.toMinute zone t)
                |> String.padLeft 2 '0'
    in
    hours ++ ":" ++ minutes


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
