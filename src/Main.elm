module Main exposing (main)

import Browser
import Duration exposing (hours, minutes)
import Element exposing (Element, column, fill, height, layout, text, width)
import Element.Input as Input
import Schedule exposing (Reservation(..), ReservationId(..), ResourceId(..), Schedule, newResource, newSchedule)
import Sheet exposing (Cell(..), Draggable(..), Droppable(..), Sheet)
import Theme exposing (Theme)
import Time
import TimeWindow


type Page
    = InputPage


type Msg
    = SheetMsg Sheet.Msg
    | Reset
    | ViewDay
    | ViewWeek
    | ViewMonth
    | PreviousPeriod
    | NextPeriod


type alias Model =
    { currPage : Page
    , sheet : Sheet
    , theme : Theme
    }


type alias Flags =
    ()


stressTestSchedule : List Schedule
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


sampleSchedule : List Schedule
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
    in
    ( { currPage = InputPage

      -- , sheet = Sheet.make 48 window sampleSchedule
      , sheet = Sheet.make window sampleSchedule
      , theme = Theme.defaultTheme (Duration.minutes 30) window
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

        ViewDay ->
            let
                updatedSheet =
                    model.sheet |> Sheet.dayView
            in
            ( { model
                | sheet = updatedSheet
                , theme = Theme.defaultTheme (Duration.minutes 30) updatedSheet.window
              }
            , Cmd.none
            )

        ViewWeek ->
            let
                updatedSheet =
                    model.sheet |> Sheet.weekView
            in
            ( { model
                | sheet = updatedSheet
                , theme = Theme.defaultTheme (Duration.minutes 30) updatedSheet.window
              }
            , Cmd.none
            )

        ViewMonth ->
            let
                updatedSheet =
                    model.sheet |> Sheet.monthView
            in
            ( { model
                | sheet = updatedSheet
                , theme = Theme.defaultTheme (Duration.minutes 30) updatedSheet.window
              }
            , Cmd.none
            )

        PreviousPeriod ->
            let
                updatedSheet =
                    model.sheet |> Sheet.prev
            in
            ( { model
                | sheet = updatedSheet
                , theme = Theme.defaultTheme (Duration.minutes 30) updatedSheet.window
              }
            , Cmd.none
            )

        NextPeriod ->
            let
                updatedSheet =
                    model.sheet |> Sheet.next
            in
            ( { model
                | sheet = updatedSheet
                , theme = Theme.defaultTheme (Duration.minutes 30) updatedSheet.window
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-resource"
    , body =
        [ layout [] <|
            column [ width fill, height fill ]
                [ Input.button [] { onPress = Just Reset, label = text "Reset" }
                , Input.button [] { onPress = Just ViewDay, label = text "Day" }
                , Input.button [] { onPress = Just ViewWeek, label = text "Week" }
                , Input.button [] { onPress = Just ViewMonth, label = text "Month" }
                , Input.button [] { onPress = Just PreviousPeriod, label = text "<" }
                , Input.button [] { onPress = Just NextPeriod, label = text ">" }
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
