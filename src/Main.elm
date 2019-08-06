module Main exposing (main)

import Browser
import Duration exposing (hours, minutes)
import Element exposing (Element, column, fill, height, layout, px, row, spacing, text, width)
import Element.Input as Input
import Schedule exposing (Reservation(..), ReservationId(..), ResourceId(..), Schedule, newResource, newSchedule)
import Sheet exposing (Cell(..), Draggable(..), Droppable(..), Sheet)
import Task exposing (andThen, perform)
import Theme exposing (Theme)
import Time exposing (Month(..), Posix)
import Time.Extra exposing (Interval(..))
import TimeWindow exposing (TimeWindow)


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
    | Today
    | NewTime Posix


type alias Model =
    { currPage : Page
    , sheet : Sheet
    , theme : Theme
    , currentTime : Posix
    }


type alias Flags =
    ()


t : Int -> Posix
t ms =
    Time.Extra.partsToPosix Time.utc { year = 2019, month = Aug, day = 6, hour = 10, minute = 0, second = 0, millisecond = 0 }
        |> Time.Extra.add Millisecond ms Time.utc


stressTestSchedule : List Schedule
stressTestSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "ZS 672AE" 0)
        [ Schedule.newReservation (ReservationId "r1") (t (1000 * 60 * 30)) (hours 4) ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 8127S" 1)
        [ Schedule.newReservation (ReservationId "r2") (t (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r3") (t (1000 * 60 * 60)) (hours 4)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 1234" 2)
        (List.range
            0
            1000
            |> List.map (\i -> Schedule.newReservation (ReservationId "r4") (t <| i * 5 * 1000 * 60) (minutes <| toFloat <| 5))
        )
    , newSchedule
        (newResource (ResourceId "id1") "ZS AAAAA" 3)
        [ Schedule.newReservation (ReservationId "r5") (t (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r6") (t (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r7") (t (1000 * 60 * 360)) (hours 2)
        , Schedule.newReservation (ReservationId "r7") (t (36 * 60 * 60 * 1000 + 1000 * 60 * 360)) (hours 2)
        ]
    ]
        ++ (List.range 0 100
                |> List.map
                    (\i ->
                        newSchedule
                            (newResource (ResourceId "id1") "ZS AAAAA" (4 + i))
                            [ Schedule.newReservation (ReservationId <| "rx" ++ String.fromInt i) (t (1000 * 60 * 180)) (hours 1)
                            ]
                    )
           )


sampleSchedule : List Schedule
sampleSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "ZS 672AE" 0)
        [ Schedule.newReservation (ReservationId "r1.1") (t (1000 * 60 * 30)) (hours 4) ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 8127S" 1)
        [ Schedule.newReservation (ReservationId "r2.1") (t (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r2.2") (t (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r2.3") (t (23 * 1000 * 60 * 60 + 30 * 60 * 1000)) (hours 4)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS 1234" 2)
        [ Schedule.newReservation (ReservationId "r3.1") (t 0) (minutes 65)
        ]
    , newSchedule
        (newResource (ResourceId "id1") "ZS AAAAA" 3)
        [ Schedule.newReservation (ReservationId "r4.1") (t (1000 * 60 * 180)) (hours 1)
        , Schedule.newReservation (ReservationId "r4.2") (t (1000 * 60 * 60)) (hours 4)
        , Schedule.newReservation (ReservationId "r4.3") (t (1000 * 60 * 360)) (hours 2)
        , Schedule.newReservation (ReservationId "r4.4") (t (1000 * 60 * 360)) (minutes 2)
        , Schedule.newReservation (ReservationId "r4.5") (t (1000 * 60 * 480)) (minutes 45)
        ]
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        window =
            -- TimeWindow.make (t (30 * 60 * 1000)) (Duration.hours 2)
            TimeWindow.make (t 0) (Duration.hours 24)
                |> Debug.log "before"
                |> TimeWindow.toDay Time.utc
                |> Debug.log "after"
    in
    ( { currPage = InputPage

      -- , sheet = Sheet.make 48 window sampleSchedule
      , sheet = Sheet.make window sampleSchedule
      , theme = Theme.defaultTheme (Duration.minutes 30) window
      , currentTime = Time.millisToPosix 0 -- Cheating a bit.
      }
    , perform NewTime Time.now
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
                newWindow =
                    model.sheet.window |> TimeWindow.toDay Time.utc
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        ViewWeek ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.toWeek Time.utc
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        ViewMonth ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.toMonth Time.utc
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        PreviousPeriod ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.goBack (TimeWindow.getDuration model.sheet.window)
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        NextPeriod ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.goForward (TimeWindow.getDuration model.sheet.window |> Debug.log "next")
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        Today ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.goToDay Time.utc model.currentTime
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        NewTime time ->
            ( { model | currentTime = time }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-resource"
    , body =
        [ layout [] <|
            column [ width fill, height (px 40) ]
                [ row [ width fill, height fill, spacing 10 ]
                    [ Input.button [] { onPress = Just Reset, label = text "Reset" }
                    , Input.button [] { onPress = Just ViewDay, label = text "Day" }
                    , Input.button [] { onPress = Just ViewWeek, label = text "Week" }
                    , Input.button [] { onPress = Just ViewMonth, label = text "Month" }
                    , Input.button [] { onPress = Just PreviousPeriod, label = text "<" }
                    , Input.button [] { onPress = Just NextPeriod, label = text ">" }
                    , Input.button [] { onPress = Just Today, label = text "Today" }
                    ]
                , row [ width fill, height fill ]
                    [ viewSheet model.sheet model.theme
                        |> Element.map SheetMsg
                    ]
                ]
        ]
    }


setSheetWindow : TimeWindow -> Model -> Model
setSheetWindow newWindow model =
    { model
        | sheet = Sheet.make newWindow sampleSchedule
        , theme = Theme.defaultTheme (Duration.minutes 30) newWindow
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
