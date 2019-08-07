module Main exposing (main)

import Browser
import Duration exposing (hours, minutes)
import Element exposing (Element, alignRight, column, fill, height, layout, padding, paddingXY, px, rgba, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Process
import Schedule exposing (Reservation(..), ReservationId(..), ResourceId(..), Schedule, newResource, newSchedule)
import Sheet exposing (Cell(..), Draggable(..), Droppable(..), Sheet)
import Task exposing (andThen, perform)
import Theme exposing (Theme)
import Time exposing (Month(..), Posix, Zone)
import Time.Extra exposing (Interval(..))
import TimeWindow exposing (TimeWindow)


type Msg
    = SheetMsg Sheet.Msg
    | ViewDay
    | ViewWeek
    | ViewMonth
    | PreviousPeriod
    | NextPeriod
    | Today
    | NewTime Posix


type alias Model =
    { sheet : Sheet
    , theme : Theme
    , currentTime : Posix
    , userZone : Zone
    }


type alias Flags =
    ()


sampleTime : Zone -> Int -> Posix
sampleTime zone ms =
    Time.Extra.partsToPosix zone { year = 2019, month = Aug, day = 6, hour = 10, minute = 0, second = 0, millisecond = 0 }
        |> Time.Extra.add Millisecond ms zone


stressTestSchedule : Zone -> List Schedule
stressTestSchedule zone =
    let
        t =
            sampleTime zone
    in
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


sampleSchedule : Zone -> List Schedule
sampleSchedule zone =
    let
        t =
            sampleTime zone
    in
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
        zone =
            Time.utc

        -- TODO: Get it from browser
        window =
            TimeWindow.makeDay zone (sampleTime zone 0)

        -- TODO: User currentTime
    in
    ( { -- , sheet = Sheet.make 48 window sampleSchedule
        sheet = Sheet.make window (sampleSchedule zone)
      , theme = Theme.defaultTheme (Duration.minutes 30) window
      , currentTime = Time.millisToPosix 0 -- Cheating a bit.
      , userZone = zone
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

        ViewDay ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.toDay model.userZone
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        ViewWeek ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.toWeek model.userZone
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        ViewMonth ->
            let
                newWindow =
                    model.sheet.window |> TimeWindow.toMonth model.userZone
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
                    model.sheet.window |> TimeWindow.goForward (TimeWindow.getDuration model.sheet.window)
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        Today ->
            let
                newWindow =
                    model.sheet.window
                        |> TimeWindow.toDay model.userZone
                        |> TimeWindow.goToDay model.userZone model.currentTime
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        NewTime time ->
            ( { model
                | currentTime = time
                , sheet = model.sheet |> Sheet.setNowMarker (Just time)
              }
            , Process.sleep (60.0 * 1000.0)
                -- Update current time indicator every minute
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform NewTime
            )



-- VIEW


btn : String -> Msg -> Element Msg
btn title msg =
    Input.button
        [ Background.color <| rgba 0.6 0.6 1 0.7
        , Font.size 15
        , Border.color <| rgba 0.4 0.4 0.8 0.7
        , Border.width 1
        , paddingXY 8 5
        ]
        { onPress = Just msg, label = text title }


view : Model -> Browser.Document Msg
view model =
    { title = "elm-resource"
    , body =
        [ layout [] <|
            column [ width fill, height (px 40) ]
                [ row [ width shrink, height fill, spacing 10, alignRight, padding 5 ]
                    [ btn "Day" ViewDay
                    , btn "Week" ViewWeek
                    , btn "Month" ViewMonth
                    , btn "◅" PreviousPeriod
                    , btn "▻" NextPeriod
                    , btn "Today" Today
                    ]
                , row [ width fill, height fill ]
                    [ viewSheet model.sheet model.theme model.userZone
                        |> Element.map SheetMsg
                    ]
                ]
        ]
    }


setSheetWindow : TimeWindow -> Model -> Model
setSheetWindow newWindow model =
    { model
        | sheet =
            Sheet.make newWindow (sampleSchedule model.userZone)
                |> Sheet.setNowMarker (Just model.currentTime)
        , theme = Theme.defaultTheme (Duration.minutes 30) newWindow
    }


viewSheet : Sheet -> Theme -> Zone -> Element Sheet.Msg
viewSheet sheet theme zone =
    sheet |> Theme.sheetFrame { zone = zone } theme


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
