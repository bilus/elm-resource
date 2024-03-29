module Main exposing (main)

import Browser
import Duration exposing (hours, minutes)
import Element exposing (Element, alignRight, column, fill, height, inFront, layout, padding, paddingXY, px, rgba, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Overlay.Connections
import Process
import Scheduler.Cell exposing (Cell(..))
import Scheduler.Connection as Connection exposing (..)
import Scheduler.Schedule as Schedule exposing (Reservation(..), ReservationId(..), ResourceId(..), Schedule, newResource, newSchedule)
import Scheduler.Sheet as Sheet exposing (Draggable(..), Droppable(..), Sheet)
import Scheduler.Theme as Theme exposing (Theme)
import Scheduler.TimeWindow as TimeWindow exposing (TimeWindow)
import Task exposing (perform)
import Time exposing (Month(..), Posix)
import Time.Extra exposing (Interval(..))
import Util.List


type Msg
    = SheetMsg Sheet.Msg
    | ViewDay
    | ViewWeek
    | ViewMonth
    | PreviousPeriod
    | NextPeriod
    | Today
    | NewTime Posix


type alias ConnectionData =
    ()


type alias Model =
    { sheet : Sheet
    , connections : List (Connection ConnectionData)
    , currentTime : Posix
    , zone : Time.Zone
    }


type alias Flags =
    ()


t : Int -> Posix
t ms =
    Time.Extra.partsToPosix Time.utc { year = 2021, month = Jun, day = 20, hour = 10, minute = 0, second = 0, millisecond = 0 }
        |> Time.Extra.add Millisecond ms Time.utc


sampleSchedule : List Schedule
sampleSchedule =
    [ newSchedule
        (newResource (ResourceId "id1") "Heroify" 0)
        [ Schedule.newReservation (ReservationId "r1.1") (t (1000 * 60 * 30)) (hours <| 24 * 14) (Just 0) ]
    , newSchedule
        (newResource (ResourceId "id2") "DTS" 1)
        [ Schedule.newReservation (ReservationId "r2.1") (t (1000 * 60 * 180)) (hours <| 24 * 5) (Just 0)
        , Schedule.newReservation (ReservationId "r2.2") (t (1000 * 60 * 180)) (hours <| 24 * 21) (Just 1)
        , Schedule.newReservation (ReservationId "r2.3") (t (1000 * 60 * 60)) (hours 4) (Just 1)
        , Schedule.newReservation (ReservationId "r2.4") (t (23 * 1000 * 60 * 60 + 30 * 60 * 1000)) (hours 4) (Just 1)
        ]
    , newSchedule
        (newResource (ResourceId "id3") "Rho" 2)
        [ Schedule.newReservation (ReservationId "r3.1") (t 0) (minutes 65) (Just 3)
        ]
    , newSchedule
        (newResource (ResourceId "id4") "Roman" 3)
        [ Schedule.newReservation (ReservationId "r4.1") (t (1000 * 60 * 180)) (hours 1) (Just 3)
        , Schedule.newReservation (ReservationId "r4.2") (t (1000 * 60 * 60)) (hours 4) (Just 3)
        , Schedule.newReservation (ReservationId "r4.3") (t (1000 * 60 * 360)) (hours 2) (Just 0)
        , Schedule.newReservation (ReservationId "r4.4") (t (1000 * 60 * 360)) (minutes 2) (Just 0)
        , Schedule.newReservation (ReservationId "r4.5") (t (1000 * 60 * 480)) (minutes 45) (Just 1)
        , Schedule.newReservation (ReservationId "r4.1") (t (1000 * 60 * 180)) (hours <| 24 * 7) (Just 1)
        ]
    ]


connection : Sheet -> Connection.Kind -> ( ReservationId, Posix ) -> ( ReservationId, Posix ) -> Maybe (Connection ())
connection sheet kind ( from, fromTime ) ( to, toTime ) =
    Maybe.map2
        (\fromCell toCell ->
            { fromCell = fromCell
            , fromTime = fromTime
            , toCell = toCell
            , toTime = toTime
            , data = ()
            , notes = ""
            , kind = kind
            }
        )
        (Sheet.findReservedCell sheet <| from)
        (Sheet.findReservedCell sheet <| to)


sampleConnections : Sheet -> List (Connection ())
sampleConnections sheet =
    [ connection sheet
        Connection.Strong
        ( Schedule.ReservationId "r2.3", t (1000 * 60 * 180 + 1000 * 60 * 60 * 24 * 7) )
        ( Schedule.ReservationId "r1.1", t (1000 * 60 * 30) )
    , connection
        sheet
        Connection.Blocked
        ( Schedule.ReservationId "r1.1", t (1000 * 60 * 30) )
        ( Schedule.ReservationId "r2.3", t (1000 * 60 * 180 + 1000 * 60 * 60 * 24 * 10) )
    , connection sheet
        Connection.Weak
        ( Schedule.ReservationId "r1.1", t (1000 * 60 * 30) )
        ( Schedule.ReservationId "r3.1", t (1000 * 60 * 180 + 1000 * 60 * 60 * 24 * 3) )
    ]
        |> Util.List.compact


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        -- TODO: Time.here (w/elm-task-parallel)
        -- TODO: Time.utc is all over Theme
        zone =
            Time.utc

        window =
            TimeWindow.makeMonth zone (t 0)

        sheet =
            Sheet.make window sampleSchedule True
    in
    ( { -- , sheet = Sheet.make 48 window sampleSchedule
        sheet = sheet
      , connections = sampleConnections sheet
      , zone = zone
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
                    model.sheet.window |> TimeWindow.goForward (TimeWindow.getDuration model.sheet.window)
            in
            ( model |> setSheetWindow newWindow
            , Cmd.none
            )

        Today ->
            let
                newWindow =
                    model.sheet.window
                        |> TimeWindow.toDay Time.utc
                        |> TimeWindow.goToDay Time.utc model.currentTime
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
    let
        theme =
            makeTheme model

        overlay =
            Overlay.Connections.render model.sheet theme model.connections
                |> Element.map SheetMsg

        sheet =
            viewSheet model.sheet theme
                |> Element.map SheetMsg
    in
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
                    [ Element.el
                        [ width shrink
                        , height shrink
                        , inFront overlay
                        ]
                        sheet
                    ]
                ]
        ]
    }


setSheetWindow : TimeWindow -> Model -> Model
setSheetWindow newWindow model =
    { model
        | sheet =
            Sheet.make newWindow sampleSchedule True
                |> Sheet.setNowMarker (Just model.currentTime)
    }


makeTheme : Model -> Theme
makeTheme model =
    let
        window =
            model.sheet.window

        zone =
            model.zone

        defaultTheme =
            Theme.defaultTheme (Duration.days 7) window

        theme =
            { defaultTheme
                | showDayBoundaries = False
                , timeCell =
                    { timeCell
                        | label = timeLabel zone
                        , widthPx = 200
                    }
            }

        timeCell =
            defaultTheme.timeCell

        _ =
            Overlay.Connections.render model.sheet theme model.connections
    in
    theme



-- { theme | overlay = Just overlay }


timeLabel : Time.Zone -> TimeWindow -> TimeWindow -> String
timeLabel zone _ window =
    let
        start =
            TimeWindow.getStart window

        month =
            String.fromInt <| monthNumber <| Time.toMonth zone start

        day =
            String.fromInt <| Time.toDay zone start

        paddedDay =
            String.padLeft 2 '0' day
    in
    month ++ "/" ++ paddedDay


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


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
