module Main exposing (main)

import Browser
import Duration as Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes exposing (style)
import Iso8601
import List.Extra
import Schedule exposing (ResourceId(..), Schedule, mapReservations, newResource, newSchedule)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)
import Timetable exposing (Timeslot(..), Timetable)


type Page
    = InputPage


type Msg
    = NoOp


type alias Model =
    { currPage : Page
    , timetable : Timetable
    }


type alias Flags =
    ()


sampleTimetable =
    Timetable.newFromSchedules
        48
        (TimeWindow.make (Time.millisToPosix 1562533510000) (Duration.hours 24))
        [ newSchedule
            (newResource (ResourceId "id1") "ZS 672AE")
            []
        , newSchedule
            (newResource (ResourceId "id1") "ZS 8127S")
            []
        ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , timetable = sampleTimetable
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
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "VisExp"
    , body =
        [ layout [] <| viewTimetable model.timetable
        ]
    }


viewTimetable : Timetable -> Element Msg
viewTimetable timetable =
    let
        header attrs =
            viewTimetableHeaderRow attrs timetable

        data =
            Timetable.mapTimeslotsOverTime
                viewTimetableRow
                timetable
    in
    row [ width fill, height fill ]
        [ column [ width fill, inFront <| header sticky ] <|
            header []
                :: data
        ]


viewTimetableHeaderRow : List (Attribute Msg) -> Timetable -> Element Msg
viewTimetableHeaderRow attrs timetable =
    let
        resourceNames =
            Timetable.getResourceNames timetable

        titles =
            ( "Czas", alignRight )
                :: (resourceNames
                        |> List.map (\name -> ( name, alignLeft ))
                   )

        columns =
            titles
                |> List.map (\( title, align ) -> column [ width <| fillPortion 1 ] [ el [ align ] <| text title ])
    in
    row
        ([ width fill
         , Background.color <| rgba 0.8 0.8 0.8 0.8
         , spacing 3
         ]
            ++ attrs
        )
        columns


viewTimetableRow : TimeWindow -> List Timeslot -> Element Msg
viewTimetableRow window timeslots =
    row
        [ width fill
        , spacing 3
        ]
        (viewOffsetCol [ width <| fillPortion 1 ] offset
            :: (timeslots
                    |> List.map (\timeslot -> column [ width <| fillPortion 1 ] [ text "XXX" ])
               )
        )


viewOffsetCol : List (Attribute Msg) -> Duration -> Element Msg
viewOffsetCol attrs offset =
    column ([] ++ attrs) <| [ el [ alignRight ] <| text <| formatOffset offset ]


viewTimeslotRow : Timeslot -> Element Msg
viewTimeslotRow timeslot =
    case timeslot of
        Available _ ->
            row [] []

        Reserved _ ->
            row [] [ text "+" ]

        Overbooked _ ->
            row [] [ text "!" ]


sticky : List (Attribute msg)
sticky =
    List.map htmlAttribute [ style "position" "sticky", style "top" "0" ]


formatOffset : Duration -> String
formatOffset offset =
    let
        hours =
            Duration.inHours offset

        fullHours =
            floor hours

        minutes =
            60 * (hours - toFloat fullHours) |> round
    in
    String.fromInt fullHours ++ ":" ++ String.fromInt minutes


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
