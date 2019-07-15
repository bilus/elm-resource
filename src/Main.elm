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
import Timetable exposing (Allocation(..), Timetable)


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
        (TimeWindow.make (Time.millisToPosix 0) (Duration.hours 24))
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
            Timetable.mapRows viewTimetableRow timetable
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


viewTimetableRow : TimeWindow -> List Allocation -> Element Msg
viewTimetableRow window allocations =
    row
        [ width fill
        , spacing 3
        ]
        (viewOffsetCol [ width <| fillPortion 1 ] window
            :: (allocations
                    |> List.map (\allocation -> column [ width <| fillPortion 1 ] [ viewAllocation allocation ])
               )
        )


viewOffsetCol : List (Attribute Msg) -> TimeWindow -> Element Msg
viewOffsetCol attrs window =
    column ([] ++ attrs) <| [ el [ alignRight ] <| text <| TimeWindow.formatStart window ]


viewAllocation : Allocation -> Element Msg
viewAllocation allocation =
    case allocation of
        Available ->
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
