module Main exposing (main)

import Browser
import Duration as Duration exposing (Duration, hours)
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes exposing (style)
import Iso8601
import List.Extra
import Schedule exposing (ReservationId(..), ResourceId(..), Schedule, mapReservations, newResource, newSchedule)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)
import Timetable exposing (Allocation(..), ColumnStyle, Timetable)


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
            [ Schedule.newReservation (ReservationId "r1") (Time.millisToPosix (1000 * 60 * 30)) (hours 4) ]
        , newSchedule
            (newResource (ResourceId "id1") "ZS 8127S")
            [ Schedule.newReservation (ReservationId "r2") (Time.millisToPosix (1000 * 60 * 180)) (hours 1) ]
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
            ( "Czas", [ width <| fillPortion 1 ], [ alignRight ] )
                :: (resourceNames
                        |> List.map (\name -> ( name, [ width <| fillPortion 4 ], [ centerX ] ))
                   )

        columns =
            titles
                |> List.map (\( title, colAttrs, textAttrs ) -> column colAttrs [ el textAttrs <| text title ])
    in
    row
        ([ width fill
         , Background.color <| rgba 0.8 0.8 0.8 0.8
         , spacing 3
         ]
            ++ attrs
        )
        columns


viewTimetableRow : TimeWindow -> List ( Allocation, ColumnStyle ) -> Element Msg
viewTimetableRow window cols =
    row
        [ width fill
        , spacing 3
        ]
        (viewOffsetCol [ width <| fillPortion 1 ] window
            :: (cols
                    |> List.map (\col -> column [ width <| fillPortion 4, height fill ] [ viewAllocation col ])
               )
        )


viewOffsetCol : List (Attribute Msg) -> TimeWindow -> Element Msg
viewOffsetCol attrs window =
    column ([] ++ attrs) <| [ el [ alignRight ] <| text <| TimeWindow.formatStart window ]


viewAllocation : ( Allocation, ColumnStyle ) -> Element Msg
viewAllocation ( allocation, style ) =
    case allocation of
        Available ->
            row [ width fill, height fill ] []

        Reserved _ ->
            row [ width fill, height fill, Background.color style.reservedColor ] []

        Overbooked _ ->
            row [ width fill, height fill, Background.color style.overbookedColor ] []


sticky : List (Attribute msg)
sticky =
    List.map htmlAttribute [ style "position" "sticky", style "top" "0" ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
