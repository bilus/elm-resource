module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import Duration exposing (Duration, hours, minutes, seconds)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font exposing (Font)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Iso8601
import List.Extra
import Maybe.Extra
import Schedule exposing (Reservation(..), ReservationId(..), Resource, ResourceId(..), Schedule, mapReservations, newResource, newSchedule)
import Sheet exposing (Cell(..), Column(..), Sheet, SubColumn)
import Theme
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)


type Page
    = InputPage


type Msg
    = NoOp


type alias Model =
    { currPage : Page
    , sheet : Sheet
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
    ( { currPage = InputPage
      , sheet = Sheet.make Theme.defaultTheme 48 (TimeWindow.make (Time.millisToPosix 0) (Duration.hours 24)) sampleSchedule
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
        [ layout [] <| viewSheet model.sheet
        ]
    }


viewSheet : Sheet -> Element Msg
viewSheet sheet =
    row
        [ width fill
        , height fill

        {- Element.explain Debug.todo -}
        ]
    <|
        List.map (viewColumn sheet) sheet.columns


stickyHeader : String -> Element Msg
stickyHeader title =
    row
        ([ width fill
         , Background.color <| rgba 0.8 0.8 0.8 0.8
         , padding 3
         ]
            ++ sticky
        )
        [ el [ centerX ] <| text title
        ]


viewColumn : Sheet -> Column -> Element Msg
viewColumn sheet col =
    case col of
        TimeColumn { slots } ->
            viewTimeColumn sheet slots

        ResourceColumn { resource, subcolumns } ->
            viewResourceColumn sheet resource subcolumns


viewTimeColumn : Sheet -> List TimeWindow -> Element Msg
viewTimeColumn sheet slots =
    let
        topPadding =
            toFloat sheet.theme.defaultCell.heightPx / 2 |> round

        slotRows =
            row [ width fill, height <| px topPadding ]
                []
                :: (slots
                        |> List.drop 1
                        |> List.map (viewSlotRow sheet)
                   )
    in
    column [ width fill, height fill, inFront <| stickyHeader "Czas" ] <|
        stickyHeader "Czas"
            :: slotRows


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewSlotRow : Sheet -> TimeWindow -> Element Msg
viewSlotRow sheet window =
    row [ width fill, height (cellHeight sheet window) ] <|
        [ el [ alignRight, centerY ] <| text <| TimeWindow.formatStart window ]


viewResourceColumn : Sheet -> Resource -> List SubColumn -> Element Msg
viewResourceColumn sheet resource subcolumns =
    column [ width fill, height fill, inFront <| viewHeaderRow sheet resource ] <|
        viewHeaderRow sheet resource
            :: [ viewLayout sheet subcolumns ]


viewHeaderRow : Sheet -> Resource -> Element Msg
viewHeaderRow sheet resource =
    stickyHeader <| Schedule.getResourceName resource


viewLayout : Sheet -> List SubColumn -> Element Msg
viewLayout sheet subcolumns =
    row [ width fill, height fill, paddingXY 1 0 ] <|
        viewSubColumns sheet subcolumns


viewSubColumns : Sheet -> List SubColumn -> List (Element Msg)
viewSubColumns sheet subcols =
    List.map (viewSubColumn sheet) subcols


viewSubColumn : Sheet -> SubColumn -> Element Msg
viewSubColumn sheet subcol =
    column [ width fill, height fill ] <|
        List.map (viewCellRow sheet) subcol


viewCellRow : Sheet -> Cell -> Element Msg
viewCellRow sheet cell =
    let
        attrs =
            case cell of
                CellAvailable _ ->
                    []

                CellReserved _ ->
                    [ Background.color (rgb 0.5 0.8 0.8) ]
    in
    row [ paddingXY 0 1, width fill, height (cellHeight sheet (Sheet.cellWindow cell)) ]
        [ el ([ width fill, height fill, Font.size 12 ] ++ attrs) <| text (cellLabel cell) ]


cellLabel : Cell -> String
cellLabel cell =
    let
        w =
            Sheet.cellWindow cell
    in
    -- (TimeWindow.getStart w |> formatTime) ++ " - " ++ (TimeWindow.getEnd w |> formatTime)
    ""


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


cellHeight : Sheet -> TimeWindow -> Length
cellHeight sheet window =
    window
        |> TimeWindow.getDuration
        |> Duration.inSeconds
        |> (*) sheet.pixelsPerSecond
        |> round
        |> px


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
