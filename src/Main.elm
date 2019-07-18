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
    let
        window =
            TimeWindow.make (Time.millisToPosix 0) (Duration.hours 24)
    in
    ( { currPage = InputPage
      , sheet = Sheet.make (Theme.defaultTheme 48 window) 48 window sampleSchedule
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
    { title = "elm-resource"
    , body =
        [ layout [] <| viewSheet model.sheet
        ]
    }


viewSheet : Sheet -> Element Msg
viewSheet sheet =
    Theme.sheetFrame sheet.theme <|
        List.map (viewColumn sheet) sheet.columns


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
        slotRows =
            slots
                |> List.drop 1
                |> List.map (Theme.timeCell sheet.theme)
    in
    Theme.timeColumn sheet.theme "Czas" slotRows


viewResourceColumn : Sheet -> Resource -> List SubColumn -> Element Msg
viewResourceColumn sheet resource subcolumns =
    let
        title =
            Schedule.getResourceName resource
    in
    Theme.resourceColumn sheet.theme
        title
        (subcolumns
            |> List.map (List.map (viewCell sheet))
        )


viewCell : Sheet -> Cell -> Element Msg
viewCell sheet cell =
    let
        labelEl =
            text (cellLabel cell)
    in
    case cell of
        EmptyCell _ ->
            Theme.emptyCell sheet.theme (Sheet.cellWindow cell) <| labelEl

        ReservedCell _ ->
            Theme.reservedCell sheet.theme (Sheet.cellWindow cell) <| labelEl


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


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
