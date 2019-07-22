module Main exposing (main)

import Array exposing (Array)
import Browser
import Color
import DragDrop
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
import Sheet exposing (Cell(..), CellRef, CellState, Column(..), ColumnRef, Draggable(..), Droppable(..), Sheet, SubColumn)
import Theme exposing (Theme)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)
import Util.Selectable as Selectable


type Page
    = InputPage


type Msg
    = SheetMsg Sheet.Msg
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

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "elm-resource"
    , body =
        [ (layout [] <| viewSheet model.sheet model.theme)
            |> Html.map SheetMsg
        ]
    }


viewSheet : Sheet -> Theme -> Element Sheet.Msg
viewSheet sheet theme =
    Theme.sheetFrame
        theme
        (sheet.columns
            |> List.indexedMap
                (\i column ->
                    viewColumn sheet theme (Sheet.makeColumnRef i) column
                )
        )


viewColumn : Sheet -> Theme -> ColumnRef -> Column -> Element Sheet.Msg
viewColumn sheet theme colRef col =
    case col of
        TimeColumn { slots } ->
            viewTimeColumn theme slots

        ResourceColumn { resource, subcolumns } ->
            viewResourceColumn sheet theme colRef resource subcolumns


viewTimeColumn : Theme -> List TimeWindow -> Element Sheet.Msg
viewTimeColumn theme slots =
    let
        slotRows =
            slots
                |> List.drop 1
                |> List.map (Theme.timeCell theme)
    in
    Theme.timeColumn theme "Czas" slotRows


viewResourceColumn : Sheet -> Theme -> ColumnRef -> Resource -> List SubColumn -> Element Sheet.Msg
viewResourceColumn sheet theme colRef resource subcolumns =
    let
        title =
            Schedule.getResourceName resource
    in
    Theme.resourceColumn theme
        title
        (subcolumns
            |> List.indexedMap
                (\subColIndex subcolumn ->
                    subcolumn
                        |> Selectable.indexedMapWithState
                            (\cellIndex cell ->
                                viewCell sheet theme (Sheet.makeCellRef colRef subColIndex cellIndex) cell
                            )
                        |> Selectable.toList
                )
        )


viewCell : Sheet -> Theme -> CellRef -> Cell -> CellState -> Element Sheet.Msg
viewCell sheet theme cellRef cell state =
    let
        labelEl =
            text <| cellLabel cell

        --++ " " ++ Debug.toString cellRef ++ " " ++ Debug.toString state
        onClick =
            Sheet.CellClicked cell cellRef
    in
    case cell of
        EmptyCell _ ->
            Theme.emptyCell theme (Sheet.cellWindow cell) state labelEl onClick

        ReservedCell _ ->
            Theme.reservedCell theme (Sheet.cellWindow cell) state labelEl onClick



-- |> (if state.selected then
--         DragDrop.makeDraggable
--             sheet.dragDropState
--             Sheet.dragDropConfig
--             (DraggableCell cellRef)
--     else
--         identity
--    )


cellLabel : Cell -> String
cellLabel cell =
    let
        w =
            Sheet.cellWindow cell
    in
    ""



-- (TimeWindow.getStart w |> formatTime) ++ " - " ++ (TimeWindow.getEnd w |> formatTime)


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
