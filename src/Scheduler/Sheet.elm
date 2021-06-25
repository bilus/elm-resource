module Scheduler.Sheet exposing
    ( CellRef(..)
    , Column
    , ColumnRef
    , Draggable(..)
    , Droppable(..)
    , Layer
    , Msg(..)
    , Sheet
    , columnByIndex
    , columnByResourceId
    , findCell
    , findReservedCell
    , layerByReservationId
    , make
    , makeCellRef
    , makeColumnRef
    , setNowMarker
    , subscribe
    , update
    )

import Browser.Events
import Duration exposing (Duration)
import Json.Decode as Json
import List.Extra
import Scheduler.Cell as Cell exposing (..)
import Scheduler.Schedule as Schedule exposing (Reservation, ReservationId, Resource, ResourceId, Schedule, getResourceId)
import Scheduler.TimeWindow as TimeWindow exposing (TimeWindow)
import Time exposing (Posix)
import Util.DragDrop as DragDrop
import Util.List exposing (slice, window2)
import Util.Maybe exposing (isJust)


type alias Sheet =
    { window : TimeWindow
    , columns : List Column
    , dragDropState : DragDrop.State Draggable Droppable
    , selectedCell : Maybe CellRef
    , nowMarker : Maybe Posix
    , readonly : Bool
    }


type alias Column =
    { resource : Resource
    , layers : List Layer
    }


type alias Layer =
    List Cell


type Draggable
    = CellStart CellRef
    | CellEnd CellRef


findReservedCell : Sheet -> ReservationId -> Maybe CellRef
findReservedCell sheet reservationId =
    sheet.columns
        |> findCellRef
            (\cell -> Cell.reservationId cell == Just reservationId)


columnByResourceId : Sheet -> ResourceId -> Maybe Column
columnByResourceId sheet resourceId =
    sheet.columns
        |> List.filter (\{ resource } -> Schedule.getResourceId resource == resourceId)
        |> List.head


layerByReservationId : Sheet -> ReservationId -> Maybe Layer
layerByReservationId sheet reservationId =
    sheet.columns
        |> List.filterMap
            (\{ layers } ->
                layers
                    |> List.filter
                        (\layer ->
                            layer
                                |> List.any
                                    (\cell -> Cell.reservationId cell == Just reservationId)
                        )
                    |> List.head
            )
        |> List.head


mapDraggable : (CellRef -> CellRef) -> Draggable -> Draggable
mapDraggable f draggable =
    case draggable of
        CellStart ref ->
            CellStart (f ref)

        CellEnd ref ->
            CellEnd (f ref)


type Droppable
    = DroppableWindow TimeWindow


type Msg
    = CellClicked CellRef
    | DragDropStarting Draggable
    | DragDropStarted
    | DragDropTargetChanged (Maybe Droppable)
    | DragDropStopped
    | DragDropCompleted Droppable
    | ClickedOutsideCells


type ColumnRef
    = ColumnRef Int


makeColumnRef : Int -> ColumnRef
makeColumnRef =
    ColumnRef


type CellRef
    = CellRef Int Int Int


makeCellRef : ColumnRef -> Int -> Int -> CellRef
makeCellRef (ColumnRef colIndex) layerIndex cellIndex =
    CellRef colIndex layerIndex cellIndex


make : TimeWindow -> List Schedule -> Bool -> Sheet
make window schedules readonly =
    { window = window
    , columns = makeColumns window schedules
    , dragDropState = DragDrop.init
    , selectedCell = Nothing
    , nowMarker = Nothing
    , readonly = readonly
    }


setNowMarker : Maybe Posix -> Sheet -> Sheet
setNowMarker marker sheet =
    { sheet | nowMarker = marker }


recalc : Sheet -> Sheet
recalc sheet =
    let
        newColumns =
            makeColumns sheet.window (getSchedules sheet)

        remap : CellRef -> CellRef
        remap =
            remappedReservedCellRef sheet.columns newColumns
    in
    { sheet
        | columns = newColumns
        , selectedCell =
            sheet.selectedCell
                |> Maybe.map remap
        , dragDropState =
            sheet.dragDropState
                |> DragDrop.mapDragged (mapDraggable remap)
    }


remappedReservedCellRef : List Column -> List Column -> CellRef -> CellRef
remappedReservedCellRef oldColumns newColumns oldRef =
    let
        reservationId =
            cellRefToReservationId oldColumns oldRef

        newRef =
            reservationId
                |> Maybe.andThen (reservationIdToCellRef newColumns)
                |> Maybe.withDefault oldRef
    in
    newRef


cellRefToReservationId : List Column -> CellRef -> Maybe Schedule.ReservationId
cellRefToReservationId columns cellRef =
    findByRef cellRef columns
        |> Maybe.andThen
            reservation
        |> Maybe.map Schedule.getReservationId


reservationIdToCellRef : List Column -> Schedule.ReservationId -> Maybe CellRef
reservationIdToCellRef columns reservationId =
    columns
        |> findCellRef
            (\cell ->
                reservation cell
                    |> Maybe.map Schedule.getReservationId
                    |> Maybe.map ((==) reservationId)
                    |> Maybe.withDefault False
            )


findCell : Sheet -> CellRef -> Maybe Cell
findCell sheet cellRef =
    findByRef cellRef sheet.columns


columnByIndex : Sheet -> Int -> Maybe Column
columnByIndex sheet colIndex =
    sheet.columns
        |> List.Extra.getAt colIndex


findByRef : CellRef -> List Column -> Maybe Cell
findByRef (CellRef colIndex layerIndex cellIndex) columns =
    columns
        |> List.Extra.getAt colIndex
        |> Maybe.map .layers
        |> Maybe.andThen (List.Extra.getAt layerIndex)
        |> Maybe.andThen (List.Extra.getAt cellIndex)


findCellRef : (Cell -> Bool) -> List Column -> Maybe CellRef
findCellRef pred =
    let
        findCellRefColumn : Int -> Column -> Maybe CellRef
        findCellRefColumn colIndex { layers } =
            layers
                |> List.Extra.indexedFoldr
                    (\layerIndex cells match ->
                        if isJust match then
                            match

                        else
                            cells
                                |> findCellRefCells colIndex layerIndex
                    )
                    Nothing

        findCellRefCells : Int -> Int -> List Cell -> Maybe CellRef
        findCellRefCells colIndex layerIndex =
            Maybe.map (CellRef colIndex layerIndex)
                << List.Extra.indexedFoldr
                    (\cellIndex cell match ->
                        if isJust match then
                            match

                        else if pred cell then
                            Just cellIndex

                        else
                            Nothing
                    )
                    Nothing
    in
    List.Extra.indexedFoldr
        (\colIndex column match ->
            if isJust match then
                match

            else
                column
                    |> findCellRefColumn colIndex
        )
        Nothing


makeColumns : TimeWindow -> List Schedule -> List Column
makeColumns window schedules =
    schedules
        |> List.map (makeColumn window)


getSchedules : Sheet -> List Schedule
getSchedules { columns } =
    let
        getSchedule { resource, layers } =
            layers
                |> List.concatMap
                    (List.concatMap
                        (\cell ->
                            case cell of
                                EmptyCell _ ->
                                    []

                                ReservedCell reservation ->
                                    [ reservation ]
                        )
                    )
                |> Schedule.newSchedule resource
                |> List.singleton
    in
    columns
        |> List.concatMap getSchedule


subscribe : Sheet -> Sub Msg
subscribe sheet =
    if DragDrop.isDragging sheet.dragDropState then
        Browser.Events.onMouseUp <|
            Json.succeed DragDropStopped

    else if not (DragDrop.isIdle sheet.dragDropState) then
        Browser.Events.onMouseMove <|
            Json.succeed DragDropStarted

    else
        Sub.none


update : Msg -> Sheet -> ( Sheet, Cmd Msg )
update msg sheet =
    case ( sheet.readonly, DragDrop.isIdle sheet.dragDropState, msg ) of
        ( _, True, CellClicked cellRef ) ->
            ( sheet |> onCellClicked cellRef, Cmd.none )

        ( _, True, ClickedOutsideCells ) ->
            ( sheet |> onClickedOutsideCells, Cmd.none )

        ( False, True, DragDropStarting draggable ) ->
            ( { sheet
                | dragDropState = sheet.dragDropState |> DragDrop.starting draggable { x = 0, y = 0 }
              }
            , Cmd.none
            )

        ( False, False, DragDropStarted ) ->
            ( { sheet
                | dragDropState = sheet.dragDropState |> DragDrop.started
              }
            , Cmd.none
            )

        ( False, False, DragDropTargetChanged droppable ) ->
            ( sheet |> onDragDropTargetChanged droppable |> recalc, Cmd.none )

        ( False, False, DragDropCompleted droppable ) ->
            ( sheet |> onDragDropCompleted droppable, Cmd.none )

        ( False, False, DragDropStopped ) ->
            ( { sheet | dragDropState = sheet.dragDropState |> DragDrop.stopped }, Cmd.none )

        ( _, _, _ ) ->
            ( sheet, Cmd.none )


onCellClicked : CellRef -> Sheet -> Sheet
onCellClicked cellRef sheet =
    let
        newSelection =
            if sheet.selectedCell == Just cellRef then
                Nothing

            else
                Just cellRef
    in
    { sheet | selectedCell = newSelection }


onClickedOutsideCells : Sheet -> Sheet
onClickedOutsideCells sheet =
    { sheet | selectedCell = Nothing }


onDragDropTargetChanged : Maybe Droppable -> Sheet -> Sheet
onDragDropTargetChanged droppable sheet =
    let
        updatedSheet =
            case ( DragDrop.getDragItem sheet.dragDropState, droppable ) of
                ( Just (CellStart cellRef), Just (DroppableWindow targetWindow) ) ->
                    sheet
                        |> updateCell cellRef
                            (\cell ->
                                case cell of
                                    ReservedCell reservation ->
                                        reservation
                                            |> Schedule.moveReservationStart (targetWindow |> TimeWindow.getStart)
                                            |> ReservedCell

                                    EmptyCell window ->
                                        EmptyCell window
                            )

                ( Just (CellEnd cellRef), Just (DroppableWindow targetWindow) ) ->
                    sheet
                        |> updateCell cellRef
                            (\cell ->
                                case cell of
                                    ReservedCell reservation ->
                                        reservation
                                            |> Schedule.moveReservationEnd (targetWindow |> TimeWindow.getEnd)
                                            |> ReservedCell

                                    EmptyCell window ->
                                        EmptyCell window
                            )

                _ ->
                    sheet
    in
    { updatedSheet
        | dragDropState = sheet.dragDropState |> DragDrop.dragged droppable
    }


onDragDropCompleted : Droppable -> Sheet -> Sheet
onDragDropCompleted _ sheet =
    { sheet | dragDropState = sheet.dragDropState |> DragDrop.stopped }


updateCell : CellRef -> (Cell -> Cell) -> Sheet -> Sheet
updateCell (CellRef colIndex layerIndex cellIndex) f sheet =
    { sheet
        | columns =
            sheet.columns
                |> List.Extra.updateAt colIndex
                    (\column ->
                        { column
                            | layers =
                                column.layers
                                    |> List.Extra.updateAt layerIndex
                                        (List.Extra.updateAt cellIndex f)
                        }
                    )
    }


makeColumn : TimeWindow -> Schedule -> Column
makeColumn window schedule =
    { resource = Schedule.getResource schedule
    , layers = makeLayers window <| Schedule.getReservations schedule
    }


makeLayers : TimeWindow -> List Reservation -> List Layer
makeLayers window reservations =
    reservations
        |> List.Extra.gatherWith Schedule.isConflict
        |> List.map (\( x, xs ) -> x :: xs)
        |> slice
        -- Ignore reservations outside the current window
        |> List.map (List.filter (TimeWindow.overlaps window << Schedule.getWindow))
        -- Make each layer continuous by filling gaps with empty cells
        |> List.map (fillInGaps window << List.map ReservedCell << Schedule.sortReservations)
