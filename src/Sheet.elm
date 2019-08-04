module Sheet exposing (Cell(..), CellRef, Column, ColumnRef, Draggable(..), Droppable(..), Layer, Msg(..), Sheet, cellWindow, make, makeCellRef, makeColumnRef, subscribe, update)

import Browser.Events
import DragDrop
import Duration exposing (Duration)
import Json.Decode as Json
import List.Extra
import Schedule exposing (Reservation, Resource, Schedule)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow)
import Util.List exposing (slice, window2)
import Util.Maybe exposing (isJust)


type alias Sheet =
    { window : TimeWindow
    , columns : List Column
    , dragDropState : DragDrop.State Draggable Droppable
    , selectedCell : Maybe CellRef
    }


type alias Column =
    { resource : Resource
    , layers : List Layer
    }


type alias Layer =
    List Cell


type Cell
    = EmptyCell TimeWindow
    | ReservedCell Reservation


type Draggable
    = CellStart CellRef
    | CellEnd CellRef


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
    = CellClicked Cell CellRef
    | DragDropStarting Draggable
    | DragDropStarted
    | DragDropTargetChanged (Maybe Droppable)
    | DragDropStopped
    | DragDropCompleted Droppable
    | Noop


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


make : TimeWindow -> List Schedule -> Sheet
make window schedules =
    { window = window
    , columns = makeColumns window schedules
    , dragDropState = DragDrop.init
    , selectedCell = Nothing
    }


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
            getReservation
        |> Maybe.map Schedule.getReservationId


reservationIdToCellRef : List Column -> Schedule.ReservationId -> Maybe CellRef
reservationIdToCellRef columns reservationId =
    columns
        |> findCellRef
            (\cell ->
                getReservation cell
                    |> Maybe.map Schedule.getReservationId
                    |> Maybe.map ((==) reservationId)
                    |> Maybe.withDefault False
            )


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


getReservation : Cell -> Maybe Schedule.Reservation
getReservation cell =
    case cell of
        ReservedCell reservation ->
            Just reservation

        EmptyCell _ ->
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
    case ( DragDrop.isIdle sheet.dragDropState, msg ) of
        ( True, CellClicked cell cellRef ) ->
            ( sheet |> onCellClicked cell cellRef, Cmd.none )

        ( True, DragDropStarting draggable ) ->
            ( { sheet
                | dragDropState = sheet.dragDropState |> DragDrop.starting draggable { x = 0, y = 0 } |> Debug.log "onDragDropStarted"
              }
            , Cmd.none
            )

        ( False, DragDropStarted ) ->
            ( { sheet
                | dragDropState = sheet.dragDropState |> DragDrop.started |> Debug.log "onDragDropStarted"
              }
            , Cmd.none
            )

        ( False, DragDropTargetChanged droppable ) ->
            ( sheet |> onDragDropTargetChanged droppable |> recalc, Cmd.none )

        ( False, DragDropCompleted droppable ) ->
            ( sheet |> onDragDropCompleted droppable, Cmd.none )

        ( False, DragDropStopped ) ->
            ( { sheet | dragDropState = sheet.dragDropState |> DragDrop.stopped |> Debug.log "onDragDropStopped" }, Cmd.none )

        ( _, Noop ) ->
            ( sheet, Cmd.none )

        ( _, _ ) ->
            ( sheet, Cmd.none )


onCellClicked : Cell -> CellRef -> Sheet -> Sheet
onCellClicked cell cellRef sheet =
    let
        _ =
            Debug.log "onCellClicked" sheet.dragDropState

        newSelection =
            if sheet.selectedCell == Just cellRef then
                Nothing

            else
                Just cellRef
    in
    { sheet | selectedCell = newSelection |> Debug.log "onCellClicked" }


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
        | dragDropState = sheet.dragDropState |> DragDrop.dragged droppable |> Debug.log "onDragDropTargetChanged"
    }


onDragDropCompleted : Droppable -> Sheet -> Sheet
onDragDropCompleted droppable sheet =
    { sheet | dragDropState = sheet.dragDropState |> DragDrop.stopped |> Debug.log "onDragDropCompleted" }


offsetBy d t =
    let
        ms =
            Duration.inMilliseconds d
    in
    t
        |> Time.posixToMillis
        |> toFloat
        |> (+) ms
        |> round
        |> Time.millisToPosix


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


makeEmptyCell : Posix -> Duration -> Cell
makeEmptyCell start duration =
    EmptyCell (TimeWindow.make start duration)


fillInGaps : TimeWindow -> List Cell -> List Cell
fillInGaps sheetWindow cells =
    let
        startPlaceholder =
            makeEmptyCell (TimeWindow.getStart sheetWindow) (Duration.seconds 0)

        endPlaceholder =
            makeEmptyCell (TimeWindow.getEnd sheetWindow) (Duration.seconds 0)

        cells2 =
            startPlaceholder :: cells ++ [ endPlaceholder ]

        fix ( c1, c2 ) =
            case gapFiller c1 c2 of
                Just filler ->
                    [ filler, c2 ]

                Nothing ->
                    [ c2 ]
    in
    cells2
        |> window2
        |> List.concatMap fix
        |> List.filter (not << TimeWindow.isEmpty << cellWindow)


cellWindow : Cell -> TimeWindow
cellWindow cell =
    case cell of
        EmptyCell window ->
            window

        ReservedCell reservation ->
            Schedule.getWindow reservation


gapFiller : Cell -> Cell -> Maybe Cell
gapFiller c1 c2 =
    let
        w1 =
            cellWindow c1

        w2 =
            cellWindow c2
    in
    TimeWindow.gap w1 w2
        |> Maybe.map EmptyCell
