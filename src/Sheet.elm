module Sheet exposing (Cell(..), CellRef, CellState, Column(..), ColumnRef, Draggable(..), Droppable(..), Msg(..), Sheet, SubColumn, cellWindow, dragDropConfig, make, makeCellRef, makeColumnRef, update)

import DragDrop
import Duration exposing (Duration)
import List.Extra
import Monocle.Lens exposing (Lens)
import Schedule exposing (Reservation, Resource, Schedule)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow)
import Util.List exposing (slice, window2)
import Util.Selectable as Selectable exposing (Selectable)


type alias Sheet =
    { window : TimeWindow
    , columns : List Column
    , dragDropState : DragDrop.State Draggable Droppable
    , slotCount : Int
    }


type Column
    = ResourceColumn
        { resource : Resource
        , subcolumns : List SubColumn
        }
    | TimeColumn
        { slots : List TimeWindow
        }


type alias SubColumn =
    Selectable CellState Cell


selected =
    Lens .selected <| \x m -> { m | selected = x }


type alias CellState =
    { selected : Bool }


type Cell
    = EmptyCell TimeWindow
    | ReservedCell Reservation


type Draggable
    = CellStart Cell CellRef
    | CellEnd Cell CellRef


type Droppable
    = DroppableCell Cell CellRef


dragDropConfig =
    { started = MoveStarted
    , dragged = MoveTargetChanged
    , dropped = MoveCompleted
    , canceled = MoveCanceled
    }


type Msg
    = CellClicked Cell CellRef
    | MoveStarted Draggable
    | MoveTargetChanged Draggable Droppable
    | MoveCompleted Draggable Droppable
    | MoveCanceled Draggable


type ColumnRef
    = ColumnRef Int


makeColumnRef : Int -> ColumnRef
makeColumnRef =
    ColumnRef


type CellRef
    = CellRef Int Int Int


makeCellRef : ColumnRef -> Int -> Int -> CellRef
makeCellRef (ColumnRef colIndex) subColIndex cellIndex =
    CellRef colIndex subColIndex cellIndex


make : Int -> TimeWindow -> List Schedule -> Sheet
make slotCount window schedules =
    { window = window
    , columns = makeColumns slotCount window schedules
    , dragDropState = DragDrop.init
    , slotCount = slotCount
    }


recalc : Sheet -> Sheet
recalc sheet =
    let
        resourceColumns =
            sheet
                |> getSchedules
                |> List.map (makeResourceColumn sheet.window)
    in
    { sheet
        | columns = makeColumns sheet.slotCount sheet.window (getSchedules sheet)
    }


makeColumns : Int -> TimeWindow -> List Schedule -> List Column
makeColumns slotCount window schedules =
    let
        slots =
            TimeWindow.split slotCount window

        resourceColumns =
            schedules
                |> List.map (makeResourceColumn window)
    in
    makeTimeColumn slots :: resourceColumns


getSchedules : Sheet -> List Schedule
getSchedules { columns } =
    let
        getSchedule column =
            case column of
                TimeColumn _ ->
                    []

                ResourceColumn { resource, subcolumns } ->
                    subcolumns
                        |> List.concatMap
                            (List.concatMap
                                (\cell ->
                                    case cell of
                                        EmptyCell _ ->
                                            []

                                        ReservedCell reservation ->
                                            [ reservation ]
                                )
                                << Selectable.toList
                            )
                        |> Schedule.newSchedule resource
                        |> List.singleton
    in
    columns
        |> List.concatMap getSchedule


update : Msg -> Sheet -> ( Sheet, Cmd Msg )
update msg sheet =
    case msg of
        CellClicked cell cellRef ->
            ( sheet |> onCellClicked cell cellRef, Cmd.none )

        MoveStarted draggable ->
            ( sheet |> onMoveStarted draggable, Cmd.none )

        MoveTargetChanged draggable droppable ->
            ( sheet |> onMoveTargetChanged draggable droppable |> recalc, Cmd.none )

        _ ->
            ( sheet, Cmd.none )


onCellClicked : Cell -> CellRef -> Sheet -> Sheet
onCellClicked cell (CellRef colIndex subColIndex cellIndex) sheet =
    { sheet
        | columns =
            sheet.columns
                |> List.Extra.updateAt colIndex
                    (\column ->
                        case column of
                            ResourceColumn col ->
                                ResourceColumn
                                    { col
                                        | subcolumns =
                                            col.subcolumns
                                                |> List.Extra.updateAt subColIndex
                                                    (Selectable.choose cell selected)
                                    }

                            TimeColumn col ->
                                TimeColumn col
                    )
    }


onMoveStarted : Draggable -> Sheet -> Sheet
onMoveStarted draggable sheet =
    { sheet
        | dragDropState = sheet.dragDropState |> DragDrop.start draggable
    }


onMoveTargetChanged : Draggable -> Droppable -> Sheet -> Sheet
onMoveTargetChanged draggable droppable sheet =
    let
        updatedSheet =
            case ( draggable, droppable ) of
                ( CellStart _ cellRef, DroppableCell dropTarget _ ) ->
                    sheet
                        |> updateCell cellRef
                            (\cell ->
                                case cell of
                                    ReservedCell reservation ->
                                        reservation
                                            |> Debug.log "before"
                                            |> Schedule.moveReservation (dropTarget |> cellWindow |> TimeWindow.getStart)
                                            |> Debug.log "after"
                                            |> ReservedCell

                                    EmptyCell window ->
                                        EmptyCell window
                            )

                _ ->
                    sheet
    in
    { updatedSheet
        | dragDropState = sheet.dragDropState |> DragDrop.drag (Debug.log "move target changed to" droppable)
    }


updateCell : CellRef -> (Cell -> Cell) -> Sheet -> Sheet
updateCell (CellRef colIndex subColIndex cellIndex) f sheet =
    { sheet
        | columns =
            sheet.columns
                |> List.Extra.updateAt colIndex
                    (\column ->
                        case column of
                            ResourceColumn col ->
                                ResourceColumn
                                    { col
                                        | subcolumns =
                                            col.subcolumns
                                                |> List.Extra.updateAt subColIndex
                                                    (Selectable.updateAt cellIndex f)
                                    }

                            TimeColumn col ->
                                TimeColumn col
                    )
    }


makeTimeColumn : List TimeWindow -> Column
makeTimeColumn slots =
    TimeColumn { slots = slots }


makeResourceColumn : TimeWindow -> Schedule -> Column
makeResourceColumn window schedule =
    ResourceColumn
        { resource = Schedule.getResource schedule
        , subcolumns = makeSubcolumns window <| Schedule.getReservations schedule
        }


makeSubcolumns : TimeWindow -> List Reservation -> List SubColumn
makeSubcolumns window reservations =
    reservations
        |> List.Extra.gatherWith Schedule.isConflict
        |> List.map (\( x, xs ) -> x :: xs)
        |> slice
        -- Make each sub-column continuous by filling gaps with empty cells
        |> List.map
            (Selectable.fromList { selected = False } << fillInGaps window << List.map ReservedCell << Schedule.sortReservations)


makeCell : Posix -> Duration -> Cell
makeCell start duration =
    EmptyCell (TimeWindow.make start duration)


fillInGaps : TimeWindow -> List Cell -> List Cell
fillInGaps window cells =
    let
        startPlaceholder =
            makeCell (TimeWindow.getStart window) (Duration.seconds 0)

        endPlaceholder =
            makeCell (TimeWindow.getEnd window) (Duration.seconds 0)

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
