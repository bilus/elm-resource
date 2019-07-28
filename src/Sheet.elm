module Sheet exposing (Cell(..), CellRef, CellState, Column(..), ColumnRef, Draggable(..), Droppable(..), Msg(..), Sheet, SubColumn, cellWindow, make, makeCellRef, makeColumnRef, subscribe, update)

import Browser.Events
import DragDrop
import Duration exposing (Duration)
import Json.Decode as Json
import List.Extra
import Monocle.Lens exposing (Lens)
import Schedule exposing (Reservation, Resource, Schedule)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow)
import Util.List exposing (slice, window2)


type alias Sheet =
    { window : TimeWindow
    , columns : List Column
    , dragDropState : DragDrop.State Draggable Droppable
    , slotCount : Int
    , selectedCell : Maybe CellRef
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
    List Cell


selected =
    Lens .selected <| \x m -> { m | selected = x }


type alias CellState =
    { selected : Bool }


type Cell
    = EmptyCell TimeWindow
    | ReservedCell Reservation


type Draggable
    = CellStart CellRef
    | CellEnd CellRef


type Droppable
    = DroppableCell Cell CellRef --  Duration


type Msg
    = CellClicked Cell CellRef
    | MouseUp Int Int
    | MoveStarted Draggable
    | MoveTargetChanged Draggable (Maybe Droppable)
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
    , selectedCell = Nothing
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
            Json.map2 MouseUp
                (Json.field "pageX" Json.int)
                (Json.field "pageY" Json.int)

    else
        Sub.none


update : Msg -> Sheet -> ( Sheet, Cmd Msg )
update msg sheet =
    case msg of
        CellClicked cell cellRef ->
            ( sheet |> onCellClicked cell cellRef, Cmd.none )

        MoveStarted draggable ->
            ( sheet |> onMoveStarted draggable, Cmd.none )

        MoveTargetChanged draggable droppable ->
            ( sheet |> onMoveTargetChanged draggable droppable |> recalc, Cmd.none )

        MoveCompleted draggable droppable ->
            ( sheet |> onMoveCompleted draggable droppable, Cmd.none )

        MoveCanceled draggable ->
            if DragDrop.isDragging sheet.dragDropState then
                ( sheet |> onMoveCanceled, Cmd.none )

            else
                ( sheet, Cmd.none )

        MouseUp _ _ ->
            ( sheet |> onMoveCanceled, Cmd.none )


onCellClicked : Cell -> CellRef -> Sheet -> Sheet
onCellClicked cell cellRef sheet =
    let
        newSelection =
            if sheet.selectedCell == Just cellRef then
                Nothing

            else
                Just cellRef
    in
    { sheet | selectedCell = newSelection }


onMoveStarted : Draggable -> Sheet -> Sheet
onMoveStarted draggable sheet =
    { sheet
        | dragDropState = sheet.dragDropState |> DragDrop.start draggable |> Debug.log "onMoveStarted"
    }


onMoveTargetChanged : Draggable -> Maybe Droppable -> Sheet -> Sheet
onMoveTargetChanged draggable droppable sheet =
    -- let
    --     relativeOffset =
    --         Duration.seconds 0
    --     updatedSheet =
    --         case ( draggable, droppable ) of
    --             ( CellStart _ cellRef, Just (DroppableCell dropTarget _) ) ->
    --                 sheet
    --                     |> updateCell cellRef
    --                         (\cell ->
    --                             case cell of
    --                                 ReservedCell reservation ->
    --                                     reservation
    --                                         |> Schedule.moveReservation (dropTarget |> cellWindow |> TimeWindow.getStart |> offsetBy relativeOffset)
    --                                         |> ReservedCell
    --                                 EmptyCell window ->
    --                                     EmptyCell window
    --                         )
    --             _ ->
    --                 sheet
    -- in
    { sheet
        | dragDropState = sheet.dragDropState |> DragDrop.drag droppable |> Debug.log "onMoveTargetChanged"
    }


onMoveCompleted : Draggable -> Droppable -> Sheet -> Sheet
onMoveCompleted draggable droppable sheet =
    { sheet | dragDropState = sheet.dragDropState |> DragDrop.stop |> Debug.log "onMoveCompleted" }


onMoveCanceled : Sheet -> Sheet
onMoveCanceled sheet =
    { sheet | dragDropState = sheet.dragDropState |> DragDrop.stop |> Debug.log "onMoveCanceled" }


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
                                                    (List.Extra.updateAt cellIndex f)
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
            (fillInGaps window << List.map ReservedCell << Schedule.sortReservations)


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
