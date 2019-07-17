module Sheet exposing (Cell(..), Column(..), Sheet, SubColumn, cellWindow, make)

import Duration exposing (Duration)
import List.Extra
import Schedule exposing (Reservation, Resource, Schedule)
import Theme exposing (Theme)
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow)
import Util.List exposing (slice, window2)


type alias Sheet =
    { window : TimeWindow
    , pixelsPerSecond : Float
    , columns : List Column
    , theme : Theme
    }


type alias SubColumn =
    List Cell


type Cell
    = CellAvailable TimeWindow
    | CellReserved Reservation


type Column
    = ResourceColumn
        { resource : Resource
        , subcolumns : List SubColumn
        }
    | TimeColumn
        { slots : List TimeWindow
        }


make : Theme -> Int -> TimeWindow -> List Schedule -> Sheet
make theme slotCount window schedules =
    let
        slotHeight =
            theme.defaultCell.heightPx

        slots =
            TimeWindow.split slotCount window

        duration =
            List.head slots
                |> Maybe.map (Duration.inSeconds << TimeWindow.getDuration)
                |> Maybe.withDefault 0

        pixelsPerSecond =
            toFloat slotHeight / duration

        resourceColumns =
            schedules
                |> List.map (makeResourceColumn window)
    in
    { window = window
    , pixelsPerSecond = pixelsPerSecond
    , columns = makeTimeColumn slots :: resourceColumns
    , theme = theme
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
        -- Columns with most reservations on the left
        |> List.Extra.stableSortWith cmpLength
        |> List.reverse
        -- Make each sub-column continuous by filling gaps with empty cells
        |> List.map
            (fillInGaps window << List.map CellReserved << Schedule.sortReservations)


makeCell : Posix -> Duration -> Cell
makeCell start duration =
    CellAvailable (TimeWindow.make start duration)


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
        CellAvailable window ->
            window

        CellReserved reservation ->
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
        |> Maybe.map CellAvailable


cmpLength : List a -> List b -> Order
cmpLength xs ys =
    compare (List.length xs) (List.length ys)
