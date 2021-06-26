module Scheduler.Cell exposing (..)

import Duration exposing (Duration)
import Scheduler.Schedule as Schedule exposing (Reservation(..))
import Scheduler.TimeWindow as TimeWindow exposing (TimeWindow)
import Time exposing (Posix)
import Util.List


type Cell
    = EmptyCell TimeWindow
    | ReservedCell Reservation


reservation : Cell -> Maybe Schedule.Reservation
reservation cell =
    case cell of
        ReservedCell r ->
            Just r

        EmptyCell _ ->
            Nothing


reservationId : Cell -> Maybe Schedule.ReservationId
reservationId cell =
    reservation cell
        |> Maybe.map Schedule.getReservationId


makeEmpty : Posix -> Duration -> Cell
makeEmpty start duration =
    EmptyCell (TimeWindow.make start duration)


fillInGaps : TimeWindow -> List Cell -> List Cell
fillInGaps sheetWindow cells =
    let
        startPlaceholder =
            makeEmpty (TimeWindow.getStart sheetWindow) (Duration.seconds 0)

        endPlaceholder =
            makeEmpty (TimeWindow.getEnd sheetWindow) (Duration.seconds 0)

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
        |> Util.List.window2
        |> List.concatMap fix
        |> List.filter (not << TimeWindow.isEmpty << window)


window : Cell -> TimeWindow
window cell =
    case cell of
        EmptyCell w ->
            w

        ReservedCell r ->
            Schedule.getWindow r


gapFiller : Cell -> Cell -> Maybe Cell
gapFiller c1 c2 =
    let
        w1 =
            window c1

        w2 =
            window c2
    in
    TimeWindow.gap w1 w2
        |> Maybe.map EmptyCell


getPaletteIndex : Cell -> Maybe Int
getPaletteIndex cell =
    case cell of
        ReservedCell (Reservation { paletteIndex }) ->
            paletteIndex

        EmptyCell _ ->
            Nothing
