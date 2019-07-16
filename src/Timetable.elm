module Timetable exposing (Allocation(..), ColumnStyle, Timetable, getResourceNames, mapRows, newFromSchedules)

import Array
import Color
import Dict exposing (Dict)
import Duration exposing (Duration)
import Element exposing (..)
import List.Extra
import Schedule exposing (Reservation, Resource, Schedule)
import TimeWindow exposing (TimeWindow)
import Util.Flip exposing (flip)


type TimeRow
    = TimeRow TimeWindow (List Allocation)


type Allocation
    = Available
    | Reserved Reservation
    | Overbooked (List Reservation)


type Timetable
    = Timetable
        { rows : List TimeRow
        , resources : List Resource
        , window : TimeWindow
        , columnStyles : List ColumnStyle
        }


type alias ColumnStyle =
    { reservedColor : Color
    , overbookedColor : Color
    }


newFromSchedules : Int -> TimeWindow -> List Schedule -> Timetable
newFromSchedules timeslotCount window schedules =
    let
        resources =
            List.map Schedule.getResource schedules
    in
    Timetable
        { rows =
            window
                |> TimeWindow.split timeslotCount
                |> List.map (buildRow schedules)
        , resources = resources
        , window = window
        , columnStyles = generateColumnStyles (List.length resources)
        }


buildRow : List Schedule -> TimeWindow -> TimeRow
buildRow schedules window =
    TimeRow window (buildAllocations window schedules)


buildAllocations : TimeWindow -> List Schedule -> List Allocation
buildAllocations window schedules =
    schedules
        |> List.map (buildAllocation window)


buildAllocation : TimeWindow -> Schedule -> Allocation
buildAllocation window schedule =
    let
        matches =
            schedule
                |> Schedule.getReservations
                |> List.filter (Schedule.isReservationOverlapping window)
    in
    case matches of
        [] ->
            Available

        [ reservation ] ->
            Reserved reservation

        conflicts ->
            Overbooked conflicts


getResourceNames : Timetable -> List String
getResourceNames (Timetable { resources }) =
    resources
        |> List.map Schedule.getResourceName


mapRows : (TimeWindow -> List ( Allocation, ColumnStyle ) -> a) -> Timetable -> List a
mapRows f ((Timetable { rows, columnStyles }) as timetable) =
    rows
        |> List.map
            (\(TimeRow window allocations) ->
                List.Extra.zip allocations columnStyles
                    |> f window
            )


generateColumnStyles : Int -> List ColumnStyle
generateColumnStyles num =
    let
        palette =
            Array.fromList
                [ Color.orange
                , Color.yellow
                , Color.green
                , Color.blue
                , Color.purple
                , Color.brown
                , Color.lightOrange
                , Color.lightYellow
                , Color.lightGreen
                , Color.lightBlue
                , Color.lightPurple
                , Color.lightBrown
                , Color.darkOrange
                , Color.darkYellow
                , Color.darkGreen
                , Color.darkBlue
                , Color.darkPurple
                , Color.darkBrown
                ]
    in
    List.range 0 (num - 1)
        |> List.map
            (\i ->
                i
                    |> modBy (Array.length palette)
                    |> flip Array.get palette
                    |> Maybe.withDefault Color.red
                    -- This means an internal error
                    |> (\color -> { reservedColor = toColor color, overbookedColor = toColor Color.red })
            )


toColor : Color.Color -> Color
toColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha
