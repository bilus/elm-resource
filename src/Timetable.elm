module Timetable exposing (Allocation(..), Timetable, getResourceNames, mapRows, newFromSchedules)

import Dict exposing (Dict)
import Duration exposing (Duration)
import List.Extra
import Schedule exposing (Reservation, Resource, Schedule)
import TimeWindow exposing (TimeWindow)


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
        }


newFromSchedules : Int -> TimeWindow -> List Schedule -> Timetable
newFromSchedules timeslotCount window schedules =
    Timetable
        { rows =
            window
                |> TimeWindow.split timeslotCount
                |> List.map (buildRow schedules)
        , resources = List.map Schedule.getResource schedules
        , window = window
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


mapRows : (TimeWindow -> List Allocation -> a) -> Timetable -> List a
mapRows f ((Timetable { rows }) as timetable) =
    rows
        |> List.map
            (\(TimeRow window allocations) -> f window allocations)
