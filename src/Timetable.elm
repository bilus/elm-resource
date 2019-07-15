module Timetable exposing (Timeslot(..), Timetable, getResourceNames, mapReservedTimeslot, mapTimeslotsOverTime, newFromSchedules)

import Duration exposing (Duration)
import List.Extra
import Schedule exposing (Reservation, Resource, Schedule)
import TimeWindow exposing (TimeWindow)


type Timeslot
    = Available { window : TimeWindow }
    | Reserved { window : TimeWindow, reservation : Reservation }
    | Overbooked { window : TimeWindow, conflicts : List Reservation }


mapReservedTimeslot : (TimeWindow -> Reservation -> a) -> Timeslot -> Maybe a
mapReservedTimeslot f timeslot =
    case timeslot of
        Reserved { window, reservation } ->
            Just (f window reservation)

        Overbooked _ ->
            Nothing

        Available _ ->
            Nothing


type Timetable
    = Timetable
        { timeslots : List ( Resource, List Timeslot )
        , window : TimeWindow
        }


newFromSchedules : Int -> TimeWindow -> List Schedule -> Timetable
newFromSchedules timeslotCount window schedules =
    Timetable
        { timeslots = List.map (buildTimeslots timeslotCount window) schedules
        , window = window
        }


buildTimeslots : Int -> TimeWindow -> Schedule -> ( Resource, List Timeslot )
buildTimeslots count window schedule =
    let
        reservations =
            Schedule.getReservations schedule

        timeslots =
            TimeWindow.split count window
                |> List.map (buildTimeslot reservations)
    in
    ( Schedule.getResource schedule, timeslots )


buildTimeslot : List Reservation -> TimeWindow -> Timeslot
buildTimeslot reservations window =
    let
        matches =
            reservations
                |> List.filter (Schedule.isReservationOverlapping window)
    in
    case matches of
        [] ->
            Available { window = window }

        [ reservation ] ->
            Reserved { window = window, reservation = reservation }

        conflicts ->
            Overbooked { window = window, conflicts = conflicts }


getResourceNames : Timetable -> List String
getResourceNames (Timetable { timeslots }) =
    timeslots
        |> List.map (Schedule.getResourceName << Tuple.first)


getTimeslotsOverTime : Timetable -> List (List Timeslot)
getTimeslotsOverTime (Timetable { timeslots }) =
    timeslots
        |> List.map Tuple.second
        |> List.Extra.transpose


mapTimeslotsOverTime : (Duration -> List Timeslot -> a) -> Timetable -> List a
mapTimeslotsOverTime f ((Timetable { timeslots }) as timetable) =
    timetable
        |> getTimeslotsOverTime
        |> List.Extra.zip (dayTimeOffsets window)
        |> List.map
            (\( offset, slots ) ->
                f offset slots
            )


dayTimeOffsets : TimeWindow -> List Duratio
dayTimeOffsets _ =
    -- TODO: Generate them properly
    let
        offsets =
            List.range 0 47
                |> List.map ((*) 30 >> toFloat)
    in
    offsets
        |> List.map Duration.minutes
