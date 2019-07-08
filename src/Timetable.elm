module Timetable exposing (Timeslot, Timetable, getResourceNames, mapReservedTimeslot, mapTimeslotsOverTime, newFromSchedules)

import Duration exposing (Duration)
import List.Extra
import Primitives exposing (TimeWindow)
import Schedule exposing (Reservation, Resource, Schedule)


type Timeslot
    = EmptyTimeslot { offset : Duration }
    | ReservedTimeslot { offset : Duration, reservations : List Reservation }


mapReservedTimeslot : (Duration -> List Reservation -> a) -> Timeslot -> Maybe a
mapReservedTimeslot f timeslot =
    case timeslot of
        ReservedTimeslot { offset, reservations } ->
            Just (f offset reservations)

        EmptyTimeslot _ ->
            Nothing


type Timetable
    = Timetable
        { timeslots : List ( Resource, List Timeslot )
        , window : TimeWindow
        }


newFromSchedules : TimeWindow -> List Schedule -> Timetable
newFromSchedules window schedules =
    Timetable
        { timeslots = buildTimeslots window schedules
        , window = window
        }


buildTimeslots : TimeWindow -> List Schedule -> List ( Resource, List Timeslot )
buildTimeslots window schedules =
    []


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
mapTimeslotsOverTime f ((Timetable { window }) as timetable) =
    timetable
        |> getTimeslotsOverTime
        |> List.Extra.zip (dayTimeOffsets window)
        |> List.map
            (\( offset, slots ) ->
                f offset slots
            )


dayTimeOffsets : TimeWindow -> List Duration
dayTimeOffsets _ =
    -- TODO: Generate them properly
    let
        offsets =
            List.range 0 47
                |> List.map ((*) 30 >> toFloat)
    in
    offsets
        |> List.map Duration.minutes
