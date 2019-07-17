module TimetableTest exposing (suite)

import Duration exposing (Duration, seconds)
import Expect exposing (Expectation)
import Schedule exposing (ReservationId(..), ResourceId(..), Schedule, newReservation, newResource, newSchedule)
import Test exposing (..)
import Time exposing (Posix)
import TimeWindow


offsetTime : Posix -> Duration -> Posix
offsetTime t d =
    t
        |> Time.posixToMillis
        |> toFloat
        |> (+) (Duration.inMilliseconds d)
        |> round
        |> Time.millisToPosix


suite : Test
suite =
    Debug.todo "implement"



-- describe "The Timetable module"
--     [ let
--         t0 =
--             Time.millisToPosix 0
--         t1 =
--             offsetTime t0 (seconds 2)
--         room1 =
--             newResource (ResourceId "room1") "Room #1"
--         r1 =
--             ReservationId "r1"
--         r2 =
--             ReservationId "r2"
--       in
--       describe "Timetable" <|
--         [ describe "mapTimeslotsOverTime" <|
--             let
--                 window =
--                     TimeWindow.make t0 (seconds 4)
--                 interval =
--                     seconds 1
--                 emptyTimetable =
--                     Timetable.newFromSchedules window interval []
--             in
--             [ test "single schedule" <|
--                 \_ ->
--                     let
--                         timetable =
--                             Timetable.newFromSchedules window
--                                 interval
--                                 [ newSchedule room1 [ newReservation r1 t0 (seconds 1) ] ]
--                     in
--                     Expect.equal ([ 0 ] |> List.map seconds)
--                         (Timetable.mapTimeslotsOverTime (\offset _ -> offset) timetable)
--             , test "single schedule spanning multiple timeslots" <|
--                 \_ ->
--                     let
--                         timetable =
--                             Timetable.newFromSchedules window
--                                 interval
--                                 [ newSchedule room1 [ newReservation r1 t0 (seconds 3) ] ]
--                     in
--                     Expect.equal ([ 0, 1, 2 ] |> List.map seconds)
--                         (Timetable.mapTimeslotsOverTime (\offset _ -> offset) timetable)
--             ]
--         ]
--     ]
