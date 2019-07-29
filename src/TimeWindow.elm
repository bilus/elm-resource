module TimeWindow exposing (TimeWindow, compare, formatStart, gap, getDuration, getEnd, getStart, isEmpty, make, moveEnd, moveStart, overlaps, split)

import Duration exposing (Duration, seconds)
import Time exposing (Posix)


type TimeWindow
    = TimeWindow
        { start : Posix
        , duration : Duration
        }


make : Posix -> Duration -> TimeWindow
make start duration =
    TimeWindow { start = start, duration = duration }


split : Int -> TimeWindow -> List TimeWindow
split count (TimeWindow { start, duration }) =
    let
        interval =
            Duration.inMilliseconds duration / toFloat count
    in
    List.range 0 (count - 1)
        |> List.map
            (toFloat
                >> (*) interval
                >> offsetTime start
                >> (\s -> TimeWindow { start = s, duration = Duration.milliseconds interval })
            )


getStart : TimeWindow -> Posix
getStart (TimeWindow { start }) =
    start


getEnd : TimeWindow -> Posix
getEnd (TimeWindow { start, duration }) =
    offsetTime start (Duration.inMilliseconds duration)


getDuration : TimeWindow -> Duration
getDuration (TimeWindow { duration }) =
    duration



-- TODO: Obsolete.


formatStart : TimeWindow -> String
formatStart (TimeWindow { start }) =
    let
        zone =
            Time.utc

        -- TODO: Make zone configurable or use CEST
        hours =
            String.fromInt (Time.toHour zone start)
                |> String.padLeft 2 '0'

        minutes =
            String.fromInt (Time.toMinute zone start)
                |> String.padLeft 2 '0'
    in
    hours ++ ":" ++ minutes


offsetTime : Posix -> Float -> Posix
offsetTime t ms =
    t
        |> Time.posixToMillis
        |> toFloat
        |> (+) ms
        |> round
        |> Time.millisToPosix


overlaps : TimeWindow -> TimeWindow -> Bool
overlaps (TimeWindow w1) (TimeWindow w2) =
    let
        start1 =
            Time.posixToMillis w1.start

        end1 =
            start1 + (w1.duration |> Duration.inMilliseconds |> round)

        start2 =
            Time.posixToMillis w2.start

        end2 =
            start2 + (w2.duration |> Duration.inMilliseconds |> round)
    in
    end1 > start2 && end2 > start1


compare : TimeWindow -> TimeWindow -> Order
compare (TimeWindow w1) (TimeWindow w2) =
    let
        start1 =
            w1.start |> Time.posixToMillis

        start2 =
            w2.start |> Time.posixToMillis
    in
    Basics.compare start1 start2


gap : TimeWindow -> TimeWindow -> Maybe TimeWindow
gap w1 w2 =
    let
        ( wa, wb ) =
            case compare w1 w2 of
                GT ->
                    ( w2, w1 )

                _ ->
                    ( w1, w2 )
    in
    if overlaps wa wb then
        Nothing

    else
        let
            e =
                getEnd wa

            s =
                getStart wb

            d =
                Duration.from e s
        in
        Just <| make e d


isEmpty : TimeWindow -> Bool
isEmpty (TimeWindow { duration }) =
    duration == seconds 0


moveStart : Posix -> TimeWindow -> TimeWindow
moveStart newStart ((TimeWindow { start, duration }) as window) =
    let
        startMs =
            Time.posixToMillis start

        newStartMs =
            Time.posixToMillis newStart

        delta =
            startMs
                - newStartMs
                |> toFloat

        newDuration =
            duration
                |> Duration.inMilliseconds
                |> (+) delta
    in
    if newDuration <= 0 then
        window

    else
        TimeWindow
            { start = newStart
            , duration = Duration.milliseconds newDuration
            }


moveEnd : Posix -> TimeWindow -> TimeWindow
moveEnd newEnd ((TimeWindow { start, duration }) as window) =
    let
        startMs =
            Time.posixToMillis start

        newEndMs =
            Time.posixToMillis newEnd

        newDurationMs =
            newEndMs - startMs
    in
    if newDurationMs <= 0 then
        window

    else
        TimeWindow
            { start = start
            , duration = newDurationMs |> toFloat |> Duration.milliseconds
            }
