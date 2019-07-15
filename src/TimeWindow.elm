module TimeWindow exposing (TimeWindow, formatStart, getStart, make, overlaps, split)

import Duration exposing (Duration)
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


formatStart : TimeWindow -> String
formatStart (TimeWindow { start }) =
    let
        zone =
            Time.utc

        -- TODO: Make zone configurable or use CEST
    in
    String.fromInt (Time.toHour zone start)
        ++ ":"
        ++ String.fromInt (Time.toMinute zone start)


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
