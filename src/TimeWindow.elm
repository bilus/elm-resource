module TimeWindow exposing (TimeWindow, compare, contains, gap, getDuration, getEnd, getStart, goBack, goForward, goToDay, includes, intersection, isEmpty, make, makeDay, makeMonth, makeWeek, moveEnd, moveStart, overlaps, setDuration, split, splitN, substract, toDay, toMonth, toWeek)

import Duration exposing (Duration, seconds)
import Time exposing (Posix, Zone)
import Time.Extra exposing (Interval(..))


type TimeWindow
    = TimeWindow
        { start : Posix
        , duration : Duration
        }


make : Posix -> Duration -> TimeWindow
make start duration =
    TimeWindow { start = start, duration = duration }


makeDay : Zone -> Posix -> TimeWindow
makeDay zone time =
    make time (seconds 1)
        |> toDay zone


makeWeek : Zone -> Posix -> TimeWindow
makeWeek zone time =
    make time (seconds 1)
        |> toWeek zone


makeMonth : Zone -> Posix -> TimeWindow
makeMonth zone time =
    make time (seconds 1)
        |> toMonth zone


splitN : Int -> TimeWindow -> List TimeWindow
splitN count (TimeWindow { start, duration }) =
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


split : Duration -> TimeWindow -> List TimeWindow
split slotDuration (TimeWindow { start, duration }) =
    let
        interval =
            Duration.inMilliseconds slotDuration

        count =
            Duration.inSeconds duration
                / Duration.inSeconds slotDuration
                |> floor
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


setDuration : Duration -> TimeWindow -> TimeWindow
setDuration newDuration (TimeWindow { start }) =
    TimeWindow { start = start, duration = newDuration }


offsetTime : Posix -> Float -> Posix
offsetTime t ms =
    t
        |> Time.posixToMillis
        |> toFloat
        |> (+) ms
        |> round
        |> max 0
        |> Time.millisToPosix


overlaps : TimeWindow -> TimeWindow -> Bool
overlaps w1 w2 =
    let
        m1 =
            toMillis w1

        m2 =
            toMillis w2
    in
    m1.end > m2.start && m2.end > m1.start



{- w1 contains w2 -}


contains : TimeWindow -> TimeWindow -> Bool
contains w1 w2 =
    let
        m1 =
            toMillis w1

        m2 =
            toMillis w2
    in
    m2.start >= m1.start && m2.end <= m1.end


includes : Posix -> TimeWindow -> Bool
includes t w =
    let
        m =
            toMillis w

        ms =
            t |> Time.posixToMillis
    in
    ms >= m.start && ms < m.end


intersection : TimeWindow -> TimeWindow -> Maybe TimeWindow
intersection w1 w2 =
    if overlaps w1 w2 then
        let
            m1 =
                toMillis w1

            m2 =
                toMillis w2

            start =
                max m1.start m2.start

            end =
                min m1.end m2.end

            duration =
                (end - start)
                    |> toFloat
                    |> Duration.milliseconds
        in
        Just (TimeWindow { start = start |> Time.millisToPosix, duration = duration })

    else
        Nothing


substract : TimeWindow -> TimeWindow -> ( Maybe TimeWindow, Maybe TimeWindow )
substract w1 w2 =
    if overlaps w1 w2 then
        let
            m1 =
                toMillis w1

            m2 =
                toMillis w2

            ( start1, start2 ) =
                sortTuple ( m1.start, m2.start )

            ( end1, end2 ) =
                sortTuple ( m1.end, m2.end )

            duration1 =
                start2 - start1

            duration2 =
                end2 - end1
        in
        ( if duration1 > 0 then
            Just <| make (start1 |> Time.millisToPosix) (duration1 |> toFloat |> Duration.milliseconds)

          else
            Nothing
        , if duration2 > 0 then
            Just <| make (start2 |> Time.millisToPosix) (duration2 |> toFloat |> Duration.milliseconds)

          else
            Nothing
        )

    else
        ( Nothing, Nothing )


{-| -}
sortTuple : ( comparable, comparable ) -> ( comparable, comparable )
sortTuple ( a, b ) =
    if a > b then
        ( b, a )

    else
        ( a, b )


toMillis : TimeWindow -> { start : Int, end : Int }
toMillis (TimeWindow w) =
    let
        start =
            Time.posixToMillis w.start

        end =
            start + (w.duration |> Duration.inMilliseconds |> round)
    in
    { start = start, end = end }


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
moveEnd newEnd ((TimeWindow { start }) as window) =
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


goBack : Duration -> TimeWindow -> TimeWindow
goBack offset (TimeWindow { start, duration }) =
    let
        offsetMs =
            offset |> Duration.inMilliseconds
    in
    TimeWindow { start = offsetTime start -offsetMs, duration = duration }


goForward : Duration -> TimeWindow -> TimeWindow
goForward offset (TimeWindow { start, duration }) =
    let
        offsetMs =
            offset |> Duration.inMilliseconds
    in
    TimeWindow { start = offsetTime start offsetMs, duration = duration }


goToDay : Zone -> Posix -> TimeWindow -> TimeWindow
goToDay zone t ((TimeWindow { duration }) as w) =
    let
        start =
            t
                |> Time.Extra.floor Day zone

        interval =
            guessInterval zone w

        newWindow =
            TimeWindow { start = start, duration = duration }
    in
    interval
        |> Maybe.map (\i -> newWindow |> toInterval i zone)
        |> Maybe.withDefault newWindow


guessInterval : Zone -> TimeWindow -> Maybe Interval
guessInterval zone w =
    let
        supportedIntervals =
            [ Day, Week, Month ]
    in
    supportedIntervals
        |> List.filter (\i -> w == (w |> toInterval i zone))
        |> List.head


toDay : Zone -> TimeWindow -> TimeWindow
toDay =
    toInterval Day


toWeek : Zone -> TimeWindow -> TimeWindow
toWeek =
    toInterval Week


toMonth : Zone -> TimeWindow -> TimeWindow
toMonth =
    toInterval Month


toInterval : Interval -> Zone -> TimeWindow -> TimeWindow
toInterval interval zone (TimeWindow { start }) =
    let
        newStart =
            start
                |> Time.Extra.floor interval zone

        end =
            start
                |> Time.Extra.add Millisecond 1 zone
                |> Time.Extra.ceiling interval zone

        newDuration =
            Time.Extra.diff Millisecond zone newStart end
                |> toFloat
                |> Duration.milliseconds
    in
    TimeWindow { start = newStart, duration = newDuration }
