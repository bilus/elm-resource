module Primitives exposing (TimeWindow, newTimeWindow)

import Duration exposing (Duration)
import Time exposing (Posix)


type TimeWindow
    = TimeWindow
        { start : Posix
        , duration : Duration
        }


newTimeWindow : Posix -> Duration -> TimeWindow
newTimeWindow start duration =
    TimeWindow { start = start, duration = duration }
