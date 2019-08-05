module Util.Time exposing (formatDateTime, formatTime)

import Time exposing (Posix, Zone)
import Time.Format exposing (format)
import Time.Format.Config.Config_en_us as US



-- TODO: Better formatting using locale.


formatTime : Zone -> Posix -> String
formatTime zone t =
    format US.config "%I:%M" zone t
        ++ p zone t


formatDateTime : Zone -> Posix -> String
formatDateTime zone t =
    format US.config "%a, %b %-@d ⋅ %I:%M" zone t
        ++ p zone t


p : Zone -> Posix -> String
p zone =
    String.toLower << format US.config "%p" zone
