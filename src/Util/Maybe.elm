module Util.Maybe exposing (..)

import Maybe.Extra


isJust : Maybe a -> Bool
isJust m =
    case m of
        Just _ ->
            True

        Nothing ->
            False


compact : List (Maybe a) -> List a
compact =
    Maybe.Extra.values
