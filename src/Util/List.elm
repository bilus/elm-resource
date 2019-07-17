module Util.List exposing (slice, window2)

import List exposing (filter, head, map, tail)
import List.Extra exposing (zip)
import Maybe.Extra exposing (values)



{-
   Similar to zip for an arbitrary number of lists, except it works for jagged lists.

   Example:

       slice [[1, 2, 3], [4], [], [5, 6]] => [[1, 4, 5], [2, 6], [3]]
-}


slice : List (List a) -> List (List a)
slice xss =
    let
        heads =
            xss
                |> map head
                |> values

        rests =
            xss
                |> map tail
                |> values
                |> filter ((/=) [])
    in
    case rests of
        [] ->
            [ heads ]

        _ ->
            heads :: slice rests



{-
   Generate pairs using with a sliding window.

   Example:

   window2 [1, 2, 3, 4, 5] =>
-}


window2 : List a -> List ( a, a )
window2 xs =
    zip xs (tail xs |> Maybe.withDefault [])
