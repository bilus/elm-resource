module TimeWindowTest exposing (suite)

import Duration exposing (Duration, days, seconds)
import Expect exposing (Expectation)
import Test exposing (..)
import Time exposing (Posix)
import TimeWindow exposing (make, split)


suite : Test
suite =
    describe "The TimeWindow module"
        [ let
            t s =
                Time.millisToPosix (s * 1000)

            td days =
                Time.millisToPosix (days * 1000 * 60 * 60 * 24)
          in
          describe "split" <|
            [ test "one big slot" <|
                \_ ->
                    Expect.equal [ make (t 0) (seconds 90) ] <| split (seconds 90) (make (t 0) (seconds 90))
            , test "30 second intervals" <|
                \_ ->
                    Expect.equal
                        [ make (t 0) (seconds 30)
                        , make (t 30) (seconds 30)
                        , make (t 60) (seconds 30)
                        ]
                    <|
                        split (seconds 30) (make (t 0) (seconds 90))
            , test "1 week intervals" <|
                \_ ->
                    Expect.equal
                        [ make (td 0) (days 7)
                        , make (td 7) (days 7)
                        , make (td 14) (days 7)
                        , make (td 21) (days 7)
                        ]
                    <|
                        split (days 7) (make (td 0) (days 30))
            ]
        ]
