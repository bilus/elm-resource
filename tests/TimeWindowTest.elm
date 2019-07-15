module TimeWindowTest exposing (suite)

import Duration exposing (Duration, seconds)
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
          in
          describe "split" <|
            [ test "zero slot count" <|
                \_ ->
                    Expect.equal [] <| split 0 (make (t 0) (seconds 90))
            , test "one big slot" <|
                \_ ->
                    Expect.equal [ make (t 0) (seconds 90) ] <| split 1 (make (t 0) (seconds 90))
            , test "30 second intervals" <|
                \_ ->
                    Expect.equal
                        [ make (t 0) (seconds 30)
                        , make (t 30) (seconds 30)
                        , make (t 60) (seconds 30)
                        ]
                    <|
                        split 3 (make (t 0) (seconds 90))
            ]
        ]
