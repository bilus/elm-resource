module Overlay.Connections exposing (render)

import Browser.Events exposing (onClick)
import Connection exposing (..)
import Element exposing (Element, fill, height, width)
import Sheet exposing (Sheet)
import Theme exposing (Theme)
import Util.List
import Util.Svg as Svg exposing (Svg)


render : Sheet -> Theme -> msg -> List (Connection d) -> Element msg
render sheet theme onClick connections =
    let
        lines =
            connections
                |> List.concatMap (renderConnection sheet theme onClick)

        defs =
            Svg.defs [ Svg.arrowHeadMarker "arrow" ]

        svg =
            Svg.svg [ width fill, height fill ]
                (defs :: lines)
    in
    svg


renderConnection : Sheet -> Theme -> msg -> Connection d -> List (Svg msg)
renderConnection sheet theme onClick connection =
    let
        from =
            Theme.xy sheet theme connection.fromCell connection.fromTime

        to =
            Theme.xy sheet theme connection.toCell connection.toTime
    in
    Maybe.map2
        (\( xf, yf ) ( xt, yt ) ->
            let
                sign num =
                    if num < 0 then
                        -1

                    else
                        1

                d =
                    (sign <| xt - xf)
                        * Basics.min 15 (abs <| xt - xf)

                points =
                    [ ( xf, yf )
                    , ( xf + d, yf )
                    , ( xt - d, yt )
                    , ( xt, yt )
                    ]
            in
            [ Svg.polylineWithMarkerEnd "arrow" onClick points ]
        )
        from
        to
        |> Maybe.withDefault []
