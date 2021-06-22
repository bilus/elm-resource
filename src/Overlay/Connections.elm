module Overlay.Connections exposing (render)

import Connection exposing (..)
import Element exposing (Element, fill, height, width)
import Sheet
import Theme exposing (Theme)
import Util.Svg as Svg exposing (Svg)


render : Theme -> List (Connection d) -> Element msg
render theme connections =
    let
        lines =
            connections
                |> List.map (renderConnection theme)

        svg =
            Svg.svg [ width fill, height fill ]
                lines
    in
    svg


renderConnection : Theme -> Connection d -> Svg msg
renderConnection theme connection =
    let
        from =
            Theme.xy theme connection.fromLayer connection.fromTime

        to =
            Theme.xy theme connection.toLayer connection.toTime
    in
    Svg.line from to
