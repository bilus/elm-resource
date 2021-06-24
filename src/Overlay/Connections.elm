module Overlay.Connections exposing (render)

import Connection exposing (Connection)
import Element exposing (Element, fill, height, width)
import Sheet exposing (Sheet)
import Theme exposing (Theme)
import TypedSvg as Svg
import Util.Svg as SvgU


render : Sheet -> Theme -> List (Connection d) -> Element msg
render sheet theme connections =
    let
        arrowMarkerId =
            "overlay-connections-arrow"

        defs =
            Svg.defs [] [ SvgU.arrowHeadMarker arrowMarkerId ]

        lines =
            connections
                |> List.concatMap (Connection.render sheet theme arrowMarkerId)

        svg =
            SvgU.svg [ width fill, height fill ]
                (defs :: lines)
    in
    svg
