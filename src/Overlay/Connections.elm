module Overlay.Connections exposing (render)

import Color
import Connection exposing (Connection)
import Element exposing (Element, fill, height, width)
import Sheet exposing (Sheet)
import Theme exposing (Theme)
import TypedSvg as Svg
import Util.Svg as SvgU


render : Sheet -> Theme -> List (Connection d) -> Element msg
render sheet theme connections =
    let
        regularArrow =
            "overlay-connections-arrow"

        style =
            { dist = 15
            , strongStroke = Color.black
            , strongArrowMarker = regularArrow
            , weakStroke = Color.black
            , weakArrowMarker = regularArrow
            , blockedStroke = Color.darkRed
            , blockedArrowMarker = regularArrow
            , blockedCrossSize = 3
            }

        defs =
            Svg.defs [] [ SvgU.arrowHeadMarker regularArrow ]

        lines =
            connections
                |> List.concatMap (Connection.render sheet theme style)

        svg =
            SvgU.svg [ width fill, height fill ]
                (defs :: lines)
    in
    svg
