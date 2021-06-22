module Util.Svg exposing (Svg, line, svg)

import Color
import Element exposing (Element)
import Html.Attributes as HA
import TypedSvg as Svg
import TypedSvg.Attributes exposing (stroke)
import TypedSvg.Attributes.InPx exposing (x1, x2, y1, y2)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..))


type alias Svg msg =
    TypedSvg.Core.Svg msg


svg : List (Element.Attribute msg) -> List (Svg msg) -> Element msg
svg attrs elements =
    let
        svgEl =
            Svg.svg [ HA.style "width" "100%", HA.style "height" "100%" ]
                elements
                |> Element.html
    in
    Element.el attrs svgEl


line : ( Float, Float ) -> ( Float, Float ) -> Svg msg
line ( xa, ya ) ( xb, yb ) =
    Svg.line [ x1 xa, y1 ya, x2 xb, y2 yb, stroke <| Paint Color.black ] []
