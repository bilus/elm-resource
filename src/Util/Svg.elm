module Util.Svg exposing (line, svg)

import Color
import Element exposing (Element)
import Html.Attributes as HA
import TypedSvg as Svg
import TypedSvg.Attributes exposing (stroke)
import TypedSvg.Attributes.InPx exposing (x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))


svg : Int -> Int -> List (Svg msg) -> Element msg
svg width height elements =
    Svg.svg [ HA.width width, HA.height height ]
        elements
        |> Element.html


line : ( Float, Float ) -> ( Float, Float ) -> Svg msg
line ( xa, ya ) ( xb, yb ) =
    Svg.line [ x1 xa, y1 ya, x2 xb, y2 yb, stroke <| Paint Color.black ] []
