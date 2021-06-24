module Util.Svg exposing
    ( Svg
    , arrowHeadMarker
    , defs
    , line
    , marker
    , polygon
    , polyline
    , polylineWithMarkerEnd
    , svg
    )

import Color
import Element exposing (Element)
import Html.Attributes as HA
import Html.Events as Events
import TypedSvg as Svg
import TypedSvg.Attributes as TA exposing (noFill, points, stroke)
import TypedSvg.Attributes.InPx exposing (x1, x2, y1, y2)
import TypedSvg.Core
import TypedSvg.Types exposing (Paint(..), px)


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
    Svg.line [ x1 xa, y1 ya, x2 xb, y2 yb ] []


polygon : List ( Float, Float ) -> Svg msg
polygon pts =
    Svg.polygon [ points pts ] []


polyline : List ( Float, Float ) -> Svg msg
polyline pts =
    Svg.polyline [ noFill, points pts ] []


polylineWithMarkerEnd : String -> List ( Float, Float ) -> Svg msg
polylineWithMarkerEnd markerId pts =
    Svg.polyline
        [ TA.markerEnd <| "url(#" ++ markerId ++ ")"
        , noFill
        , stroke <| Paint Color.black
        , points pts
        ]
        []


arrowHeadMarker : String -> Svg msg
arrowHeadMarker id =
    let
        pts =
            [ ( 0.0, 0.0 )
            , ( 9.0, 3.0 )
            , ( 0.0, 6.0 )
            ]
    in
    marker id ( 9.0, 3.0 ) 8.0 6.0 [ polygon pts ]


marker : String -> ( Float, Float ) -> Float -> Float -> List (Svg msg) -> Svg msg
marker id ( refX, refY ) width height svgs =
    Svg.marker
        [ TA.id id
        , TA.refX <| String.fromFloat refX
        , TA.refY <| String.fromFloat refY
        , TA.markerWidth <| px width
        , TA.markerHeight <| px height
        , TA.orient "auto"
        ]
        svgs


defs : List (Svg msg) -> Svg msg
defs =
    Svg.defs []
