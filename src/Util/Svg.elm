module Util.Svg exposing
    ( arrowHeadMarker
    , marker
    , markerEndUrl
    , polygon
    , polyline
    , svg
    )

import Element exposing (Element)
import Html.Attributes as HA
import TypedSvg as Svg
import TypedSvg.Attributes as TA exposing (noFill, points, stroke)
import TypedSvg.Attributes.InPx exposing (x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg)
import TypedSvg.Types exposing (Paint(..), px)


svg : List (Element.Attribute msg) -> List (Svg msg) -> Element msg
svg attrs elements =
    let
        svgEl =
            Svg.svg [ HA.style "width" "100%", HA.style "height" "100%" ]
                elements
                |> Element.html
    in
    Element.el attrs svgEl


polygon : List ( Float, Float ) -> List (Attribute msg) -> Svg msg
polygon pts attrs =
    Svg.polygon (attrs ++ [ points pts ]) []


polyline : List ( Float, Float ) -> List (Attribute msg) -> Svg msg
polyline pts attrs =
    Svg.polyline (attrs ++ [ noFill, points pts ]) []


markerEndUrl : String -> Attribute msg
markerEndUrl id =
    TA.markerEnd <| "url(#" ++ id ++ ")"


arrowHeadMarker : String -> Svg msg
arrowHeadMarker id =
    let
        pts =
            [ ( 0.0, 0.0 )
            , ( 9.0, 3.0 )
            , ( 0.0, 6.0 )
            ]
    in
    marker id ( 9.0, 3.0 ) 8.0 6.0 [ polygon pts [] ]


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
