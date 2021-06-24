module Connection exposing (Connection, Kind(..), render)

-- import Sheet exposing (CellRef, Sheet, cellWindow)

import Color exposing (Color)
import List.Extra
import Pixels exposing (pixels)
import Sheet exposing (Sheet)
import Theme exposing (Theme)
import Time exposing (Posix)
import TypedSvg.Attributes as TA
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))
import Util.Svg as SvgU


type alias Connection d =
    { fromCell : Sheet.CellRef
    , fromTime : Posix
    , toCell : Sheet.CellRef
    , toTime : Posix
    , data : d
    , notes : String
    , kind : Kind
    }


type Kind
    = Strong
    | Weak
    | Blocked


type alias Style =
    { dist : Float
    , strongStroke : Color
    , strongArrowMarker : String
    , weakStroke : Color
    , weakArrowMarker : String
    , blockedStroke : Color
    , blockedArrowMarker : String
    , blockedCrossSize : Float
    }


render : Sheet -> Theme -> Style -> Connection d -> List (Svg msg)
render sheet theme style connection =
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
                        * Basics.min style.dist (abs <| xt - xf)

                points =
                    [ ( xf, yf )
                    , ( xf + d, yf )
                    , ( xt - d, yt )
                    , ( xt, yt )
                    ]
            in
            case connection.kind of
                Strong ->
                    renderStrong style points

                Weak ->
                    renderWeak style points

                Blocked ->
                    renderBlocked style points
        )
        from
        to
        |> Maybe.withDefault []


renderStrong : Style -> List ( Float, Float ) -> List (Svg msg)
renderStrong style points =
    [ SvgU.polyline points
        [ TA.stroke <| Paint style.strongStroke
        , SvgU.markerEndUrl style.strongArrowMarker
        ]
    ]


renderWeak : Style -> List ( Float, Float ) -> List (Svg msg)
renderWeak style points =
    [ SvgU.polyline points
        [ TA.stroke <| Paint style.weakStroke
        , TA.strokeDasharray "2,1"
        , SvgU.markerEndUrl style.weakArrowMarker
        ]
    ]


renderBlocked : Style -> List ( Float, Float ) -> List (Svg msg)
renderBlocked style points =
    let
        center =
            Maybe.map2
                (\( xs, ys ) ( xe, ye ) ->
                    let
                        x =
                            if xs > xe then
                                xe + (xs - xe) / 2

                            else
                                xs + (xe - xs) / 2

                        y =
                            if ys > ye then
                                ye + (ys - ye) / 2

                            else
                                ys + (ye - ys) / 2
                    in
                    ( x, y )
                )
                (List.head points)
                (List.Extra.last points)

        size =
            style.blockedCrossSize

        cross =
            center
                |> Maybe.map (drawCross style)
                |> Maybe.withDefault []
    in
    cross ++ [ SvgU.polyline points [ TA.stroke <| Paint style.blockedStroke, SvgU.markerEndUrl style.blockedArrowMarker ] ]


drawCross : Style -> ( Float, Float ) -> List (Svg msg)
drawCross style ( x, y ) =
    let
        size =
            style.blockedCrossSize
    in
    [ ( ( x - size, y + size ), ( x + size, y - size ) )
    , ( ( x - size, y - size ), ( x + size, y + size ) )
    ]
        |> List.map
            (\( start, end ) ->
                SvgU.line start end [ TA.stroke <| Paint style.blockedStroke ]
            )
