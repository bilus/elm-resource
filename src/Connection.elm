module Connection exposing (Connection, Kind(..), render)

-- import Sheet exposing (CellRef, Sheet, cellWindow)

import Color
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


render : Sheet -> Theme -> String -> Connection d -> List (Svg msg)
render sheet theme arrowMarkerId connection =
    let
        from =
            Theme.xy sheet theme connection.fromCell connection.fromTime

        to =
            Theme.xy sheet theme connection.toCell connection.toTime

        dist =
            15
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
                        * Basics.min dist (abs <| xt - xf)

                points =
                    [ ( xf, yf )
                    , ( xf + d, yf )
                    , ( xt - d, yt )
                    , ( xt, yt )
                    ]
            in
            case connection.kind of
                Strong ->
                    renderStrong points arrowMarkerId

                Weak ->
                    renderWeak points arrowMarkerId

                Blocked ->
                    renderBlocked points arrowMarkerId
        )
        from
        to
        |> Maybe.withDefault []


renderStrong : List ( Float, Float ) -> String -> List (Svg msg)
renderStrong points arrowMarkerId =
    [ SvgU.polyline points
        [ TA.stroke <| Paint Color.black
        , SvgU.markerEndUrl arrowMarkerId
        ]
    ]


renderWeak : List ( Float, Float ) -> String -> List (Svg msg)
renderWeak points arrowMarkerId =
    [ SvgU.polyline points [ SvgU.markerEndUrl arrowMarkerId ] ]


renderBlocked : List ( Float, Float ) -> String -> List (Svg msg)
renderBlocked points arrowMarkerId =
    [ SvgU.polyline points [ SvgU.markerEndUrl arrowMarkerId ] ]
