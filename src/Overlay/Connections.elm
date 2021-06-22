module Overlay.Connections exposing (..)

import Connection exposing (..)
import Element exposing (Element, fill, height, width)
import Theme exposing (Theme)
import Util.Svg as Svg


render : List (Connection d) -> Theme -> Element msg
render connections theme =
    let
        _ =
            Debug.log "aa" 10

        lines =
            theme.slots
                |> List.foldr
                    (\window ( ls, offsetY ) ->
                        let
                            height =
                                Theme.cellHeight theme window

                            line =
                                Svg.line ( 0, offsetY ) ( 100, offsetY + toFloat height )
                        in
                        ( line :: ls, offsetY + toFloat height )
                    )
                    ( [], toFloat theme.header.heightPx )
                |> Tuple.first

        svg =
            Svg.svg [ width fill, height fill ]
                lines
    in
    svg
