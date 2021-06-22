module Overlay.Connections exposing (render)

import Connection exposing (..)
import Element exposing (Element, fill, height, width)
import Sheet exposing (Sheet)
import Theme exposing (Theme)
import Util.List
import Util.Svg as Svg exposing (Svg)


render : Sheet -> Theme -> List (Connection d) -> Element msg
render sheet theme connections =
    let
        lines =
            connections
                |> List.map (renderConnection sheet theme)
                |> Util.List.compact

        svg =
            Svg.svg [ width fill, height fill ]
                lines
    in
    svg


renderConnection : Sheet -> Theme -> Connection d -> Maybe (Svg msg)
renderConnection sheet theme connection =
    let
        from =
            Theme.xy sheet theme connection.fromCell connection.fromTime
                |> Debug.log "** from"

        to =
            Theme.xy sheet theme connection.toCell connection.toTime
                |> Debug.log "** to"
    in
    Maybe.map2 Svg.line from to
