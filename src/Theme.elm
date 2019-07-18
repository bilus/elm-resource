module Theme exposing (Theme, defaultTheme, emptyCell, reservedCell, resourceColumn, sheetFrame, timeCell, timeColumn)

import Array exposing (Array)
import Color
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font exposing (Font)
import Html.Attributes exposing (style)
import TimeWindow exposing (TimeWindow)


type alias Theme =
    { defaultCell : { heightPx : Int, widthPx : Int, pixelsPerSecond : Float }
    , columns : Array { bgColor : Color }
    , header : { fontFamily : List Font, fontSize : Int, backgroundColor : Color, textColor : Color }
    , cells : { fontFamily : List Font, fontSize : Int, backgroundColor : Color, textColor : Color }
    }


type alias CellState s =
    { s | selected : Bool }


defaultTheme : Int -> TimeWindow -> Theme
defaultTheme slotCount window =
    let
        columns =
            [ Color.orange
            , Color.yellow
            , Color.green
            , Color.blue
            , Color.purple
            , Color.brown
            , Color.lightOrange
            , Color.lightYellow
            , Color.lightGreen
            , Color.lightBlue
            , Color.lightPurple
            , Color.lightBrown
            , Color.darkOrange
            , Color.darkYellow
            , Color.darkGreen
            , Color.darkBlue
            , Color.darkPurple
            , Color.darkBrown
            ]
                |> List.map toColor
                |> List.map (\color -> { bgColor = color })
                |> Array.fromList

        defaultCellHeight =
            30

        windowDuration =
            window
                |> TimeWindow.getDuration
                |> Duration.inSeconds

        cellDuration =
            windowDuration / toFloat slotCount

        pixelsPerSecond =
            toFloat defaultCellHeight / cellDuration
    in
    { defaultCell =
        { heightPx = defaultCellHeight
        , widthPx = 200
        , pixelsPerSecond = pixelsPerSecond
        }
    , columns = columns
    , header =
        { fontFamily = [ Font.typeface "Helvetica" ]
        , fontSize = 20
        , backgroundColor = rgba 0.8 0.8 0.8 0.8
        , textColor = rgb 0 0 0
        }
    , cells =
        { fontFamily = [ Font.typeface "Helvetica" ]
        , fontSize = 20
        , backgroundColor = Color.lightOrange |> toColor
        , textColor = Color.darkBrown |> toColor
        }
    }


toColor : Color.Color -> Color
toColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha


sheetFrame : Theme -> List (Element msg) -> Element msg
sheetFrame theme =
    row
        [ width fill
        , height fill
        ]


timeColumn : Theme -> String -> List (Element msg) -> Element msg
timeColumn theme title elements =
    let
        titleElems =
            [ el [ alignRight, centerY ] <| text title ]

        topPadding =
            toFloat theme.defaultCell.heightPx / 2 |> round
    in
    column [ width fill, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: elements


resourceColumn : Theme -> String -> List (List (Element msg)) -> Element msg
resourceColumn theme title elementGrid =
    let
        titleElems =
            [ el [ centerX, centerY ] <| text title ]

        subcolumnsEl =
            row [ width fill, height fill, paddingXY 1 0 ] <|
                List.map subcolumnEl elementGrid

        subcolumnEl els =
            column [ width fill, height fill ] <| els
    in
    column [ width fill, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: [ subcolumnsEl ]


stickyHeader : Theme -> List (Element msg) -> Element msg
stickyHeader theme elems =
    let
        sticky =
            [ style "position" "sticky", style "top" "0" ] |> List.map htmlAttribute
    in
    row
        ([ width fill
         , Background.color <| theme.header.backgroundColor
         , padding 3
         ]
            ++ sticky
        )
        elems


timeCell : Theme -> TimeWindow -> Element msg
timeCell theme window =
    row
        [ width fill, height (cellHeight theme window) ]
    <|
        [ el [ alignRight, centerY ] <| text <| TimeWindow.formatStart window ]


emptyCell : Theme -> TimeWindow -> CellState s -> Element msg -> msg -> Element msg
emptyCell theme window state elem onClickCell =
    let
        attrs =
            []
    in
    cell theme attrs window elem onClickCell


reservedCell : Theme -> TimeWindow -> CellState s -> Element msg -> msg -> Element msg
reservedCell theme window { selected } elem onClickCell =
    let
        attrs =
            [ if selected then
                Background.color <| rgba 0.9 0.4 0.3 0.1

              else
                Background.color theme.cells.backgroundColor
            ]
    in
    cell theme attrs window elem onClickCell


cell : Theme -> List (Attribute msg) -> TimeWindow -> Element msg -> msg -> Element msg
cell theme attrs window elem onClickCell =
    row [ paddingXY 0 1, width fill, height (cellHeight theme window), Events.onClick onClickCell ]
        [ el ([ width fill, height fill, Font.size 12 ] ++ attrs) <| elem ]


cellHeight : Theme -> TimeWindow -> Length
cellHeight theme window =
    window
        |> TimeWindow.getDuration
        |> Duration.inSeconds
        |> (*) theme.defaultCell.pixelsPerSecond
        |> round
        |> px
