module Theme exposing (Theme, defaultTheme, emptyCell, reservedCell, resourceColumn, sheetFrame, timeCell, timeColumn)

import Array exposing (Array)
import Color
import DragDrop
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font exposing (Font)
import Element.Lazy as Lazy
import Html.Attributes exposing (style)
import List.Extra
import Schedule
import Sheet exposing (Sheet)
import TimeWindow exposing (TimeWindow)
import Util.Flip exposing (flip)
import Util.Selectable as Selectable


type alias Padding =
    { top : Int, right : Int, bottom : Int, left : Int }


edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


type alias Theme =
    { defaultCell :
        { heightPx : Int
        , widthPx : Int
        , pixelsPerSecond : Float
        }
    , columns : Array ColumnStyle
    , defaultColumnStyle : ColumnStyle
    , header :
        { heightPx : Int
        , padding : Int
        , fontFamily : List Font
        , fontSize : Int
        , backgroundColor : Color
        , textColor : Color
        }
    , timeCell :
        { fontSize : Int
        , widthPx : Int
        , padding : Padding
        }
    }


type alias ColumnStyle =
    { reservedCell : { backgroundColor : Color, textColor : Color } }


type alias CellState s =
    { s | selected : Bool }


dragDropConfig =
    { starting = Sheet.DragDropStarting
    , started = Sheet.DragDropStarted
    , dragged = Sheet.DragDropTargetChanged
    , dropped = Sheet.DragDropCompleted
    , stopped = Sheet.DragDropStopped
    }


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
                |> List.map (\color -> { reservedCell = { backgroundColor = color, textColor = Color.darkBrown |> toColor } })
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
        { heightPx = defaultCellHeight
        , padding = 3
        , fontFamily = [ Font.typeface "Helvetica" ]
        , fontSize = 20
        , backgroundColor = rgba 0.8 0.8 0.8 0.8
        , textColor = rgb 0 0 0
        }
    , timeCell =
        { fontSize = 14
        , widthPx = 100
        , padding = { edges | right = 5 }
        }
    , defaultColumnStyle =
        { reservedCell =
            { backgroundColor = Color.lightOrange |> toColor
            , textColor = Color.darkBrown |> toColor
            }
        }
    }


cellHeight : Theme -> TimeWindow -> Int
cellHeight theme window =
    window
        |> TimeWindow.getDuration
        |> Duration.inSeconds
        |> (*) theme.defaultCell.pixelsPerSecond
        |> round


toColor : Color.Color -> Color
toColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha


getColumnStyle : Theme -> Int -> ColumnStyle
getColumnStyle { columns, defaultColumnStyle } paletteIndex =
    paletteIndex
        |> modBy (Array.length columns)
        |> flip Array.get columns
        |> Maybe.withDefault defaultColumnStyle


sheetFrame : Theme -> Sheet -> Element Sheet.Msg
sheetFrame theme sheet =
    let
        colSeparator =
            column [ width <| px 1, height fill, Border.solid, Border.widthEach { edges | right = 1 }, Border.color <| rgb 0.9 0.9 0.9 ]
                []

        cols =
            sheet.columns
                |> List.indexedMap
                    (\i column ->
                        anyColumn theme sheet (Sheet.makeColumnRef i) column
                    )
                |> List.intersperse colSeparator

        fillerHeader =
            stickyHeader theme []

        filler =
            column [ width fill, height fill, inFront <| fillerHeader ]
                [ fillerHeader
                ]
    in
    row
        [ width shrink -- fit contents
        , height fill
        , behindContent <| sheetBackground theme sheet
        , inFront <|
            if DragDrop.isDragging sheet.dragDropState then
                dragDropGrid theme sheet

            else
                none
        ]
        (cols ++ [ filler ])


sheetBackground : Theme -> Sheet -> Element Sheet.Msg
sheetBackground theme sheet =
    let
        border =
            [ Border.color <| rgba 0.9 0.9 0.9 1, Border.width 1, Border.dotted ]

        guides =
            List.range 1 sheet.slotCount
                |> List.map
                    (\_ -> row ([ width fill, height <| px theme.defaultCell.heightPx ] ++ border) [])
    in
    column [ width fill, height fill ] <|
        headerRow theme [] []
            :: guides


dragDropGrid : Theme -> Sheet -> Element Sheet.Msg
dragDropGrid theme sheet =
    let
        border =
            [ Border.color <| rgba 0.7 0.9 0.9 1, Border.width 1, Border.dotted ]

        guides =
            Sheet.getTimeSlots sheet
                |> List.map
                    (\window ->
                        let
                            droppable =
                                DragDrop.droppable sheet.dragDropState dragDropConfig (Sheet.DroppableWindow window)
                        in
                        row
                            ([ width fill, height <| px theme.defaultCell.heightPx ]
                                ++ border
                                ++ droppable
                            )
                            []
                    )
    in
    column [ width fill, height fill ] <|
        headerRow theme [] []
            :: guides


anyColumn : Theme -> Sheet -> Sheet.ColumnRef -> Sheet.Column -> Element Sheet.Msg
anyColumn theme sheet colRef col =
    case col of
        Sheet.TimeColumn { slots } ->
            timeColumn theme slots

        Sheet.ResourceColumn { resource, subcolumns } ->
            resourceColumn theme sheet colRef resource subcolumns


timeColumn : Theme -> List TimeWindow -> Element Sheet.Msg
timeColumn theme slots =
    let
        title =
            ""

        titleElems =
            [ el [ alignRight, centerY ] <| text title ]

        slotRows =
            slots
                |> List.drop 1
                |> List.map (timeCell theme)
    in
    column [ width <| px theme.timeCell.widthPx, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: slotRows


resourceColumn : Theme -> Sheet -> Sheet.ColumnRef -> Schedule.Resource -> List Sheet.SubColumn -> Element Sheet.Msg
resourceColumn theme sheet colRef resource subcolumns =
    let
        title =
            Schedule.getResourceName resource

        titleElems =
            [ el [ centerX, centerY ] <| text title ]

        columnStyle =
            getColumnStyle theme (Schedule.getResourcePaletteIndex resource)

        elementGrid =
            subcolumns
                |> List.indexedMap
                    (\subColIndex subcolumn ->
                        subcolumn
                            |> List.indexedMap
                                (\cellIndex cell ->
                                    let
                                        cellRef =
                                            Sheet.makeCellRef colRef subColIndex cellIndex

                                        selected =
                                            Just cellRef == sheet.selectedCell
                                    in
                                    anyCell theme columnStyle sheet cellRef cell { selected = selected }
                                )
                    )

        subcolumnsEl =
            row [ width fill, height fill, paddingXY 1 0 ] <|
                List.map subcolumnEl elementGrid

        subcolumnEl els =
            column [ width fill, height fill ] <| els
    in
    column [ width <| px theme.defaultCell.widthPx, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: [ subcolumnsEl ]


anyCell : Theme -> ColumnStyle -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
anyCell theme columnStyle sheet cellRef cell state =
    case cell of
        Sheet.EmptyCell _ ->
            emptyCell theme columnStyle sheet cellRef cell state

        Sheet.ReservedCell _ ->
            reservedCell theme columnStyle sheet cellRef cell state


stickyHeader : Theme -> List (Element Sheet.Msg) -> Element Sheet.Msg
stickyHeader theme elems =
    let
        sticky =
            [ style "position" "sticky", style "top" "0" ] |> List.map htmlAttribute

        bgColor =
            Background.color <| theme.header.backgroundColor
    in
    headerRow theme (bgColor :: sticky) elems


headerRow : Theme -> List (Attribute Sheet.Msg) -> List (Element Sheet.Msg) -> Element Sheet.Msg
headerRow theme attrs elems =
    row
        ([ width fill, padding theme.header.padding, height <| px theme.header.heightPx ] ++ attrs)
        elems


timeCell : Theme -> TimeWindow -> Element Sheet.Msg
timeCell theme window =
    let
        h =
            cellHeight theme window
    in
    row
        [ width fill, height <| px h, moveDown <| toFloat theme.timeCell.fontSize / 2, paddingEach <| theme.timeCell.padding ]
    <|
        [ el [ alignRight, Font.size theme.timeCell.fontSize, alignBottom ] <| text <| TimeWindow.formatStart window ]


emptyCell : Theme -> ColumnStyle -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
emptyCell theme columnStyle sheet cellRef cell state =
    let
        attrs =
            [ Events.onClick (Sheet.CellClicked cell cellRef) ]
    in
    renderCell theme attrs <|
        Sheet.cellWindow cell


reservedCell : Theme -> ColumnStyle -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
reservedCell theme columnStyle sheet cellRef cell { selected } =
    let
        topHandle =
            cellResizeHandle theme sheet (Sheet.CellStart cellRef)

        bottomHandle =
            cellResizeHandle theme sheet (Sheet.CellEnd cellRef)

        handles =
            if selected then
                [ above <|
                    topHandle
                , below <|
                    bottomHandle
                ]

            else
                []

        attrs =
            Background.color columnStyle.reservedCell.backgroundColor
                :: Border.rounded 3
                :: Border.shadow { offset = ( 1, 1 ), size = 0.005, blur = 5.0, color = rgb 0.5 0.5 0.5 }
                :: Events.onClick (Sheet.CellClicked cell cellRef)
                :: (if selected then
                        [ above <|
                            topHandle
                        , below <|
                            bottomHandle
                        ]

                    else
                        []
                   )
                ++ handles

        visibleWindow =
            TimeWindow.intersection sheet.window <| Sheet.cellWindow cell
    in
    case visibleWindow of
        Just window ->
            renderCell theme attrs <| window

        Nothing ->
            none


unselectable : List (Attribute Sheet.Msg)
unselectable =
    [ style "-moz-user-select" "none"
    , style "-khtml-user-select" "none"
    , style "-webkit-user-select" "none"
    , style "-ms-user-select" "none"
    , style "user-select" "none"
    ]
        |> List.map htmlAttribute


cellResizeHandle : Theme -> Sheet -> Sheet.Draggable -> Element Sheet.Msg
cellResizeHandle theme sheet draggable =
    let
        handleHeight =
            15

        move : Attribute Sheet.Msg
        move =
            case draggable of
                Sheet.CellStart _ ->
                    moveDown handleHeight

                Sheet.CellEnd _ ->
                    moveUp handleHeight

        draggableAttrs =
            DragDrop.draggable sheet.dragDropState dragDropConfig draggable

        handle : Element Sheet.Msg
        handle =
            el
                (width fill
                    :: (height <| px handleHeight)
                    :: (Background.color <| rgb 0.3 0.3 0.3)
                    :: move
                    :: centerX
                    :: Events.onCustom "click" { stopPropagation = True, preventDefault = True } Sheet.Noop
                    :: unselectable
                    ++ draggableAttrs
                )
            <|
                none
    in
    handle


renderCell : Theme -> List (Attribute Sheet.Msg) -> TimeWindow -> Element Sheet.Msg
renderCell theme attrs window =
    row [ paddingXY 0 1, width fill, height <| px <| cellHeight theme window ]
        [ el ([ width fill, height fill, Font.size 12 ] ++ attrs) none ]
