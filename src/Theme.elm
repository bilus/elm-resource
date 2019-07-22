module Theme exposing (Theme, defaultTheme, emptyCell, reservedCell, resourceColumn, sheetFrame, timeCell, timeColumn)

import Array exposing (Array)
import Color
import DragDrop
import Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font exposing (Font)
import Html.Attributes exposing (style)
import Schedule
import Sheet exposing (Sheet)
import TimeWindow exposing (TimeWindow)
import Util.Selectable as Selectable


type alias Theme =
    { defaultCell : { heightPx : Int, widthPx : Int, pixelsPerSecond : Float }
    , columns : Array { bgColor : Color }
    , header : { fontFamily : List Font, fontSize : Int, backgroundColor : Color, textColor : Color }
    , cells : { fontFamily : List Font, fontSize : Int, backgroundColor : Color, textColor : Color }
    }


type alias CellState s =
    { s | selected : Bool }


dragDropConfig =
    { started = Sheet.MoveStarted
    , dragged = \draggable droppable _ -> Sheet.MoveTargetChanged draggable droppable
    , dropped = Sheet.MoveCompleted
    , canceled = Sheet.MoveCanceled
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


cellHeight : Theme -> TimeWindow -> Length
cellHeight theme window =
    window
        |> TimeWindow.getDuration
        |> Duration.inSeconds
        |> (*) theme.defaultCell.pixelsPerSecond
        |> round
        |> px


toColor : Color.Color -> Color
toColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha


sheetFrame : Theme -> Sheet -> Element Sheet.Msg
sheetFrame theme sheet =
    let
        cols =
            sheet.columns
                |> List.indexedMap
                    (\i column ->
                        anyColumn theme sheet (Sheet.makeColumnRef i) column
                    )
    in
    row
        [ width fill
        , height fill
        ]
        cols


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
            "Czas"

        titleElems =
            [ el [ alignRight, centerY ] <| text title ]

        topPadding =
            toFloat theme.defaultCell.heightPx / 2 |> round

        slotRows =
            slots
                |> List.drop 1
                |> List.map (timeCell theme)
    in
    column [ width fill, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: slotRows


resourceColumn : Theme -> Sheet -> Sheet.ColumnRef -> Schedule.Resource -> List Sheet.SubColumn -> Element Sheet.Msg
resourceColumn theme sheet colRef resource subcolumns =
    let
        title =
            Schedule.getResourceName resource

        titleElems =
            [ el [ centerX, centerY ] <| text title ]

        elementGrid =
            subcolumns
                |> List.indexedMap
                    (\subColIndex subcolumn ->
                        subcolumn
                            |> Selectable.indexedMapWithState
                                (\cellIndex cell ->
                                    anyCell theme sheet (Sheet.makeCellRef colRef subColIndex cellIndex) cell
                                )
                            |> Selectable.toList
                    )

        subcolumnsEl =
            row [ width fill, height fill, paddingXY 1 0 ] <|
                List.map subcolumnEl elementGrid

        subcolumnEl els =
            column [ width fill, height fill ] <| els
    in
    column [ width fill, height fill, inFront <| stickyHeader theme titleElems ] <|
        stickyHeader theme titleElems
            :: [ subcolumnsEl ]


anyCell : Theme -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
anyCell theme sheet cellRef cell state =
    case cell of
        Sheet.EmptyCell _ ->
            emptyCell theme sheet cellRef cell state

        Sheet.ReservedCell _ ->
            reservedCell theme sheet cellRef cell state


stickyHeader : Theme -> List (Element Sheet.Msg) -> Element Sheet.Msg
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


timeCell : Theme -> TimeWindow -> Element Sheet.Msg
timeCell theme window =
    row
        [ width fill, height (cellHeight theme window) ]
    <|
        [ el [ alignRight, centerY ] <| text <| TimeWindow.formatStart window ]


emptyCell : Theme -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
emptyCell theme sheet cellRef cell state =
    let
        attrs =
            [ Events.onClick (Sheet.CellClicked cell cellRef) ]
    in
    (renderCell theme attrs <|
        Sheet.cellWindow cell
    )
        |> DragDrop.makeDroppable sheet.dragDropState dragDropConfig (Sheet.DroppableCell cell cellRef)


reservedCell : Theme -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
reservedCell theme sheet cellRef cell { selected } =
    let
        topHandle =
            handle theme
                |> (if selected then
                        DragDrop.makeDraggable sheet.dragDropState
                            dragDropConfig
                            (Sheet.CellStart cell cellRef)

                    else
                        identity
                   )

        bottomHandle =
            handle theme
                |> (if selected then
                        DragDrop.makeDraggable sheet.dragDropState
                            dragDropConfig
                            (Sheet.CellEnd cell cellRef)

                    else
                        identity
                   )

        attrs =
            Background.color theme.cells.backgroundColor
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
    in
    (renderCell theme attrs <| Sheet.cellWindow cell)
        |> DragDrop.makeDroppable sheet.dragDropState dragDropConfig (Sheet.DroppableCell cell cellRef)


handle : Theme -> Element Sheet.Msg
handle theme =
    el [ centerX ] <| text "-- o --"


renderCell : Theme -> List (Attribute Sheet.Msg) -> TimeWindow -> Element Sheet.Msg
renderCell theme attrs window =
    row [ paddingXY 0 1, width fill, height (cellHeight theme window) ]
        [ el ([ width fill, height fill, Font.size 12 ] ++ attrs) <| text "" ]
