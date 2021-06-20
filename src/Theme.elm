module Theme exposing (Theme, defaultTheme, emptyCell, reservedCell, resourceColumn, sheetFrame, timeCell, timeColumn)

import Array exposing (Array)
import Color
import DragDrop
import Duration exposing (Duration)
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , above
        , alignRight
        , behindContent
        , below
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , inFront
        , moveDown
        , moveUp
        , none
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb
        , rgba
        , row
        , shrink
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font exposing (Font)
import Html.Attributes exposing (style)
import Schedule
import Sheet exposing (Sheet)
import Time exposing (Posix, Zone)
import Time.Extra exposing (Interval(..))
import TimeWindow exposing (TimeWindow)
import Util.Flip exposing (flip)
import Util.List
import Util.Time


type alias Padding =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


edges : { top : Int, right : Int, bottom : Int, left : Int }
edges =
    { top = 0, right = 0, bottom = 0, left = 0 }


type alias Theme =
    { defaultCell :
        { heightPx : Int
        , widthPx : Int
        , pixelsPerSecond : Float
        }
    , slots : List TimeWindow
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
    { reservedCell :
        { backgroundColor : Color
        , textColor : Color
        }
    }


type alias CellState s =
    { s | selected : Bool }


dragDropConfig : DragDrop.Config Sheet.Msg Sheet.Draggable Sheet.Droppable
dragDropConfig =
    { starting = Sheet.DragDropStarting
    , started = Sheet.DragDropStarted
    , dragged = Sheet.DragDropTargetChanged
    , dropped = Sheet.DragDropCompleted
    , stopped = Sheet.DragDropStopped
    }


defaultTheme : Duration -> TimeWindow -> Theme
defaultTheme slotDuration window =
    let
        columns =
            [ Color.darkOrange
            , Color.darkYellow
            , Color.darkGreen
            , Color.darkBlue
            , Color.darkPurple
            , Color.darkBrown
            , Color.orange
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
            ]
                |> List.map (setAlpha 0.5)
                |> List.map toColor
                |> List.map (\color -> { reservedCell = { backgroundColor = color, textColor = Color.darkBrown |> toColor } })
                |> Array.fromList

        defaultCellHeight =
            30

        windowDuration =
            window
                |> TimeWindow.getDuration
                |> Duration.inSeconds

        slotDurationInSec =
            slotDuration |> Duration.inSeconds

        slotCount =
            windowDuration
                / slotDurationInSec
                |> floor

        pixelsPerSecond =
            toFloat defaultCellHeight / slotDurationInSec
    in
    { slots = TimeWindow.split slotCount window
    , defaultCell =
        { heightPx = defaultCellHeight
        , widthPx = 200
        , pixelsPerSecond = pixelsPerSecond
        }
    , columns = columns
    , header =
        { heightPx = 26
        , padding = 3
        , fontFamily = [ Font.typeface "Helvetica" ]
        , fontSize = 14
        , backgroundColor = rgba 0.8 0.8 0.8 0.8
        , textColor = rgb 0 0 0
        }
    , timeCell =
        { fontSize = 14
        , widthPx = 200
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


setAlpha : Float -> Color.Color -> Color.Color
setAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.fromRgba { rgba | alpha = alpha }


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
                        resourceColumn theme sheet (Sheet.makeColumnRef i) column
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
        (timeColumn theme :: cols ++ [ filler ])


slotWindows : Theme -> List ( TimeWindow, TimeWindow )
slotWindows theme =
    let
        maybePlaceholder =
            theme.slots
                |> List.head
                |> Maybe.map makeTimeWindowBefore
    in
    maybePlaceholder
        |> Maybe.map
            (\placeholder -> placeholder :: theme.slots)
        -- (\placeholder -> theme.slots)
        |> Maybe.withDefault theme.slots
        |> Util.List.window2


makeTimeWindowBefore : TimeWindow -> TimeWindow
makeTimeWindowBefore w =
    let
        startMs =
            TimeWindow.getStart w
                |> Time.posixToMillis

        prevStart =
            startMs
                - 1000
                |> Time.millisToPosix
    in
    TimeWindow.make prevStart (Duration.milliseconds 1000)


sheetBackground : Theme -> Sheet -> Element Sheet.Msg
sheetBackground theme sheet =
    let
        guides =
            theme
                |> slotWindows
                |> List.map
                    (\( prev, crnt ) ->
                        let
                            nowMarker =
                                sheet.nowMarker
                                    |> Maybe.andThen
                                        (\now ->
                                            if TimeWindow.includes now crnt then
                                                let
                                                    offset =
                                                        Time.Extra.diff Millisecond Time.utc (TimeWindow.getStart crnt) now |> toFloat

                                                    pixels =
                                                        offset / 1000.0 * theme.defaultCell.pixelsPerSecond |> round
                                                in
                                                Just pixels

                                            else
                                                Nothing
                                        )

                            isCurrentDay =
                                sheet.nowMarker
                                    |> Maybe.map
                                        (\now ->
                                            isSameDay Time.utc now crnt
                                        )
                                    |> Maybe.withDefault False
                        in
                        guide theme (isDayBoundary prev crnt) isCurrentDay nowMarker
                    )
    in
    column [ width fill, height fill ] <|
        headerRow theme [] []
            :: guides


guide : Theme -> Bool -> Bool -> Maybe Int -> Element Sheet.Msg
guide theme boundary currentDay nowMarker =
    let
        regularBorder =
            [ Border.color <| rgba 0.9 0.9 0.9 1, Border.widthEach { edges | bottom = 1 }, Border.dotted ]

        boundaryMarker =
            if boundary then
                row [ width fill, height fill, Border.color <| rgba 1 0.5 0.5 1, Border.widthEach { edges | top = 1 }, Border.dotted ] []

            else
                none

        background =
            if currentDay then
                [ Background.color <| rgba 0.7 0.8 0.6 0.1 ]

            else
                []
    in
    row ([ width fill, height <| px theme.defaultCell.heightPx ] ++ regularBorder ++ background) <|
        (boundaryMarker
            :: (nowMarker
                    |> Maybe.map (currentTimeIndicator theme)
                    |> Maybe.map List.singleton
                    |> Maybe.withDefault []
               )
        )


currentTimeIndicator : Theme -> Int -> Element Sheet.Msg
currentTimeIndicator _ offset =
    row
        [ width fill
        , height fill
        , moveDown <| toFloat offset
        , Border.widthEach { edges | top = 2 }
        , Border.solid
        , Border.color <| rgba 0 0 0 1
        ]
        []


dragDropGrid : Theme -> Sheet -> Element Sheet.Msg
dragDropGrid theme sheet =
    let
        border =
            [ Border.color <| rgba 0.7 0.9 0.9 0.3, Border.width 1, Border.dotted ]

        guides =
            theme.slots
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


resourceColumn : Theme -> Sheet -> Sheet.ColumnRef -> Sheet.Column -> Element Sheet.Msg
resourceColumn theme sheet colRef { resource, layers } =
    let
        title =
            Schedule.getResourceName resource

        titleElems =
            [ el [ centerX, centerY ] <| text title ]

        columnStyle =
            getColumnStyle theme (Schedule.getResourcePaletteIndex resource)

        elementGrid =
            layers
                |> List.indexedMap
                    (\layerIndex layer ->
                        layer
                            |> List.indexedMap
                                (\cellIndex cell ->
                                    let
                                        cellRef =
                                            Sheet.makeCellRef colRef layerIndex cellIndex

                                        selected =
                                            Just cellRef == sheet.selectedCell
                                    in
                                    anyCell theme columnStyle sheet cellRef cell { selected = selected }
                                )
                    )

        layersEl =
            row [ width fill, height fill, paddingXY 1 0 ] <|
                List.map layerEl elementGrid

        layerEl els =
            column [ width fill, height fill ] <| els
    in
    column [ width <| px theme.defaultCell.widthPx, height fill, inFront <| stickyHeader theme titleElems ] <|
        [ stickyHeader theme titleElems, layersEl ]


timeColumn : Theme -> Element Sheet.Msg
timeColumn theme =
    let
        slotRows =
            theme
                |> slotWindows
                |> List.map (timeCell theme)
    in
    column [ width <| px theme.timeCell.widthPx, height fill ] <|
        headerRow theme [ Background.color <| rgba 1 1 1 1 ] []
            :: slotRows


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
        ([ width fill
         , padding theme.header.padding
         , height <| px theme.header.heightPx
         , Font.size <| theme.header.fontSize
         ]
            ++ attrs
        )
        elems


isSameDay : Zone -> Posix -> TimeWindow -> Bool
isSameDay zone time w =
    let
        wholeDay =
            TimeWindow.makeDay zone time
    in
    TimeWindow.contains wholeDay w


isDayBoundary : TimeWindow -> TimeWindow -> Bool
isDayBoundary w1 w2 =
    let
        -- TODO: Make zone configurable
        day =
            Time.toDay Time.utc
    in
    (w1 |> TimeWindow.getStart |> day) /= (w2 |> TimeWindow.getStart |> day)


timeCell : Theme -> ( TimeWindow, TimeWindow ) -> Element Sheet.Msg
timeCell theme ( prevWindow, window ) =
    let
        h =
            cellHeight theme window

        label =
            if isDayBoundary prevWindow window then
                TimeWindow.getStart window |> Util.Time.formatDateTime Time.utc

            else
                TimeWindow.getStart window |> Util.Time.formatTime Time.utc
    in
    row
        [ width fill, height <| px h, moveUp <| toFloat theme.defaultCell.heightPx / 2, paddingEach <| theme.timeCell.padding, Background.color <| rgba 1 1 1 1 ]
    <|
        -- TODO: Make zone configurable
        [ el [ alignRight, Font.size theme.timeCell.fontSize, centerY ] <|
            text label
        ]


emptyCell : Theme -> ColumnStyle -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
emptyCell theme _ _ cellRef cell _ =
    let
        attrs =
            [ Events.onClick (Sheet.CellClicked cell cellRef) ]
    in
    renderCell theme attrs none <| Sheet.cellWindow cell


reservedCell : Theme -> ColumnStyle -> Sheet -> Sheet.CellRef -> Sheet.Cell -> CellState s -> Element Sheet.Msg
reservedCell theme columnStyle sheet cellRef cell { selected } =
    let
        topHandle =
            cellResizeHandle theme sheet (Sheet.CellStart cellRef)

        bottomHandle =
            cellResizeHandle theme sheet (Sheet.CellEnd cellRef)

        attrs =
            [ Background.color columnStyle.reservedCell.backgroundColor
            , padding 2
            , Events.onClick (Sheet.CellClicked cell cellRef)
            ]

        contents =
            el
                [ Font.color (rgba 0.1 0.1 0.1 0.6), padding 2 ]
            <|
                paragraph [] <|
                    [ text <| formatCellLabel theme sheet window ]

        window =
            Sheet.cellWindow cell

        maybeVisibleWindow =
            TimeWindow.intersection sheet.window <|
                window
    in
    case maybeVisibleWindow of
        Just visibleWindow ->
            let
                ( topOutlier, bottomOutlier ) =
                    TimeWindow.substract window visibleWindow

                top =
                    case ( selected, topOutlier ) of
                        ( _, Just tow ) ->
                            [ above <| renderOutlier theme columnStyle tow 0 ]

                        ( True, Nothing ) ->
                            [ above <| topHandle ]

                        ( False, Nothing ) ->
                            []

                bottom =
                    case ( selected, bottomOutlier ) of
                        ( _, Just bow ) ->
                            [ below <| renderOutlier theme columnStyle bow pi ]

                        ( True, Nothing ) ->
                            [ below <| bottomHandle ]

                        ( False, Nothing ) ->
                            []
            in
            renderCell theme (attrs ++ top ++ bottom) contents <| visibleWindow

        Nothing ->
            none


renderOutlier : Theme -> ColumnStyle -> TimeWindow -> Float -> Element Sheet.Msg
renderOutlier theme columnStyle _ angle =
    let
        noColor =
            rgba 1 1 1 0
    in
    row
        [ paddingXY 0 1
        , width fill
        , height <| px <| theme.defaultCell.heightPx
        , Background.gradient
            { angle = angle
            , steps =
                [ columnStyle.reservedCell.backgroundColor
                , noColor
                ]
            }
        ]
        []


formatCellLabel : Theme -> Sheet -> TimeWindow -> String
formatCellLabel theme sheet window =
    -- TODO: Make zone configurable
    let
        start =
            TimeWindow.getStart window

        end =
            TimeWindow.getEnd window
    in
    if TimeWindow.contains sheet.window window then
        let
            startTime =
                start |> Util.Time.formatTime Time.utc

            endTime =
                end |> Util.Time.formatTime Time.utc

            isRegular =
                modBy theme.defaultCell.heightPx (cellHeight theme window) == 0
        in
        if isRegular then
            startTime

        else
            startTime ++ " – " ++ endTime

    else
        start |> Util.Time.formatDateTime Time.utc


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
cellResizeHandle _ sheet draggable =
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
                    -- :: Events.onCustom "click" { stopPropagation = True, preventDefault = True } Sheet.Noop
                    :: unselectable
                    ++ draggableAttrs
                )
            <|
                none
    in
    handle


renderCell : Theme -> List (Attribute Sheet.Msg) -> Element Sheet.Msg -> TimeWindow -> Element Sheet.Msg
renderCell theme attrs elem window =
    row [ paddingXY 0 1, width fill, height <| px <| cellHeight theme window ]
        [ el ([ width fill, height fill, Font.size 12 ] ++ attrs) elem ]
