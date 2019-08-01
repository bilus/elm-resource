module DragDrop exposing (Pos, State, draggable, dragged, droppable, getDragItem, init, isDragging, isIdle, mapDragged, started, starting, stopped)

import Element exposing (Attribute)
import Element.Events as Events
import Html.Attributes exposing (property)
import Json.Encode



{- Specifies messages sent on various drag & drop events. -}


type alias Config msg draggable droppable =
    { starting : draggable -> msg
    , started : msg
    , dragged : Maybe droppable -> msg
    , stopped : msg
    , dropped : droppable -> msg
    }


type
    State draggable droppable
    -- TODO: Probably Idle -> Starting -> Dragging
    = Idle
    | Starting
        { draggedItem : draggable
        , startPos : Pos
        }
    | Started
        { draggedItem : draggable
        , dropTarget : Maybe droppable
        }


type alias Pos =
    { x : Int, y : Int }


init : State draggable droppable
init =
    Idle


starting : draggable -> Pos -> State draggable droppable -> State draggable droppable
starting draggedItem startPos state =
    Starting { draggedItem = draggedItem, startPos = startPos }


started : State draggable droppable -> State draggable droppable
started state =
    case state of
        Idle ->
            Idle

        Starting { draggedItem, startPos } ->
            Started { draggedItem = draggedItem, dropTarget = Nothing }

        Started s ->
            Started s


dragged : Maybe droppable -> State draggable droppable -> State draggable droppable
dragged dropTarget state =
    case state of
        Idle ->
            Idle

        Starting { draggedItem, startPos } ->
            Started { draggedItem = draggedItem, dropTarget = dropTarget }

        Started s ->
            Started { s | dropTarget = dropTarget }


stopped : State draggable droppable -> State draggable droppable
stopped state =
    init


isIdle : State draggable droppable -> Bool
isIdle state =
    case state of
        Idle ->
            True

        Starting _ ->
            False

        Started _ ->
            False


isDragging : State draggable droppable -> Bool
isDragging state =
    case state of
        Idle ->
            False

        Starting _ ->
            False

        Started _ ->
            True


getDragItem : State draggable droppable -> Maybe draggable
getDragItem state =
    case state of
        Idle ->
            Nothing

        Starting { draggedItem } ->
            Just draggedItem

        Started { draggedItem } ->
            Just draggedItem


mapDragged : (draggable -> draggable) -> State draggable droppable -> State draggable droppable
mapDragged f state =
    case state of
        Starting s ->
            Starting { s | draggedItem = f s.draggedItem }

        Started s ->
            Started { s | draggedItem = f s.draggedItem }

        Idle ->
            Idle


draggable : State draggable droppable -> Config msg draggable droppable -> draggable -> List (Attribute msg)
draggable state config draggedItem =
    if not <| isIdle state then
        [ Events.onMouseMove config.started
        ]

    else
        [ Events.onMouseDown (config.starting draggedItem)
        ]


droppable : State draggable droppable -> Config msg draggable droppable -> droppable -> List (Attribute msg)
droppable state config dropTarget =
    let
        droppableAttrs =
            [ Events.onMouseEnter (config.dragged (Just dropTarget))
            , Events.onMouseLeave (config.dragged Nothing)
            , Events.onMouseUp (config.dropped dropTarget)
            ]
    in
    case state of
        Idle ->
            []

        Starting { draggedItem } ->
            droppableAttrs

        Started { draggedItem } ->
            droppableAttrs
