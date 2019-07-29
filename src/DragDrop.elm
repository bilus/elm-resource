module DragDrop exposing (State, drag, draggable, droppable, init, isDragging, mapDragged, start, stop)

import Element exposing (Attribute)
import Element.Events as Events
import Html.Attributes exposing (property)
import Json.Encode


type alias Config msg draggable droppable =
    { started : draggable -> msg
    , dragged : draggable -> Maybe droppable -> msg
    , canceled : draggable -> msg
    , dropped : draggable -> droppable -> msg
    }


type alias State draggable droppable =
    { dragged : Maybe draggable
    , dropTarget : Maybe droppable
    }


init : State draggable droppable
init =
    { dragged = Nothing, dropTarget = Nothing }


start : draggable -> State draggable droppable -> State draggable droppable
start dragged state =
    { state | dragged = Just dragged }


drag : Maybe droppable -> State draggable droppable -> State draggable droppable
drag dropTarget state =
    { state | dropTarget = dropTarget }


stop : State draggable droppable -> State draggable droppable
stop state =
    init


isDragging : State draggable droppable -> Bool
isDragging state =
    case state.dragged of
        Just _ ->
            True

        Nothing ->
            False


mapDragged : (draggable -> draggable) -> State draggable droppable -> State draggable droppable
mapDragged f state =
    { state | dragged = Maybe.map f state.dragged }


draggable : State draggable droppable -> Config msg draggable droppable -> draggable -> List (Attribute msg)
draggable state config dragged =
    [ Events.onMouseDown (config.started dragged)
    ]


droppable : State draggable droppable -> Config msg draggable droppable -> droppable -> List (Attribute msg)
droppable state config dropTarget =
    case state.dragged of
        Just dragged ->
            [ Events.onMouseEnter (config.dragged dragged (Just dropTarget))
            , Events.onMouseLeave (config.dragged dragged Nothing)
            , Events.onMouseUp (config.dropped dragged dropTarget)
            ]

        Nothing ->
            []
