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


type State draggable droppable
    = Idle
    | Started
        { dragged : draggable
        , dropTarget : Maybe droppable
        }


init : State draggable droppable
init =
    Idle


start : draggable -> State draggable droppable -> State draggable droppable
start dragged state =
    Started { dragged = dragged, dropTarget = Nothing }


drag : Maybe droppable -> State draggable droppable -> State draggable droppable
drag dropTarget state =
    case state of
        Idle ->
            Idle

        Started s ->
            Started { s | dropTarget = dropTarget }


stop : State draggable droppable -> State draggable droppable
stop state =
    init


isDragging : State draggable droppable -> Bool
isDragging state =
    case state of
        Started _ ->
            True

        Idle ->
            False


mapDragged : (draggable -> draggable) -> State draggable droppable -> State draggable droppable
mapDragged f state =
    case state of
        Idle ->
            Idle

        Started s ->
            Started { s | dragged = f s.dragged }


draggable : State draggable droppable -> Config msg draggable droppable -> draggable -> List (Attribute msg)
draggable state config dragged =
    [ Events.onMouseDown (config.started dragged)
    ]


droppable : State draggable droppable -> Config msg draggable droppable -> droppable -> List (Attribute msg)
droppable state config dropTarget =
    case state of
        Started { dragged } ->
            [ Events.onMouseEnter (config.dragged dragged (Just dropTarget))
            , Events.onMouseLeave (config.dragged dragged Nothing)
            , Events.onMouseUp (config.dropped dragged dropTarget)
            ]

        Idle ->
            []
