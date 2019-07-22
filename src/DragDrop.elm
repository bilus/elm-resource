module DragDrop exposing (State, drag, init, makeDraggable, makeDroppable, start, stop)

import Element exposing (Element)
import Element.Events as Events
import Html.Attributes exposing (property)
import Json.Encode


type alias Config msg draggable droppable =
    { started : draggable -> msg
    , dragged : draggable -> droppable -> msg
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


start : State draggable droppable -> draggable -> State draggable droppable
start state dragged =
    { state | dragged = Just dragged }


drag : State draggable droppable -> droppable -> State draggable droppable
drag state dropTarget =
    { state | dropTarget = Just dropTarget }


stop : State draggable droppable -> State draggable droppable
stop state =
    init


makeDraggable : State draggable droppable -> Config msg draggable droppable -> draggable -> Element msg -> Element msg
makeDraggable state config dragged elem =
    Element.el [ Element.width Element.fill, Element.height Element.fill, Element.htmlAttribute <| property "draggable" (Json.Encode.bool True), Events.onDragStart (config.started dragged) ] elem


makeDroppable : State draggable droppable -> Config msg draggable droppable -> droppable -> Element msg -> Element msg
makeDroppable state msgs dropTarget el =
    Debug.todo "impl"
