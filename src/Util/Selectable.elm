module Util.Selectable exposing
    ( Selectable
    , fromList, append, cons, toList
    , choose, chooseIf, first, current, prior, discard, isCurrent, undo, any
    , map, mapWithState, mapCurrent, indexedMap, indexedMapWithState
    , filter, removeIf, updateIf, head
    , deselect
    )

{-| This module implements a list with selectable elements. It's similar to zippers
except each element can have multiple states not just one (example selected and
hovered over).

It uses lenses to define accessor for the various states. For example:

        selected =
            Lens .selected (\x m -> { m | selected = x })

        focused =
            Lens .focused (\x m -> { m | focused = x })

        elements =
            LS.fromList
                [ "lorem", "ipsum", "quid" ]
                { selected = False, focused = False }

        elements
            |> LS.current selected
            |> Maybe.withDefault "nothing"
            |> Debug.log "Selected: "
            -- Selected: nothing

        elements
            |> LS.choose "lorem" selected
            |> LS.current selected
            |> Maybe.withDefault "nothing"
            |> Debug.log "Selected: "
            -- Selected: lorem

IMPORTANT: Requires list elements to be unique because selection is based on their
value.

Internally, it supports multi-selection but the functions in the module work in
single-selection mode.


# Selectable type

@docs Selectable


# Construction & conversion

@docs fromList, append, cons, toList


# Single selection

@docs choose, chooseIf, first, current, prior, discard, isCurrent, undo, any


# Mapping

@docs map, mapWithState, mapCurrent, indexedMap, indexedMapWithState


# Manipulation

@docs filter, removeIf, updateIf, head

-}

import Monocle.Lens exposing (Lens)


type alias Elem s a =
    { state : s
    , priorState : s
    , value : a
    }


{-| Represents a selectable list.
-}
type alias Selectable s a =
    { elements : List (Elem s a)
    , defaultState : s
    }


{-| Constructs a selectable list from a regular one.
-}
fromList : s -> List a -> Selectable s a
fromList defaultState xs =
    { elements =
        List.map
            (\x ->
                { state = defaultState
                , priorState = defaultState
                , value = x
                }
            )
            xs
    , defaultState = defaultState
    }


{-| Cons an element to a selectable list.
-}
cons : a -> Selectable s a -> Selectable s a
cons x list =
    let
        newEl =
            { state = list.defaultState
            , priorState = list.defaultState
            , value = x
            }
    in
    { list
        | elements = newEl :: list.elements
    }


{-| Appends one selectable list to another.

Keeps the default state of the lhs list.

-}
append : Selectable s a -> Selectable s a -> Selectable s a
append xs ys =
    { xs
        | elements = xs.elements ++ ys.elements
    }


{-| Converts a selectable list to a regular one.
-}
toList : Selectable s a -> List a
toList =
    List.map .value << .elements



-- TODO: chooseIf so we can select pages by their ID instead of full value (which is brittle)
-- TODO: Move lens as first argument whenever it's present, then list (that also includes f: lens BEFORE f)


{-| Selects `x` through the lens.

Saves selection so it's available to `undo` and `prior` functions.

-}
choose : a -> Lens s Bool -> Selectable s a -> Selectable s a
choose x lens list =
    if List.member x (toList list) then
        unsafeChooseSingle (Just x) lens list

    else
        list


{-| Same as choose but selects by predicate.
-}
chooseIf : (a -> Bool) -> Lens s Bool -> Selectable s a -> Selectable s a
chooseIf pred lens list =
    let
        elem =
            list
                |> filter pred
                |> head
    in
    case elem of
        Just e ->
            choose e lens list

        _ ->
            list


deselect : Lens s Bool -> Selectable s a -> Selectable s a
deselect lens list =
    unsafeDeselectAll lens list


{-| Selects the first element through the lens.
-}
first : Lens s Bool -> Selectable s a -> Selectable s a
first lens list =
    case List.head <| toList list of
        Just element ->
            choose element lens list

        Nothing ->
            list


{-| Returns the element selected through the lens or `Nothing` if none is
selected.
-}
current : Lens s Bool -> Selectable s a -> Maybe a
current lens list =
    list.elements
        |> List.filter (lens.get << .state)
        |> List.head
        |> Maybe.map .value


{-| Returns True if `x` is selected through the lens.
-}
isCurrent : a -> Lens s Bool -> Selectable s a -> Bool
isCurrent x lens list =
    current lens list == Just x


{-| Returns the element previously selected through the lens or `Nothing`
if no previous selection.
-}
prior : Lens s Bool -> Selectable s a -> Maybe a
prior lens list =
    list.elements
        |> List.filter (lens.get << .priorState)
        |> List.head
        |> Maybe.map .value


{-| Reverts selection to the previous state.

When applied repeatedly, it will toggle between two states.

-}
undo : Lens s Bool -> Selectable s a -> Selectable s a
undo lens list =
    list
        |> mapElems
            (\_ element ->
                { element
                    | state = copyState lens element.priorState element.state
                    , priorState = copyState lens element.state element.priorState
                }
            )


{-| Clear selection.

Saves selection so it's available to `undo` and `prior` functions.

-}
discard : Lens s Bool -> Selectable s a -> Selectable s a
discard lens list =
    unsafeChooseSingle Nothing lens list


{-| Returns True if there's an element selected through the lens.
-}
any : Lens s Bool -> Selectable s a -> Bool
any lens list =
    current lens list /= Nothing


{-| Apply the function to every element in the selectable list.
-}
map : (a -> b) -> Selectable s a -> Selectable s b
map f list =
    list
        |> mapElems
            (\_ { value, state, priorState } ->
                { value = f value
                , state = state
                , priorState = priorState
                }
            )


{-| Apply the function to index of every element and the element itself in the selectable list.
-}
indexedMap : (Int -> a -> b) -> Selectable s a -> Selectable s b
indexedMap f list =
    list
        |> mapElems
            (\index { value, state, priorState } ->
                { value = f index value
                , state = state
                , priorState = priorState
                }
            )


{-| Apply the function to index of every element in the selectable list, element itself and its state.
-}
indexedMapWithState : (Int -> a -> s -> b) -> Selectable s a -> Selectable s b
indexedMapWithState f list =
    list
        |> mapElems
            (\index { value, state, priorState } ->
                { value = f index value state
                , state = state
                , priorState = priorState
                }
            )


{-| Apply the function to every element in the selectable list and its state.
-}
mapWithState : (a -> s -> b) -> Selectable s a -> Selectable s b
mapWithState f list =
    list
        |> mapElems
            (\_ { value, state, priorState } ->
                { value = f value state
                , state = state
                , priorState = priorState
                }
            )


{-| Applies the function to element(s) selected through the lens.
-}
mapCurrent : (a -> a) -> Lens s Bool -> Selectable s a -> Selectable s a
mapCurrent f lens list =
    list
        |> mapElems
            (\_ element ->
                if lens.get element.state then
                    { value = f element.value
                    , state = element.state
                    , priorState = element.priorState
                    }

                else
                    element
            )


{-| Filters elements using the predicate.
-}
filter : (a -> Bool) -> Selectable s a -> Selectable s a
filter pred list =
    { list
        | elements = List.filter (pred << .value) list.elements
    }


{-| Applies the function to elements matching the predicate.
-}
updateIf : (a -> Bool) -> (a -> a) -> Selectable s a -> Selectable s a
updateIf pred f list =
    list
        |> map
            (\x ->
                if pred x then
                    f x

                else
                    x
            )


{-| Filters out all elements matching the predicate.
-}
removeIf : (a -> Bool) -> Selectable s a -> Selectable s a
removeIf pred =
    filter (not << pred)


{-| Extract the first element of the selectable list.
-}
head : Selectable s a -> Maybe a
head =
    List.head << toList



--
-- Internal helpers.
--


mapElems : (Int -> Elem s a -> Elem s b) -> Selectable s a -> Selectable s b
mapElems f list =
    let
        newItems =
            list.elements
                |> List.indexedMap f
    in
    { elements = newItems
    , defaultState = list.defaultState
    }


unsafeChooseSingle : Maybe a -> Lens s Bool -> Selectable s a -> Selectable s a
unsafeChooseSingle x lens list =
    list
        |> mapElems
            (\_ element ->
                { element
                    | state = lens.set (Just element.value == x) element.state
                    , priorState = copyState lens element.state element.priorState
                }
            )


unsafeDeselectAll : Lens s Bool -> Selectable s a -> Selectable s a
unsafeDeselectAll lens list =
    list
        |> mapElems
            (\_ element ->
                { element
                    | state = lens.set False element.state
                    , priorState = copyState lens element.state element.priorState
                }
            )


copyState : Lens s Bool -> s -> s -> s
copyState lens source target =
    lens.set (lens.get source) target
