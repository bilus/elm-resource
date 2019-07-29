module Schedule exposing (Reservation(..), ReservationId(..), Resource, ResourceId(..), Schedule, getReservationId, getReservations, getResource, getResourceName, getWindow, isConflict, isReservationOverlapping, mapReservations, moveReservationEnd, moveReservationStart, newReservation, newResource, newSchedule, sortReservations)

import Duration as Duration exposing (Duration)
import List.Extra
import Time exposing (Posix)
import TimeWindow exposing (TimeWindow, make)


type ResourceId
    = ResourceId String


type Resource
    = Resource { id : ResourceId, name : String }


newResource : ResourceId -> String -> Resource
newResource id name =
    Resource { id = id, name = name }


getResourceName : Resource -> String
getResourceName (Resource { name }) =
    name


type Reservation
    = Reservation { id : ReservationId, window : TimeWindow }


newReservation : ReservationId -> Posix -> Duration -> Reservation
newReservation id start duration =
    Reservation { id = id, window = make start duration }


isReservationOverlapping : TimeWindow -> Reservation -> Bool
isReservationOverlapping otherWindow (Reservation { window }) =
    TimeWindow.overlaps otherWindow window


isConflict : Reservation -> Reservation -> Bool
isConflict (Reservation r1) (Reservation r2) =
    TimeWindow.overlaps r1.window r2.window


sortReservations : List Reservation -> List Reservation
sortReservations =
    let
        compareStart (Reservation r1) (Reservation r2) =
            TimeWindow.compare r1.window r2.window
    in
    List.Extra.stableSortWith compareStart


getWindow : Reservation -> TimeWindow
getWindow (Reservation { window }) =
    window


moveReservationStart : Posix -> Reservation -> Reservation
moveReservationStart newStart (Reservation { id, window }) =
    Reservation { id = id, window = window |> TimeWindow.moveStart newStart }


moveReservationEnd : Posix -> Reservation -> Reservation
moveReservationEnd newEnd (Reservation { id, window }) =
    Reservation { id = id, window = window |> TimeWindow.moveEnd newEnd }


getReservationId : Reservation -> ReservationId
getReservationId (Reservation { id }) =
    id


type ReservationId
    = ReservationId String


type Schedule
    = Schedule Resource (List Reservation)


newSchedule : Resource -> List Reservation -> Schedule
newSchedule =
    Schedule


getReservations : Schedule -> List Reservation
getReservations (Schedule _ reservations) =
    reservations


getResource : Schedule -> Resource
getResource (Schedule resource _) =
    resource


mapReservations : (Reservation -> Reservation) -> Schedule -> Schedule
mapReservations f (Schedule resource reservations) =
    Schedule resource (List.map f reservations)
