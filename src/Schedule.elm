module Schedule exposing (Reservation, ReservationId(..), Resource, ResourceId(..), Schedule, getReservations, getResource, getResourceName, mapReservations, newResource, newSchedule)

import Duration as Duration exposing (Duration)
import Primitives exposing (TimeWindow(..))
import Time exposing (Posix)


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
