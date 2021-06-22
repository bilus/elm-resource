module Connection exposing (Connection, Kind(..))

-- import Sheet exposing (CellRef, Sheet, cellWindow)

import Schedule exposing (ResourceId)
import Time exposing (Posix)


type alias Connection d =
    { fromResource : ResourceId
    , fromTime : Posix
    , toResource : ResourceId
    , toTime : Posix
    , data : d
    , notes : String
    , kind : Kind
    }


type Kind
    = Strong
    | Weak
    | Blocked



-- ends : Sheet -> Connection d -> Maybe ( CellRef, CellRef )
-- ends sheet connection =
--     let
--         from =
--             cellRef sheet connection.fromResource connection.fromTime
--         to =
--             cellRef sheet connection.toResource connection.toTime
--     in
--     Maybe.map2 (\f t -> ( f, t ))
--         from
--         to
-- cellRef : Sheet -> ResourceId -> Posix -> Maybe CellRef
-- cellRef sheet resourceId t =
--     let
--         pred =
--             \cell ->
--                 let
--                     window =
--                         cellwindow
--                 in
--                 TimeWindow.includes t
--     in
--     Nothing
