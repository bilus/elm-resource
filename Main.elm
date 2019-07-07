module Main exposing (main)

import Browser
import Duration as Duration exposing (Duration)
import Element exposing (..)
import Element.Background as Background
import Html exposing (Html)
import Html.Attributes exposing (style)
import Iso8601
import Time exposing (Posix)


type Page
    = InputPage


type Msg
    = NoOp


type alias Model =
    { currPage : Page
    , timetable : List Schedule
    , timetableConfig : TimetableConfig
    }


type alias Flags =
    ()


type Resource
    = Resource { id : ResourceId, name : String }


newResource : ResourceId -> String -> Resource
newResource id name =
    Resource { id = id, name = name }


type ResourceId
    = ResourceId String


type Reservation
    = Reservation { id : ReservationId, start : Posix, duration : Duration }


type ReservationId
    = ReservationId String


type Schedule
    = Schedule Resource (List Reservation)


type alias TimetableConfig =
    { start : Posix
    , duration : Duration
    }


sampleTimetable =
    [ Schedule
        (newResource (ResourceId "id1") "ZS 672AE")
        []
    , Schedule
        (newResource (ResourceId "id1") "ZS 8127S")
        []
    ]


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , timetable = sampleTimetable
      , timetableConfig = { start = Time.millisToPosix 1562533510000, duration = Duration.hours 24 }
      }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "VisExp"
    , body =
        [ layout [] <| viewTimetable model.timetableConfig model.timetable
        ]
    }


viewTimetable : TimetableConfig -> List Schedule -> Element Msg
viewTimetable timetableConfig timetable =
    row [ height fill ]
        [ column [ width fill ]
            [ row [ height fill ]
                [ text "Hello world" ]
            , row
                [ height fill, width fill ]
              <|
                viewOffsetCol
                    :: List.map (viewScheduleCol timetableConfig) timetable
            ]
        ]


viewOffsetCol : Element Msg
viewOffsetCol =
    let
        offsetRows =
            dayTimeOffsets
                |> List.map (\offset -> row [ alignRight ] [ text <| formatOffset offset ])
    in
    column [ inFront <| stickyHeader "Czas" ] <| stickyHeader "Czas" :: offsetRows


sticky : List (Attribute msg)
sticky =
    List.map htmlAttribute [ style "position" "sticky", style "top" "0" ]


stickyHeader : String -> Element Msg
stickyHeader title =
    row
        ([ width fill
         , Background.color <| rgba 0.8 0.8 0.8 0.8
         , padding 3
         ]
            ++ sticky
        )
        [ text title
        ]


viewScheduleCol : TimetableConfig -> Schedule -> Element Msg
viewScheduleCol timetableConfig (Schedule (Resource { name }) reservations) =
    column [ height fill, width fill, inFront <| stickyHeader name ] <|
        [ stickyHeader name -- Repeat to provide padding for stickyHeader ^
        , row [] [ viewReservationCol timetableConfig reservations ]
        ]


viewReservationCol : TimetableConfig -> List Reservation -> Element Msg
viewReservationCol timetableConfig reservations =
    let
        timeslots =
            timeslotsFrom timetableConfig reservations
    in
    column [] <|
        List.map viewTimeslotRow timeslots


type Timeslot
    = EmptyTimeslot { offset : Duration }
    | ReservedTimeslot { offset : Duration, reservations : List Reservation }


timeslotFromReservation : Reservation -> Timeslot
timeslotFromReservation reservation =
    EmptyTimeslot { offset = Duration.minutes 0 }


mergeTimeslots : List Timeslot -> List Timeslot -> List Timeslot
mergeTimeslots xs ys =
    xs


dayTimeOffsets : List Duration
dayTimeOffsets =
    let
        offsets =
            List.range 0 47
                |> List.map ((*) 30 >> toFloat)
    in
    offsets
        |> List.map Duration.minutes


timeslotsFrom : TimetableConfig -> List Reservation -> List Timeslot
timeslotsFrom { start, duration } reservations =
    let
        empties =
            dayTimeOffsets
                |> List.map (\offset -> EmptyTimeslot { offset = offset })

        reservies =
            reservations
                |> List.map timeslotFromReservation
    in
    mergeTimeslots empties reservies


viewTimeslotRow : Timeslot -> Element Msg
viewTimeslotRow timeslot =
    case timeslot of
        EmptyTimeslot { offset } ->
            row [] []

        ReservedTimeslot { offset } ->
            row [] [ text <| formatOffset offset ]


formatOffset : Duration -> String
formatOffset offset =
    let
        hours =
            Duration.inHours offset

        fullHours =
            floor hours

        minutes =
            60 * (hours - toFloat fullHours) |> round
    in
    String.fromInt fullHours ++ ":" ++ String.fromInt minutes


loremParagraph =
    paragraph [] [ text "Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci commodo lobortis.  Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.  Sed diam.  Praesent fermentum tempor tellus.  Nullam tempus.  Mauris ac felis vel velit tristique imperdiet.  Donec at pede.  Etiam vel neque nec dui dignissim bibendum.  Vivamus id enim.  Phasellus neque orci, porta a, aliquet quis, semper a, massa.  Phasellus purus.  Pellentesque tristique imperdiet tortor.  Nam euismod tellus id erat.\n\nNullam eu ante vel est convallis dignissim.  Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.  Nunc porta vulputate tellus.  Nunc rutrum turpis sed pede.  Sed bibendum.  Aliquam posuere.  Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.  Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.  Curabitur vulputate vestibulum lorem.  Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros.  Sed id ligula quis est convallis tempor.  Curabitur lacinia pulvinar nibh.  Nam a sapien.\n\n", text "Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci commodo lobortis.  Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.  Sed diam.  Praesent fermentum tempor tellus.  Nullam tempus.  Mauris ac felis vel velit tristique imperdiet.  Donec at pede.  Etiam vel neque nec dui dignissim bibendum.  Vivamus id enim.  Phasellus neque orci, porta a, aliquet quis, semper a, massa.  Phasellus purus.  Pellentesque tristique imperdiet tortor.  Nam euismod tellus id erat.\n\nNullam eu ante vel est convallis dignissim.  Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.  Nunc porta vulputate tellus.  Nunc rutrum turpis sed pede.  Sed bibendum.  Aliquam posuere.  Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.  Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.  Curabitur vulputate vestibulum lorem.  Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros.  Sed id ligula quis est convallis tempor.  Curabitur lacinia pulvinar nibh.  Nam a sapien.\n\n", text "Aliquam erat volutpat.  Nunc eleifend leo vitae magna.  In id erat non orci commodo lobortis.  Proin neque massa, cursus ut, gravida ut, lobortis eget, lacus.  Sed diam.  Praesent fermentum tempor tellus.  Nullam tempus.  Mauris ac felis vel velit tristique imperdiet.  Donec at pede.  Etiam vel neque nec dui dignissim bibendum.  Vivamus id enim.  Phasellus neque orci, porta a, aliquet quis, semper a, massa.  Phasellus purus.  Pellentesque tristique imperdiet tortor.  Nam euismod tellus id erat.\n\nNullam eu ante vel est convallis dignissim.  Fusce suscipit, wisi nec facilisis facilisis, est dui fermentum leo, quis tempor ligula erat quis odio.  Nunc porta vulputate tellus.  Nunc rutrum turpis sed pede.  Sed bibendum.  Aliquam posuere.  Nunc aliquet, augue nec adipiscing interdum, lacus tellus malesuada massa, quis varius mi purus non odio.  Pellentesque condimentum, magna ut suscipit hendrerit, ipsum augue ornare nulla, non luctus diam neque sit amet urna.  Curabitur vulputate vestibulum lorem.  Fusce sagittis, libero non molestie mollis, magna orci ultrices dolor, at vulputate neque nulla lacinia eros.  Sed id ligula quis est convallis tempor.  Curabitur lacinia pulvinar nibh.  Nam a sapien.\n\n" ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
