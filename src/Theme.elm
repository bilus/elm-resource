module Theme exposing (Theme, defaultTheme)

import Array exposing (Array)
import Color
import Element exposing (Color, rgba)
import Element.Font as Font exposing (Font)


type alias Theme =
    { defaultCell : { heightPx : Int, widthPx : Int }
    , columns : Array { bgColor : Color }
    , header : { fontFamily : List Font, fontSize : Int }
    , cells : { fontFamily : List Font, fontSize : Int }
    }


defaultTheme : Theme
defaultTheme =
    let
        columns =
            [ Color.orange
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
            , Color.darkOrange
            , Color.darkYellow
            , Color.darkGreen
            , Color.darkBlue
            , Color.darkPurple
            , Color.darkBrown
            ]
                |> List.map toColor
                |> List.map (\color -> { bgColor = color })
                |> Array.fromList
    in
    { defaultCell = { heightPx = 30, widthPx = 200 }
    , columns = columns
    , header = { fontFamily = [ Font.typeface "Helvetica" ], fontSize = 20 }
    , cells = { fontFamily = [ Font.typeface "Helvetica" ], fontSize = 20 }
    }


toColor : Color.Color -> Color
toColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgba color
    in
    rgba red green blue alpha
