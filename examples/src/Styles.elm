module Styles
    exposing
        ( Styles(..)
        , stylesheet
        )

import Color exposing (rgba)
import Style exposing (Property, StyleSheet, cursor, hover, style)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow


type Styles
    = None
    | Title
    | Edit
    | Field
    | Error
    | Button
    | Table
    | TableHeader
    | TableCell
    | FormGroupTitle


colors =
    { reactiveGreen = Color.rgb 85 175 106
    , navbarBackground = Color.darkCharcoal
    , mainBackground = Color.black
    }


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style Title
            [ Color.text Color.darkGrey
            , Font.size 30
            ]
        , Style.style Edit
            [ Color.text (Color.rgba 0 0 0 0.6)
            , Font.size 30
            ]
        , style Field
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightGrey
            ]
        , style Error
            [ Color.text Color.red
            ]
        , style Button
            [ Border.rounded 3
            , Font.size 16
            , Border.all 1
            , Border.solid
            , Color.background Color.darkGray
            ]
        , style Table
            [ Shadow.box
                { offset = ( 0, 2 )
                , size = 2
                , blur = 2
                , color = Color.rgba 0 0 0 0.14
                }
            , Color.background Color.white
            , Border.all 1
            , Border.solid
            , Color.border (Color.rgba 0 0 0 0.12)
            , Font.size 12
            ]
        , style TableHeader
            [ Color.text (Color.rgba 0 0 0 0.54)
            , Font.weight 700
            ]
        , style TableCell
            [ Color.text (Color.rgba 0 0 0 0.87)
            , Font.weight 400
            , Font.size 13
            , cursor "pointer"
            ]
        , style FormGroupTitle
            [ Font.size 16
            , Font.weight 700
            ]
        ]
