module SpectrumColour exposing (..)

import Dict
import Maybe exposing (withDefault)


type SpectrumColourValue
    = Black
    | Blue
    | Red
    | Magenta
    | Green
    | Cyan
    | Yellow
    | White
    | BrightBlue
    | BrightRed
    | BrightMagenta
    | BrightGreen
    | BrightCyan
    | BrightYellow
    | BrightWhite


type alias SpectrumColour =
    { value : SpectrumColourValue
    , colour : String
    }


c_BLACK =
    "#000000"


c_BLUE =
    "#0000FF"


c_RED =
    "#FF0000"


c_MAGENTA =
    "#FF00FF"


c_GREEN =
    "#00FF00"


c_CYAN =
    "#00FFFF"


c_YELLOW =
    "#FFFF00"


c_WHITE =
    "#FFFFFF"


c_UNBRIGHT =
    "D7"


spectrumColours =
    Dict.fromList
        [ ( 0, { value = Black, colour = c_BLACK } )
        , ( 1, { value = Blue, colour = c_UNBRIGHT_BLUE } )
        , ( 2, { value = Red, colour = c_UNBRIGHT_RED } )
        , ( 3, { value = Magenta, colour = c_UNBRIGHT_MAGENTA } )
        , ( 4, { value = Green, colour = c_UNBRIGHT_GREEN } )
        , ( 5, { value = Cyan, colour = c_UNBRIGHT_CYAN } )
        , ( 6, { value = Yellow, colour = c_UNBRIGHT_YELLOW } )
        , ( 7, { value = White, colour = c_UNBRIGHT_WHITE } )
        ]


spectrumBrightColours =
    Dict.fromList
        [ ( 0, { value = Black, colour = c_BLACK } )
        , ( 1, { value = BrightBlue, colour = c_BLUE } )
        , ( 2, { value = BrightRed, colour = c_RED } )
        , ( 3, { value = BrightMagenta, colour = c_MAGENTA } )
        , ( 4, { value = BrightGreen, colour = c_GREEN } )
        , ( 5, { value = BrightCyan, colour = c_CYAN } )
        , ( 6, { value = BrightYellow, colour = c_YELLOW } )
        , ( 7, { value = BrightWhite, colour = c_WHITE } )
        ]


spectrumColour : Int -> Bool -> SpectrumColour
spectrumColour value bright =
    let
        x =
            if bright then
                Dict.get value spectrumBrightColours |> withDefault { value = White, colour = c_UNBRIGHT_WHITE }

            else
                Dict.get value spectrumColours |> withDefault { value = White, colour = c_UNBRIGHT_WHITE }
    in
    x


c_UNBRIGHT_BLUE =
    c_BLUE |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_RED =
    c_RED |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_MAGENTA =
    c_MAGENTA |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_GREEN =
    c_GREEN |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_CYAN =
    c_CYAN |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_YELLOW =
    c_YELLOW |> String.replace "FF" c_UNBRIGHT


c_UNBRIGHT_WHITE =
    c_WHITE |> String.replace "FF" c_UNBRIGHT
