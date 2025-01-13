module SpectrumColour exposing (..)

import Dict
import Maybe exposing (withDefault)


type SpectrumColour
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
        [ ( 0, Black )
        , ( 1, Blue )
        , ( 2, Red )
        , ( 3, Magenta )
        , ( 4, Green )
        , ( 5, Cyan )
        , ( 6, Yellow )
        , ( 7, White )
        ]


spectrumBrightColours =
    Dict.fromList
        [ ( 0, Black )
        , ( 1, BrightBlue )
        , ( 2, BrightRed )
        , ( 3, BrightMagenta )
        , ( 4, BrightGreen )
        , ( 5, BrightCyan )
        , ( 6, BrightYellow )
        , ( 7, BrightWhite )
        ]


spectrumColour : Int -> Bool -> SpectrumColour
spectrumColour value bright =
    if bright then
        Dict.get value spectrumBrightColours |> withDefault BrightWhite

    else
        Dict.get value spectrumColours |> withDefault White


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


colourToString : SpectrumColour -> String
colourToString colour =
    case colour of
        Black ->
            c_BLACK

        Blue ->
            c_UNBRIGHT_BLUE

        Red ->
            c_UNBRIGHT_RED

        Magenta ->
            c_UNBRIGHT_MAGENTA

        Green ->
            c_UNBRIGHT_GREEN

        Cyan ->
            c_UNBRIGHT_CYAN

        Yellow ->
            c_UNBRIGHT_YELLOW

        White ->
            c_UNBRIGHT_WHITE

        BrightBlue ->
            c_BLUE

        BrightRed ->
            c_RED

        BrightMagenta ->
            c_MAGENTA

        BrightGreen ->
            c_GREEN

        BrightCyan ->
            c_CYAN

        BrightYellow ->
            c_YELLOW

        BrightWhite ->
            c_WHITE
