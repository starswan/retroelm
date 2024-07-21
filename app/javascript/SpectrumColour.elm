module SpectrumColour exposing (..)

import Dict
import Maybe exposing (withDefault)


type InkPaperColour
    = Black
    | Blue
    | Red
    | Magenta
    | Green
    | Cyan
    | Yellow
    | White


type alias SpectrumColour =
    { colour : InkPaperColour
    , bright : Bool
    }

plainGreen = SpectrumColour Green False
brightGreen = SpectrumColour Green True
plainWhite = SpectrumColour White False
--brightWhite = SpectrumColour White True
--plainBlue = SpectrumColour Blue False
plainBlack = SpectrumColour Black False
plainMagenta = SpectrumColour Magenta False
brightMagenta = SpectrumColour Magenta True



--= Black
--| Blue
--| Red
--| Magenta
--| Green
--| Cyan
--| Yellow
--| White
--| BrightBlue
--| BrightRed
--| BrightMagenta
--| BrightGreen
--| BrightCyan
--| BrightYellow
--| BrightWhite


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


spectrumColour : Int -> Bool -> SpectrumColour
spectrumColour value bright =
    SpectrumColour (Dict.get value spectrumColours |> withDefault White) bright

colourToString : SpectrumColour -> String
colourToString colour =
    let
        brightVersion =
            case colour.colour of
                Black ->
                    c_BLACK

                Blue ->
                    c_BLUE

                Red ->
                    c_RED

                Magenta ->
                    c_MAGENTA

                Green ->
                    c_GREEN

                Cyan ->
                    c_CYAN

                Yellow ->
                    c_YELLOW

                White ->
                    c_WHITE
    in
    if colour.bright then
        brightVersion

    else
        brightVersion |> String.replace "FF" c_UNBRIGHT
