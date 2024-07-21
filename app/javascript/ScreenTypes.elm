module ScreenTypes exposing (..)

-- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink

import SpectrumColour exposing (InkPaperColour, SpectrumColour)


type alias RawScreenData =
    { colour : Int
    , data : Int
    }



-- line definition - length colour (3 bits) and brightness


type alias ScreenColourRun =
    { start : Int
    , length : Int
    , colour : SpectrumColour
    }


type alias PaperInkBrightFlash =
    { paper : InkPaperColour
    , ink : InkPaperColour
    , bright : Bool
    , flash : Bool
    , dataByte : Int
    }

type alias PaperInkBrightFlashBool =
    { paper : InkPaperColour
    , ink : InkPaperColour
    , bright : Bool
    , flash : Bool
    , bit : Bool
    }
