

module Z80Byte exposing (..)


type Nybble
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen


type Z80Byte
    = Z80Byte Nybble Nybble
