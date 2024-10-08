module Z80Tape exposing (..)

import Dict exposing (Dict)
import Tapfile exposing (Tapfile)


type alias TapePosition =
    { position : Int
    , tapfileNumber : Int
    }


newPosition : TapePosition
newPosition =
    TapePosition 0 0


type alias Z80Tape =
    { tapePos : TapePosition
    , tapfiles : Dict Int Tapfile
    }
