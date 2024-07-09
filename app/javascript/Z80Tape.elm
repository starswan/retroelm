module Z80Tape exposing (..)

import Dict exposing (Dict)
import Tapfile exposing (Tapfile)

type alias Z80Tape =
    { tapfiles : Dict Int Tapfile
    }

