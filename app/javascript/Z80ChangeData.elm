module Z80ChangeData exposing (..)

import RegisterChange exposing (RegisterChange)
import Z80Change exposing (Z80Change)
type alias Z80ChangeData =
    {
        cpu_time: Int
        , changes: Z80Change
        , pc_change: Int
    }

type alias RegisterChangeData =
        {
            cpu_time: Int
            , changes: RegisterChange
            , pc_change: Int
        }