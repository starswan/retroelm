module RegisterChange exposing (..)

import Z80Address exposing (Z80Address)
import Z80Types exposing (Z80)
type RegisterChange
    = ChangeRegisterC Int
    | ChangeRegisterBC Int Int
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int
    | ChangeRegisterE Int
    | ChangeRegisterHL Z80Address

applyRegisterChange : RegisterChange -> Z80 -> Z80
applyRegisterChange change z80 =
    case change of
        ChangeRegisterC int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }

        ChangeRegisterBC b_value c_value ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = b_value, c = c_value } }

        ChangeRegisterDE d_value e_value ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | d = d_value, e = e_value } }

        ChangeRegisterE int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | e = int } }

        ChangeRegisterHL int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = int } }

        ChangeRegisterB int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }

