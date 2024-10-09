module Z80Change exposing (..)

import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (Z80)


type Z80Change
    = OnlyFlags FlagRegisters
    | FlagsWithBRegister FlagRegisters Int
    | FlagsWithCRegister FlagRegisters Int
    | FlagsWithDRegister FlagRegisters Int
    | FlagsWithERegister FlagRegisters Int
    | HLRegister Int
    | FlagsWithHLRegister FlagRegisters Int


applyZ80Change : Z80Change -> Z80 -> Z80
applyZ80Change change z80 =
    case change of
        OnlyFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        FlagsWithBRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | b = int } }

        FlagsWithCRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | c = int } }

        FlagsWithDRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | d = int } }

        FlagsWithERegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | e = int } }

        HLRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = int } }

        FlagsWithHLRegister flagRegisters int ->
            let
                main =
                    z80.main
            in
            { z80 | flags = flagRegisters, main = { main | hl = int } }


