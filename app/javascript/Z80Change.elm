module Z80Change exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement)
import Utils exposing (shiftLeftBy8)
import Z80Env exposing (addCpuTimeEnvInc, setMem)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (Z80)


type Z80Change
    = FlagsWithBRegister FlagRegisters Int
    | FlagsWithCRegister FlagRegisters Int
    | FlagsWithDRegister FlagRegisters Int
    | FlagsWithERegister FlagRegisters Int
    | FlagsWithHLRegister FlagRegisters Int CpuTimeIncrement
    | FlagsWithHRegister FlagRegisters Int
    | FlagsWithLRegister FlagRegisters Int
    | Z80RegisterB Int
    | Z80RegisterC Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int CpuTimeIncrement


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChangeB Int
    | FlagChangeC Int
    | FlagChangeD Int
    | FlagChangeE Int
    | FlagChangeH Int
    | FlagChangeL Int
    | ReturnWithPop CpuTimeIncrement
    | EmptyFlagChange CpuTimeIncrement
    | FlagChangePush Int


applyZ80Change : Z80Change -> Z80 -> Z80
applyZ80Change change z80 =
    case change of
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

        FlagsWithHLRegister flagRegisters int time ->
            let
                main =
                    z80.main

                env =
                    z80.env |> addCpuTimeEnvInc time
            in
            { z80 | env = env, flags = flagRegisters, main = { main | hl = int } }

        Z80RegisterB int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }

        Z80RegisterC int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }

        Z80ChangeFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        Z80ChangeSetIndirect addr int time ->
            let
                env = z80.env |> setMem addr int |> addCpuTimeEnvInc time
            in
            { z80 | env = env }

        FlagsWithHRegister flagRegisters int ->
            let
                main =
                    z80.main
                new_hl =
                    Bitwise.or (int |> shiftLeftBy8) (Bitwise.and main.hl 0xFF)
            in
            { z80 | flags = flagRegisters, main = { main | hl = new_hl } }

        FlagsWithLRegister flagRegisters int ->
            let
                main =
                    z80.main
                new_hl =
                    Bitwise.or int (Bitwise.and main.hl 0xFF00)
            in
            { z80 | flags = flagRegisters, main = { main | hl = new_hl } }


