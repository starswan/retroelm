module Z80Change exposing (..)

import CpuTimeCTime exposing (CpuTimeIncrement)
import Z80Env exposing (addCpuTimeEnvInc, setMem)
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Types exposing (Z80)


type Z80Change
    = FlagsWithBRegister IntWithFlags
    | FlagsWithCRegister IntWithFlags
    | FlagsWithDRegister IntWithFlags
    | FlagsWithERegister FlagRegisters Int
    | HLRegister Int
    | FlagsWithHLRegister FlagRegisters Int CpuTimeIncrement
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
        FlagsWithBRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | b = intWithFlags.value } }

        FlagsWithCRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | c = intWithFlags.value } }

        FlagsWithDRegister intWithFlags ->
            let
                main =
                    z80.main
            in
            { z80 | flags = intWithFlags.flags, main = { main | d = intWithFlags.value } }

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

