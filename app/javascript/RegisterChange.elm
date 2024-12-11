module RegisterChange exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement)
import Utils exposing (shiftLeftBy8)
import Z80Address exposing (Z80Address, fromInt, lower8Bits, top8BitsWithoutShift)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (MainWithIndexRegisters, Z80, set_de_main)


type RegisterChange
    = ChangeRegisterCWithTime Int CpuTimeIncrement
    | ChangeRegisterBC Int Int CpuTimeIncrement
    | ChangeRegisterB Int
    | ChangeRegisterDE Int Int CpuTimeIncrement
    | ChangeRegisterEWithTime Int CpuTimeIncrement
    | ChangeRegisterE Int
    | ChangeRegisterHL Z80Address CpuTimeIncrement
    | ChangeRegisterIX Z80Address CpuTimeIncrement
    | ChangeRegisterIY Z80Address CpuTimeIncrement
    | ChangeRegisterD Int
    | ChangeRegisterA Int
    | ChangeRegisterC Int
    | ChangeRegisterH Int
    | ChangeRegisterL Int
    | PushedValue Z80Address
    | RegChangeNewSP Z80Address CpuTimeIncrement
    | IncrementIndirect Z80Address CpuTimeIncrement
    | DecrementIndirect Z80Address CpuTimeIncrement
    | RegisterChangeJump Z80Address
    | SetIndirect Z80Address Int CpuTimeIncrement
    | ChangeRegisterDEAndHL Int Z80Address
    | Shifter0 Z80Address CpuTimeIncrement
    | Shifter1 Z80Address CpuTimeIncrement
    | Shifter2 Z80Address CpuTimeIncrement
    | Shifter3 Z80Address CpuTimeIncrement
    | Shifter4 Z80Address CpuTimeIncrement
    | Shifter5 Z80Address CpuTimeIncrement
    | Shifter6 Z80Address CpuTimeIncrement
    | Shifter7 Z80Address CpuTimeIncrement


type RegisterChangeApplied
    = MainRegsApplied MainWithIndexRegisters
    | MainRegsWithTimeApplied MainWithIndexRegisters CpuTimeIncrement
    | FlagRegsApplied FlagRegisters
    | PushedValueApplied Z80Address
    | NewSPApplied Z80Address CpuTimeIncrement
    | IncrementIndirectApplied Z80Address CpuTimeIncrement
    | DecrementIndirectApplied Z80Address CpuTimeIncrement
    | JumpApplied Z80Address
    | SetIndirectApplied Z80Address Int CpuTimeIncrement
    | Shifter0Applied Z80Address CpuTimeIncrement
    | Shifter1Applied Z80Address CpuTimeIncrement
    | Shifter2Applied Z80Address CpuTimeIncrement
    | Shifter3Applied Z80Address CpuTimeIncrement
    | Shifter4Applied Z80Address CpuTimeIncrement
    | Shifter5Applied Z80Address CpuTimeIncrement
    | Shifter6Applied Z80Address CpuTimeIncrement
    | Shifter7Applied Z80Address CpuTimeIncrement


applyRegisterChange : RegisterChange -> FlagRegisters -> MainWithIndexRegisters -> RegisterChangeApplied
applyRegisterChange change z80_flags main =
    case change of
        ChangeRegisterCWithTime int time ->
            MainRegsWithTimeApplied { main | c = int } time

        ChangeRegisterBC b_value c_value time ->
            MainRegsWithTimeApplied { main | b = b_value, c = c_value } time

        ChangeRegisterDE d_value e_value time ->
            MainRegsWithTimeApplied { main | d = d_value, e = e_value } time

        ChangeRegisterEWithTime int time ->
            MainRegsWithTimeApplied { main | e = int } time

        ChangeRegisterHL int time ->
            MainRegsWithTimeApplied { main | hl = int } time

        ChangeRegisterIX int cpuTimeIncrement ->
            MainRegsWithTimeApplied { main | ix = int } cpuTimeIncrement

        ChangeRegisterIY int cpuTimeIncrement ->
            MainRegsWithTimeApplied { main | iy = int } cpuTimeIncrement

        ChangeRegisterB int ->
            MainRegsApplied { main | b = int }

        ChangeRegisterD int ->
            MainRegsApplied { main | d = int }

        ChangeRegisterA int ->
            FlagRegsApplied { z80_flags | a = int }

        ChangeRegisterE int ->
            MainRegsApplied { main | e = int }

        ChangeRegisterC int ->
            MainRegsApplied { main | c = int }

        ChangeRegisterH int ->
            --MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF) (shiftLeftBy8 int) }
            MainRegsApplied { main | hl = Bitwise.or (lower8Bits main.hl) (shiftLeftBy8 int) |> fromInt }

        ChangeRegisterL int ->
            --MainRegsApplied { main | hl = Bitwise.or (Bitwise.and main.hl 0xFF00) int }
            MainRegsApplied { main | hl = Bitwise.or (top8BitsWithoutShift main.hl) int |> fromInt }

        PushedValue int ->
            PushedValueApplied int

        RegChangeNewSP int cpuTimeIncrement ->
            NewSPApplied int cpuTimeIncrement

        IncrementIndirect int cpuTimeIncrement ->
            IncrementIndirectApplied int cpuTimeIncrement

        DecrementIndirect int cpuTimeIncrement ->
            DecrementIndirectApplied int cpuTimeIncrement

        RegisterChangeJump int ->
            JumpApplied int

        SetIndirect addr value cpuTimeIncrement ->
            SetIndirectApplied addr value cpuTimeIncrement

        ChangeRegisterDEAndHL de hl ->
            MainRegsApplied ({ main | hl = hl } |> set_de_main de)

        Shifter0 int cpuTimeIncrement ->
            Shifter0Applied int cpuTimeIncrement

        Shifter1 int cpuTimeIncrement ->
            Shifter1Applied int cpuTimeIncrement

        Shifter2 int cpuTimeIncrement ->
            Shifter2Applied int cpuTimeIncrement

        Shifter3 int cpuTimeIncrement ->
            Shifter3Applied int cpuTimeIncrement

        Shifter4 int cpuTimeIncrement ->
            Shifter4Applied int cpuTimeIncrement

        Shifter5 int cpuTimeIncrement ->
            Shifter5Applied int cpuTimeIncrement

        Shifter6 int cpuTimeIncrement ->
            Shifter6Applied int cpuTimeIncrement

        Shifter7 int cpuTimeIncrement ->
            Shifter7Applied int cpuTimeIncrement
