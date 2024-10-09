module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import RegisterChange exposing (RegisterChange, applyRegisterChange)
import Z80Change exposing (FlagChange(..), applyZ80Change)
import Z80ChangeData exposing (RegisterChangeData, Z80ChangeData)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (IXIYHL(..), Z80)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta CpuTimeCTime Z80ChangeData
    | FlagDelta CpuTimeCTime FlagChange
    | RegisterChangeDelta CpuTimeCTime RegisterChangeData


apply_delta : Z80 -> DeltaWithChanges -> Z80
apply_delta z80 z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        PureDelta cpu_time z80ChangeData ->
            z80 |> applyPureDelta cpu_time z80ChangeData

        FlagDelta cpuTimeCTime flagRegisters ->
            z80 |> applyFlagDelta cpuTimeCTime flagRegisters

        RegisterChangeDelta cpuTimeCTime registerChange ->
            z80 |> applyRegisterDelta cpuTimeCTime registerChange


applyFlagDelta : CpuTimeCTime -> FlagChange -> Z80 -> Z80
applyFlagDelta cpu_time z80_flags tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        new_pc =
            Bitwise.and (tmp_z80.pc + 1) 0xFFFF

        z80 =
            { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTime 4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

    in
    case z80_flags of
        OnlyFlags flagRegisters ->
            { z80 | flags = flagRegisters }

        FlagChangeB int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }

        FlagChangeC int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | c = int } }


applyPureDelta : CpuTimeCTime -> Z80ChangeData -> Z80 -> Z80
applyPureDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTime (4 + z80changeData.cpu_time) }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + z80changeData.pc_change) 0xFFFF
    in
    { z80 | pc = new_pc } |> applyZ80Change z80changeData.changes


applyRegisterDelta : CpuTimeCTime -> RegisterChangeData -> Z80 -> Z80
applyRegisterDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTime (4 + z80changeData.cpu_time) }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + z80changeData.pc_change) 0xFFFF
    in
    { z80 | pc = new_pc } |> applyRegisterChange z80changeData.changes
