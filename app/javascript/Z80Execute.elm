module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), addCpuTimeTime, addCpuTimeTimeInc, cpuTimeIncrement4)
import RegisterChange exposing (RegisterChange, RegisterChangeApplied(..), applyRegisterChange)
import SingleByteWithEnv exposing (SingleByteEnvChange(..))
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import SingleWith8BitParameter exposing (DoubleWithRegisterChange(..), JumpChange(..), Single8BitChange, applySimple8BitChange)
import TripleByte exposing (TripleByteChange(..))
import TripleWithFlags exposing (TripleWithFlagsChange(..))
import Utils exposing (shiftLeftBy8)
import Z80Change exposing (FlagChange(..), Z80Change, applyZ80Change)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (addCpuTimeEnvInc, mem, setMem, z80_pop, z80_push)
import Z80Flags exposing (dec, inc)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), Z80, set_bc_main, set_de_main)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | PureDelta Int CpuTimeCTime Z80Change
    | FlagDelta CpuTimeCTime FlagChange
    | RegisterChangeDelta CpuTimeCTime RegisterChange
    | Simple8BitDelta CpuTimeCTime Single8BitChange
    | DoubleWithRegistersDelta CpuTimeCTime DoubleWithRegisterChange
    | JumpChangeDelta CpuTimeCTime JumpChange
    | NoParamsDelta CpuTimeCTime NoParamChange
    | TripleChangeDelta CpuTimeCTime TripleByteChange
    | SingleEnvDelta CpuTimeCTime SingleByteEnvChange
    | TripleFlagDelta CpuTimeCTime TripleWithFlagsChange


apply_delta : Z80 -> Z80ROM -> DeltaWithChanges -> Z80
apply_delta z80 rom48k z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        PureDelta cpuInc cpu_time z80ChangeData ->
            z80 |> applyPureDelta cpuInc cpu_time z80ChangeData

        FlagDelta cpuTimeCTime flagRegisters ->
            z80 |> applyFlagDelta cpuTimeCTime flagRegisters rom48k

        RegisterChangeDelta cpuTimeCTime registerChange ->
            z80 |> applyRegisterDelta cpuTimeCTime registerChange rom48k

        Simple8BitDelta cpuTimeCTime single8BitChange ->
            z80 |> applySimple8BitDelta cpuTimeCTime single8BitChange

        DoubleWithRegistersDelta cpuTimeCTime doubleWithRegisterChange ->
            z80 |> applyDoubleWithRegistersDelta cpuTimeCTime doubleWithRegisterChange

        JumpChangeDelta cpuTimeCTime jumpChange ->
            z80 |> applyJumpChangeDelta cpuTimeCTime jumpChange

        NoParamsDelta cpuTimeCTime noParamChange ->
            z80 |> applyNoParamsDelta cpuTimeCTime noParamChange rom48k

        TripleChangeDelta cpuTimeCTime tripleByteChange ->
            z80 |> applyTripleChangeDelta cpuTimeCTime tripleByteChange

        SingleEnvDelta cpuTimeCTime singleByteEnvChange ->
            z80 |> applyEnvChangeDelta cpuTimeCTime singleByteEnvChange

        TripleFlagDelta cpuTimeCTime tripleWithFlagsChange ->
            z80 |> applyTripleFlagChange cpuTimeCTime tripleWithFlagsChange


applyJumpChangeDelta : CpuTimeCTime -> JumpChange -> Z80 -> Z80
applyJumpChangeDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        old_env =
            tmp_z80.env
    in
    case z80changeData of
        ActualJump jump ->
            let
                pc =
                    Bitwise.and (tmp_z80.pc + 2 + jump) 0xFFFF
            in
            { tmp_z80
                | pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime 8 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        NoJump ->
            let
                pc =
                    Bitwise.and (tmp_z80.pc + 2) 0xFFFF
            in
            { tmp_z80
                | pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime 8 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        FlagJump flags ->
            let
                pc =
                    Bitwise.and (tmp_z80.pc + 2) 0xFFFF
            in
            { tmp_z80
                | pc = pc
                , flags = flags
                , env = { old_env | time = cpu_time }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


applyDoubleWithRegistersDelta : CpuTimeCTime -> DoubleWithRegisterChange -> Z80 -> Z80
applyDoubleWithRegistersDelta cpu_time z80changeData tmp_z80 =
    -- This would appear to just be for DJNZ d
    let
        interrupts =
            tmp_z80.interrupts

        old_env =
            tmp_z80.env
    in
    case z80changeData of
        RelativeJumpWithTimeOffset single8BitChange maybeInt timeOffset ->
            let
                pc =
                    case maybeInt of
                        Just jump ->
                            Bitwise.and (tmp_z80.pc + 2 + jump) 0xFFFF

                        Nothing ->
                            Bitwise.and (tmp_z80.pc + 2) 0xFFFF

                main =
                    tmp_z80.main |> applySimple8BitChange single8BitChange
            in
            { tmp_z80
                | main = main
                , pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime timeOffset }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        DoubleRegChangeStoreIndirect addr value cpuTimeIncrement ->
            let
                pc =
                    Bitwise.and (tmp_z80.pc + 2) 0xFFFF

                env_1 =
                    { old_env | time = cpu_time } |> setMem addr value
            in
            { tmp_z80
                | pc = pc
                , env = env_1 |> addCpuTimeEnvInc cpuTimeIncrement
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


applySimple8BitDelta : CpuTimeCTime -> Single8BitChange -> Z80 -> Z80
applySimple8BitDelta cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        main =
            z80.main |> applySimple8BitChange z80changeData
    in
    { z80 | pc = new_pc, main = main }


applyFlagDelta : CpuTimeCTime -> FlagChange -> Z80ROM -> Z80 -> Z80
applyFlagDelta cpu_time z80_flags rom48k tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        new_pc =
            Bitwise.and (tmp_z80.pc + 1) 0xFFFF

        z80 =
            { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
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

        FlagChangeD int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | d = int } }

        FlagChangeE int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | e = int } }

        FlagChangeH int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }

        FlagChangeL int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }

        ReturnWithPop timeIncrement ->
            let
                result =
                    z80.env |> z80_pop rom48k

                env1 =
                    z80.env

                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            { z80 | pc = result.value, env = { env1 | time = result.time |> addCpuTimeTimeInc timeIncrement, sp = result.sp } }

        EmptyFlagChange timeIncrement ->
            let
                env1 =
                    z80.env |> addCpuTimeEnvInc timeIncrement
            in
            { z80 | env = env1 }

        FlagChangePush int ->
            { z80 | env = env |> z80_push int }


applyPureDelta : Int -> CpuTimeCTime -> Z80Change -> Z80 -> Z80
applyPureDelta cpuInc cpu_time z80changeData tmp_z80 =
    let
        interrupts =
            tmp_z80.interrupts

        env =
            tmp_z80.env

        z80 =
            { tmp_z80 | env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        new_pc =
            Bitwise.and (z80.pc + cpuInc) 0xFFFF
    in
    { z80 | pc = new_pc } |> applyZ80Change z80changeData


applyRegisterDelta : CpuTimeCTime -> RegisterChange -> Z80ROM -> Z80 -> Z80
applyRegisterDelta cpu_time z80changeData rom48k z80 =
    let
        interrupts =
            z80.interrupts

        env =
            z80.env

        new_pc =
            Bitwise.and (z80.pc + 1) 0xFFFF
    in
    case z80.main |> applyRegisterChange z80changeData z80.flags of
        MainRegsApplied new_main ->
            { z80 | pc = new_pc, main = new_main, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        FlagRegsApplied new_flags ->
            { z80 | pc = new_pc, flags = new_flags, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        MainRegsWithTimeApplied mainWithIndexRegisters timeIncrement ->
            let
                env_1 =
                    { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 |> addCpuTimeTimeInc timeIncrement }
            in
            { z80 | env = env_1, pc = new_pc, main = mainWithIndexRegisters, interrupts = { interrupts | r = interrupts.r + 1 } }

        PushedValueApplied int ->
            let
                env1 =
                    env |> z80_push int
            in
            { z80 | pc = new_pc, env = { env1 | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        NewSPApplied int cpuTimeIncrement ->
            { z80 | pc = new_pc, env = { env | sp = int, time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        JumpApplied int ->
            { z80 | pc = int, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }

        IncrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            let
                value =
                    mem addr cpu_time rom48k z80.env.ram

                env_2 =
                    { env | time = value.time }

                flags =
                    z80.flags |> inc value.value

                env_3 =
                    env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, interrupts = { interrupts | r = interrupts.r + 1 } }

        DecrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            let
                value =
                    mem addr cpu_time rom48k z80.env.ram

                env_2 =
                    { env | time = value.time }

                flags =
                    z80.flags |> dec value.value

                env_3 =
                    env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80 | pc = new_pc, env = env_3, flags = flags.flags, interrupts = { interrupts | r = interrupts.r + 1 } }



applyTripleChangeDelta : CpuTimeCTime -> TripleByteChange -> Z80 -> Z80
applyTripleChangeDelta cpu_time z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        new_pc =
            Bitwise.and (z80.pc + 3) 0xFFFF

        env =
            z80.env
    in
    case z80changeData of
        NewBCRegister int ->
            { z80
                | main = z80.main |> set_bc_main int
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        NewDERegister int ->
            { z80
                | main = z80.main |> set_de_main int
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        NewHLRegister int ->
            let
                main =
                    z80.main
            in
            { z80
                | main = { main | hl = int }
                , pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        NewSPRegister int ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4, sp = int }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        NewPCRegister int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


applyEnvChangeDelta : CpuTimeCTime -> SingleByteEnvChange -> Z80 -> Z80
applyEnvChangeDelta cpu_time z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        new_pc =
            Bitwise.and (z80.pc + 1) 0xFFFF

        env =
            z80.env
    in
    case z80changeData of
        NewSPValue int time ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc time |> addCpuTimeTimeInc cpuTimeIncrement4, sp = int }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


applyTripleFlagChange : CpuTimeCTime -> TripleWithFlagsChange -> Z80 -> Z80
applyTripleFlagChange cpu_time z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        env =
            z80.env
    in
    case z80changeData of
        Skip3ByteInstruction ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF
            in
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        AbsoluteJump int ->
            { z80
                | pc = int
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        TripleSetIndirect addr value cpuTimeIncrement ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF

                env1 =
                    { env | time = cpu_time } |> setMem addr value |> addCpuTimeEnvInc cpuTimeIncrement4 |> addCpuTimeEnvInc cpuTimeIncrement
            in
            { z80
                | pc = new_pc
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }
