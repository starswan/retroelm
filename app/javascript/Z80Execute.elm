module Z80Execute exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..))
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..), TriplePCIncrement(..))
import RegisterChange exposing (RegisterChange, RegisterChangeApplied(..), applyRegisterChange)
import SingleNoParams exposing (NoParamChange(..), applyNoParamsDelta)
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (shiftLeftBy8)
import Z80Change exposing (FlagChange(..), Z80Change)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), applyDeltaWithChanges)
import Z80Env exposing (Z80Env, z80_pop)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainOperation(..), ChangeMemoryOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (IXIYHL(..), MainWithIndexRegisters, Z80)


type DeltaWithChanges
    = OldDeltaWithChanges DeltaWithChangesData
    | NoParamsDelta CpuTimeCTime NoParamChange


type ExecuteResult
    = Z80DeltaChange DeltaWithChanges
    | Transformer Z80Transform


apply_delta : Z80 -> Z80ROM -> DeltaWithChanges -> Z80
apply_delta z80 rom48k z80delta =
    case z80delta of
        OldDeltaWithChanges deltaWithChangesData ->
            z80 |> applyDeltaWithChanges deltaWithChangesData

        NoParamsDelta cpuTimeCTime noParamChange ->
            z80 |> applyNoParamsDelta cpuTimeCTime noParamChange rom48k


applyFlagDelta : PCIncrement -> CpuTimeCTime -> FlagChange -> Z80ROM -> Z80 -> Z80Transform
applyFlagDelta pcInc instrTime flagChange rom48k z80 =
    let
        --interrupts =
        --    tmp_z80.interrupts
        --
        --env =
        --    tmp_z80.env
        new_pc =
            --Bitwise.and (tmp_z80.pc + pcInc) 0xFFFF
            case pcInc of
                IncrementByOne ->
                    --(tmp_z80.pc + 1) |> Bitwise.and 0xFFFF
                    OneByteInstruction

                IncrementByTwo ->
                    --(tmp_z80.pc + 2) |> Bitwise.and 0xFFFF
                    TwoByteInstruction

        --z80 =
        --    { tmp_z80 | pc = new_pc, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
    in
    case flagChange of
        OnlyFlags flagRegisters ->
            --{ z80 | flags = flagRegisters }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeFlagRegisters flagRegisters }

        FlagChangeB int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | b = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeBRegister int) }

        FlagChangeC int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | c = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeCRegister int) }

        FlagChangeD int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | d = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeDRegister int) }

        FlagChangeE int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | e = int } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeERegister int) }

        FlagChangeH int ->
            let
                main =
                    z80.main

                hl =
                    Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF)
            in
            --{ z80 | main = { main | hl = Bitwise.or (shiftLeftBy8 int) (Bitwise.and main.hl 0xFF) } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeHLRegister hl) }

        FlagChangeL int ->
            let
                main =
                    z80.main

                hl =
                    Bitwise.or int (Bitwise.and main.hl 0xFF00)
            in
            --{ z80 | main = { main | hl = Bitwise.or int (Bitwise.and main.hl 0xFF00) } }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeMain (ChangeHLRegister hl) }

        ReturnWithPop timeIncrement ->
            let
                result =
                    z80.env |> z80_pop rom48k

                --env1 =
                --    z80.env
                --x = debug_log "ret nz" (result.value |> subName) Nothing
            in
            --{ z80 | pc = result.value, env = { env1 | time = result.time |> addCpuTimeTimeInc timeIncrement, sp = result.sp } }
            { pcIncrement = JumpInstruction result.value, time = instrTime, timeIncrement = SixTStates, operation = ChangeEnv (ChangeSPRegister result.sp) }

        EmptyFlagChange timeIncrement ->
            --let
            --    env1 =
            --        z80.env |> addCpuTimeEnvInc timeIncrement
            --in
            --{ z80 | env = env1 }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeNothing }

        FlagChangePush int ->
            --{ z80 | env = env |> z80_push int }
            { pcIncrement = new_pc, time = instrTime, timeIncrement = SixTStates, operation = ChangeEnv (PushValue int) }


applyRegisterDelta : PCIncrement -> CpuTimeCTime -> RegisterChange -> Z80 -> Z80Transform
applyRegisterDelta pc_inc cpu_time z80changeData z80 =
    let
        --interrupts =
        --    z80.interrupts
        --env =
        --    z80.env
        ( increment ) =
            case pc_inc of
                IncrementByOne ->
                    ( OneByteInstruction )

                IncrementByTwo ->
                    ( TwoByteInstruction )
    in
    case z80.main |> applyRegisterChange z80changeData z80.flags of
        MainRegsApplied new_main ->
            --{ z80 | pc = new_pc, main = new_main, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeMainRegisters new_main) }

        FlagRegsApplied new_flags ->
            --{ z80 | pc = new_pc, flags = new_flags, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeFlagRegisters new_flags }

        MainRegsWithTimeApplied mainWithIndexRegisters timeIncrement ->
            --let
            --    env_1 =
            --        { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 |> addCpuTimeTimeInc timeIncrement }
            --in
            --{ z80 | env = env_1, pc = new_pc, main = mainWithIndexRegisters, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeMainRegisters mainWithIndexRegisters) }

        PushedValueApplied int ->
            --let
            --    env1 =
            --        env |> z80_push int
            --in
            --{ z80 | pc = new_pc, env = { env1 | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (PushValue int) }

        NewSPApplied int cpuTimeIncrement ->
            --{ z80 | pc = new_pc, env = { env | sp = int, time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ChangeSPRegister int) }

        JumpApplied int ->
            --{ z80 | pc = int, env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement4 }, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = JumpInstruction int, time = cpu_time, timeIncrement = FourTStates, operation = ChangeNothing }

        IncrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to increment a stored value
            --let
            --    value =
            --        mem addr cpu_time rom48k z80.env.ram
            --
            --    env_2 =
            --        { env | time = value.time }
            --
            --    flags =
            --        z80.flags |> inc value.value
            --
            --    env_3 =
            --        env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            --in
            --{ z80 | pc = new_pc, env = env_3, flags = flags.flags, interrupts = { interrupts | r = interrupts.r + 1 } }
            let
                ram_addr =
                    addr - 0x4000
            in
            if ram_addr >= 0 then
                { pcIncrement = increment, time = cpu_time, timeIncrement = SevenTStates, operation = ChangeMemory (IncrementMemory ram_addr) }

            else
                { pcIncrement = increment, time = cpu_time, timeIncrement = SevenTStates, operation = ChangeNothing }

        DecrementIndirectApplied addr cpuTimeIncrement ->
            -- This should be a primitive operation on Z80Env to decrement a stored value
            --let
            --    value =
            --        mem addr cpu_time rom48k z80.env.ram
            --
            --    env_2 =
            --        { env | time = value.time }
            --
            --    flags =
            --        z80.flags |> dec value.value
            --
            --    env_3 =
            --        env_2 |> setMem addr flags.value |> addCpuTimeEnvInc cpuTimeIncrement
            --in
            --{ z80 | pc = new_pc, env = env_3, flags = flags.flags, interrupts = { interrupts | r = interrupts.r + 1 } }
            let
                ram_addr =
                    addr - 0x4000
            in
            if ram_addr >= 0 then
                { pcIncrement = increment, time = cpu_time, timeIncrement = SevenTStates, operation = ChangeMemory (DecrementMemory ram_addr) }

            else
                { pcIncrement = increment, time = cpu_time, timeIncrement = SevenTStates, operation = ChangeNothing }

        SetIndirectApplied addr value cpuTimeIncrement ->
            --let
            --    env_1 =
            --        env |> setMem addr value |> addCpuTimeEnvInc cpuTimeIncrement
            --in
            --{ z80 | pc = new_pc, env = env_1, interrupts = { interrupts | r = interrupts.r + 1 } }
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (Store8BitMemoryValue addr value) }

        Shifter0Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter0 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter0 addr cpuTimeIncrement) }

        Shifter1Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter1 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter1 addr cpuTimeIncrement) }

        Shifter2Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter2 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter2 addr cpuTimeIncrement) }

        Shifter3Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter3 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter3 addr cpuTimeIncrement) }

        Shifter4Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter4 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter4 addr cpuTimeIncrement) }

        Shifter5Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter5 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter5 addr cpuTimeIncrement) }

        Shifter6Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter6 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter6 addr cpuTimeIncrement) }

        Shifter7Applied addr cpuTimeIncrement ->
            --z80 |> applyShifter new_pc shifter7 addr cpuTimeIncrement cpu_time rom48k
            { pcIncrement = increment, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (ApplyShifter7 addr cpuTimeIncrement) }
