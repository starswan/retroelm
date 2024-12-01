module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime, addCpuTimeTimeInc)
import Z80Env exposing (Z80Env, addCpuTimeEnv, setMem, setMem16, z80_push)
import Z80Flags exposing (FlagRegisters, f_szh0n0p)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, set408bit)


type Z80Delta
    = Whole Z80
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
    | FlagsWithPCMainAndTime FlagRegisters Int MainWithIndexRegisters CpuTimeIncrement
    | FlagsWithMainAndTime FlagRegisters MainWithIndexRegisters Int
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters CpuTimeCTime
    | FlagRegs FlagRegisters
    | MainRegs MainWithIndexRegisters
    | MainRegsWithPc MainWithIndexRegisters Int
    | CpuTimeWithFlags CpuTimeCTime FlagRegisters
    | EnvWithFlagsAndPc Z80Env FlagRegisters Int
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | SpAndCpuTime Int Int
    | EnvWithPc Z80Env Int
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
    | OnlyPc Int
    | FlagsWithPcAndTime FlagRegisters Int CpuTimeCTime
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | MainRegsWithSpPcAndTime MainWithIndexRegisters Int Int CpuTimeCTime
    | MainRegsWithEnvAndPc MainWithIndexRegisters Z80Env Int
    | PushWithCpuTimeAndPc Int CpuTimeCTime Int
    | SetMem8WithTime Int Int Int
    | SetMem16WithTimeAndPc Int Int Int Int
    | SetMem8WithCpuTimeIncrementAndPc Int Int CpuTimeCTime Int Int
    | PcTimeSet408Bit Int CpuTimeCTime Int Int
    | Fszh0n0pTimeDeltaSet408Bit Int Int Int


type alias DeltaWithChangesData =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , time : CpuTimeCTime
    }


applyDeltaWithChanges : DeltaWithChangesData -> Z80 -> Z80
applyDeltaWithChanges z80delta z80 =
    let
        z80_env =
            z80.env
    in
    case z80delta.delta of
        Whole just_z80 ->
            just_z80

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
            { z80 | pc = pc, env = { z80_env | time = cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        CpuTimeWithFlags time flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = time }, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndTime flagRegisters pc mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = z80delta.time |> addCpuTimeTimeInc cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        FlagsWithMainAndTime flagRegisters mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTime sp cpu_time ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time, sp = sp }, interrupts = z80delta.interrupts }

        EnvWithFlagsAndPc z80Env flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = cpu_time }, interrupts = z80delta.interrupts }

        MainRegs mainWithIndexRegisters ->
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        MainRegsWithPc mainWithIndexRegisters pc ->
            { z80 | main = mainWithIndexRegisters, pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        OnlyPc pc ->
            { z80 | pc = pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagsWithPcAndTime flags pc time ->
            { z80 | pc = pc, flags = flags, env = { z80_env | time = time }, interrupts = z80delta.interrupts }

        CpuTimeWithSpAndPc time sp pc ->
            { z80 | pc = pc, env = { z80_env | time = time, sp = sp }, interrupts = z80delta.interrupts }

        InterruptsWithCpuTime interruptRegisters cpuTimeCTime ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = cpuTimeCTime }, interrupts = interruptRegisters }

        MainRegsWithSpPcAndTime main sp pc time ->
            { z80 | main = main, pc = pc, env = { z80_env | sp = sp, time = time }, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        MainRegsWithEnvAndPc mainWithIndexRegisters z80Env pc ->
            { z80 | env = z80Env, pc = pc, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndCpuTime flagRegisters pc mainWithIndexRegisters time ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        PushWithCpuTimeAndPc value time pc ->
            { z80 | pc = pc, env = { z80_env | time = time } |> z80_push value, interrupts = z80delta.interrupts }

        SetMem8WithTime addr value time ->
            { z80 | pc = z80delta.pc, env = z80.env |> setMem addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        SetMem16WithTimeAndPc addr value time pc ->
            { z80 | pc = pc, env = z80.env |> setMem16 addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        SetMem8WithCpuTimeIncrementAndPc addr value cpuTimeCTime time pc ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } |> setMem addr value |> addCpuTimeEnv time, interrupts = z80delta.interrupts }

        PcTimeSet408Bit pc cpuTimeCTime caseval result ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } } |> set408bit caseval result HL

        Fszh0n0pTimeDeltaSet408Bit timeDelta caseval result ->
            let
                z80_1 =
                    z80 |> set408bit caseval result HL
            in
            { z80_1 | flags = z80_1.flags |> f_szh0n0p result } |> add_cpu_time timeDelta
