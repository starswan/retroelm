module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, addCpuTimeTime)
import Z80Env exposing (Z80Env, add_cpu_time_env, setMem, setMem16, z80_push)
import Z80Flags exposing (FlagRegisters)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, f_szh0n0p, imm16, rst, set408bit, set_flag_regs)


type Z80Delta
    = Whole Z80
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
    | MainRegsAndCpuTime MainWithIndexRegisters Int
    | FlagsWithMain FlagRegisters MainWithIndexRegisters
    | FlagsWithPCMainAndTime FlagRegisters Int MainWithIndexRegisters Int
    | FlagsWithPCMainAndCpuTime FlagRegisters Int MainWithIndexRegisters CpuTimeCTime
    | FlagRegs FlagRegisters
    | MainRegs MainWithIndexRegisters
    | MainRegsWithPc MainWithIndexRegisters Int
    | FlagsAndAlt FlagRegisters FlagRegisters
    | CpuTimeWithFlags CpuTimeCTime FlagRegisters
    | EnvWithFlagsAndPc Z80Env FlagRegisters Int
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | SpAndCpuTime Int Int
    | EnvWithPc Z80Env Int
    | CpuTimeWithPc CpuTimeCTime Int
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
    | NoChange
    | OnlyPc Int
    | FlagsWithPcAndTime FlagRegisters Int CpuTimeCTime
    | FlagsWithSpTimeAndPc FlagRegisters Int CpuTimeCTime Int
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | OnlyInterrupts InterruptRegisters
    | MainRegsWithSpAndTime MainWithIndexRegisters Int CpuTimeCTime
    | MainRegsWithSpPcAndTime MainWithIndexRegisters Int Int CpuTimeCTime
    | OnlyTime CpuTimeCTime
    | MainRegsWithAltRegs MainWithIndexRegisters MainRegisters
    | MainRegsWithEnvAndPc MainWithIndexRegisters Z80Env Int
    | OnlyPush Int
    | PushWithCpuTimeAndPc Int CpuTimeCTime Int
    | SetMem8WithTime Int Int Int
    | SetMem16WithTimeAndPc Int Int Int Int
    | SetMem8WithCpuTimeIncrementAndPc Int Int CpuTimeCTime Int Int
    | PcTimeFlagsSet408Bit Int CpuTimeCTime FlagRegisters Int Int
    | PcTimeSet408Bit Int CpuTimeCTime Int Int
    | Fszh0n0pTimeDeltaSet408Bit Int Int Int


type alias DeltaWithChanges =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , time : CpuTimeCTime
    }


apply_delta : Z80 -> DeltaWithChanges -> Z80
apply_delta z80 z80delta =
    let
        z80_env =
            z80.env
    in
    case z80delta.delta of
        Whole just_z80 ->
            just_z80

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
            { z80 | pc = pc, env = { z80_env | time = cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        MainRegsAndCpuTime mainRegisters cpu_time ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagsWithMain flagRegisters mainRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagsAndAlt flagRegisters altFlags ->
            { z80 | flags = flagRegisters, alt_flags = altFlags, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        CpuTimeWithFlags time flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { z80_env | time = time }, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        CpuTimeWithPc cpu_time programCounter ->
            { z80 | env = { z80_env | time = cpu_time }, pc = programCounter, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndTime flagRegisters pc mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTime sp cpu_time ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time |> addCpuTimeTime cpu_time, sp = sp }, interrupts = z80delta.interrupts }

        EnvWithFlagsAndPc z80Env flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = cpu_time }, interrupts = z80delta.interrupts }

        NoChange ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

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

        OnlyInterrupts interruptRegisters ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = interruptRegisters }

        MainRegsWithSpAndTime main sp time ->
            { z80 | main = main, pc = z80delta.pc, env = { z80_env | sp = sp, time = time }, interrupts = z80delta.interrupts }

        MainRegsWithSpPcAndTime main sp pc time ->
            { z80 | main = main, pc = pc, env = { z80_env | sp = sp, time = time }, interrupts = z80delta.interrupts }

        OnlyTime cpuTimeCTime ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = cpuTimeCTime }, interrupts = z80delta.interrupts }

        MainRegsWithAltRegs main alt_main ->
            { z80 | main = main, alt_main = alt_main, pc = z80delta.pc, env = { z80_env | time = z80delta.time }, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        MainRegsWithEnvAndPc mainWithIndexRegisters z80Env pc ->
            { z80 | env = z80Env, pc = pc, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndCpuTime flagRegisters pc mainWithIndexRegisters time ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        FlagsWithSpTimeAndPc flagRegisters sp time pc ->
            { z80 | flags = flagRegisters, pc = pc, env = { z80_env | time = time, sp = sp }, interrupts = z80delta.interrupts }

        OnlyPush value ->
            { z80 | pc = z80delta.pc, env = { z80_env | time = z80delta.time } |> z80_push value, interrupts = z80delta.interrupts }

        PushWithCpuTimeAndPc value time pc ->
            { z80 | pc = pc, env = { z80_env | time = time } |> z80_push value, interrupts = z80delta.interrupts }

        SetMem8WithTime addr value time ->
            { z80 | pc = z80delta.pc, env = z80.env |> setMem addr value |> add_cpu_time_env time, interrupts = z80delta.interrupts }

        SetMem16WithTimeAndPc addr value time pc ->
            { z80 | pc = pc, env = z80.env |> setMem16 addr value |> add_cpu_time_env time, interrupts = z80delta.interrupts }

        SetMem8WithCpuTimeIncrementAndPc addr value cpuTimeCTime time pc ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } |> setMem addr value |> add_cpu_time_env time, interrupts = z80delta.interrupts }

        PcTimeFlagsSet408Bit pc cpuTimeCTime flagRegisters caseval value ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } } |> set_flag_regs flagRegisters |> set408bit caseval value HL

        PcTimeSet408Bit pc cpuTimeCTime caseval result ->
            { z80 | pc = pc, env = { z80_env | time = cpuTimeCTime } } |> set408bit caseval result HL

        Fszh0n0pTimeDeltaSet408Bit timeDelta caseval result ->
            let
                z80_1 =
                    z80 |> set408bit caseval result HL
            in
            { z80_1 | flags = z80_1.flags |> f_szh0n0p result } |> add_cpu_time timeDelta


delta_noop : Z80ROM -> Z80 -> Z80Delta
delta_noop _ _ =
    NoChange


jp_delta : Bool -> Z80ROM -> Z80 -> Z80Delta
jp_delta y rom48k z80 =
    let
        result =
            z80 |> jp y rom48k
    in
    CpuTimeWithPc result.time result.pc


jp : Bool -> Z80ROM -> Z80 -> CpuTimeAndPc
jp y rom48k z80 =
    let
        a =
            z80 |> imm16 rom48k
    in
    if y then
        CpuTimeAndPc a.time a.value

    else
        CpuTimeAndPc a.time a.pc


rst_delta : Int -> Z80 -> Z80Delta
rst_delta value z80 =
    --z80 |> rst_z80 0xC7
    let
        result =
            z80 |> rst value
    in
    EnvWithPc result.env result.pc
