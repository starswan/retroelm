module Z80Delta exposing (..)

import Z80Env exposing (Z80Env, add_cpu_time_env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters, ProgramCounter, Z80)

type IXIYHLValue
    = IXValue Int
    | IYValue Int
    | HLValue Int


type Z80Delta
    = Whole Z80
    | MainRegsWithPcAndEnv MainWithIndexRegisters ProgramCounter Z80Env
    | JustEnv Z80Env
    | MainRegsAndCpuTime MainWithIndexRegisters Int
    | FlagsWithMain FlagRegisters MainWithIndexRegisters
    | FlagRegs FlagRegisters
    | FlagsAndAlt FlagRegisters FlagRegisters
    | EnvWithFlags Z80Env FlagRegisters
    | EnvWithSpAndPc Z80Env Int ProgramCounter
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | PcAndCpuTime ProgramCounter Int
    | EnvWithPc Z80Env Int
    | CpuTimeFlagsAndIXIYHLValueWithPC Int FlagRegisters IXIYHLValue Int
    | PCIXIYHL Int IXIYHLValue
    | PcEnvIXIYHL Int Z80Env IXIYHLValue
    --| IXIYMainFlagsCpuTime Int Int MainRegisters FlagRegisters Int


type alias DeltaWithChanges =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , env : Z80Env
    }


apply_delta : Z80 -> DeltaWithChanges -> Z80
apply_delta z80 z80delta =
    case z80delta.delta of
        Whole just_z80 ->
            just_z80

        --Flags flagRegisters env -> { z80 | flags = flagRegisters, env = env }
        -- have to replicate startup changes for all deltas that don't produce PC changes
        MainRegsWithPcAndEnv mainRegisters pc z80Env ->
            { z80 | pc = pc.pc, env = z80Env, main = mainRegisters, interrupts = z80delta.interrupts }

        JustEnv z80Env ->
            { z80 | pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        MainRegsAndCpuTime mainRegisters cpu_time ->
            { z80 | pc = z80delta.pc, env = z80delta.env |> add_cpu_time_env cpu_time, main = mainRegisters, interrupts = z80delta.interrupts }

        PcAndCpuTime pc cpu_time ->
            { z80 | pc = pc.pc, env = z80delta.env |> add_cpu_time_env cpu_time, interrupts = z80delta.interrupts }

        FlagsWithMain flagRegisters mainRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80delta.env, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        FlagsAndAlt flagRegisters altFlags ->
            { z80 | flags = flagRegisters, alt_flags = altFlags, pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        EnvWithFlags z80Env flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        EnvWithSpAndPc z80Env sp programCounter ->
            { z80 | sp = sp, pc = programCounter.pc, env = z80Env, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        CpuTimeFlagsAndIXIYHLValueWithPC cpu_time flagRegisters iXIYHLValue pc ->
            let
                env = z80.env |> add_cpu_time_env cpu_time
                main = z80.main
                z80_1 = case iXIYHLValue of
                    IXValue int -> { main | ix = int }
                    IYValue int -> { main | iy = int }
                    HLValue hlvalue -> { main | hl = hlvalue }
            in
                { z80 | main = z80_1, flags = flagRegisters, env = env, pc = pc, interrupts = z80delta.interrupts }

        PCIXIYHL pc iXIYHLValue ->
            let
                main = z80.main
                z80_1 = case iXIYHLValue of
                    IXValue int ->
                        { main | ix = int }
                    IYValue int ->
                        { main | iy = int }
                    HLValue hlvalue ->
                        { main | hl = hlvalue }
            in
                { z80 | main = z80_1, pc = pc, interrupts = z80delta.interrupts }

        PcEnvIXIYHL pc z80Env iXIYHLValue ->
            let
                main = z80.main
                z80_1 = case iXIYHLValue of
                    IXValue int -> { main | ix = int }
                    IYValue int -> { main | iy = int }
                    HLValue hlvalue -> { main | hl = hlvalue }
            in
                { z80 | main = z80_1, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        --IXIYMainFlagsCpuTime ix iy mainRegisters flagRegisters cpu_time ->
        --    { z80 | flags = flagRegisters, main = mainRegisters, ix = ix, iy = iy, pc = z80delta.pc, env = z80delta.env |> add_cpu_time_env cpu_time, interrupts = z80delta.interrupts }

