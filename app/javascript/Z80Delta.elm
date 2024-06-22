module Z80Delta exposing (..)

import Z80Env exposing (CpuTimeCTime, Z80Env, add_cpu_time_env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters, ProgramCounter, Z80)


type Z80Delta
    = Whole Z80
    | MainRegsWithPcAndEnv MainWithIndexRegisters Int Z80Env
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
    | JustEnv Z80Env
    | MainRegsAndCpuTime MainWithIndexRegisters Int
    | FlagsWithMain FlagRegisters MainWithIndexRegisters
    | FlagsWithPCMainAndTime FlagRegisters Int MainWithIndexRegisters Int
    | FlagRegs FlagRegisters
    | MainRegs MainWithIndexRegisters
    | MainRegsWithPc MainWithIndexRegisters Int
    | FlagsAndAlt FlagRegisters FlagRegisters
    | EnvWithFlags Z80Env FlagRegisters
    | EnvWithFlagsAndPc Z80Env FlagRegisters Int
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | PcAndCpuTime Int CpuTimeCTime
    | SpPcAndCpuTime Int Int CpuTimeCTime
    | PcAndCpuTimeIncrement Int Int
    | SpAndCpuTime Int Int
    | EnvWithPc Z80Env Int
    | NoChange


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
            { z80 | pc = pc, env = z80Env, main = mainRegisters, interrupts = z80delta.interrupts }

        JustEnv z80Env ->
            { z80 | pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        MainRegsAndCpuTime mainRegisters cpu_time ->
            { z80 | pc = z80delta.pc, env = z80delta.env |> add_cpu_time_env cpu_time, main = mainRegisters, interrupts = z80delta.interrupts }

        PcAndCpuTime pc cpu_time ->
            let
               env = z80delta.env
            in
               { z80 | pc = pc, env = { env | time = cpu_time }, interrupts = z80delta.interrupts }

        SpPcAndCpuTime sp pc cpu_time ->
            let
               env = z80delta.env
            in
               { z80 | pc = pc, env = { env | sp = sp, time = cpu_time }, interrupts = z80delta.interrupts }

        FlagsWithMain flagRegisters mainRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80delta.env, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        FlagsAndAlt flagRegisters altFlags ->
            { z80 | flags = flagRegisters, alt_flags = altFlags, pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        EnvWithFlags z80Env flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndTime flagRegisters pc mainWithIndexRegisters cpu_time ->
            { z80 | flags = flagRegisters, pc = pc, env = z80delta.env |> add_cpu_time_env cpu_time, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTime sp cpu_time ->
            let
                env = z80delta.env
            in
            { z80 | pc = z80delta.pc, env = { env | sp = sp } |> add_cpu_time_env cpu_time, interrupts = z80delta.interrupts }

        EnvWithFlagsAndPc z80Env flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        NoChange ->
            { z80 | pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        MainRegs mainWithIndexRegisters ->
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = z80delta.env, interrupts = z80delta.interrupts }

        MainRegsWithPc mainWithIndexRegisters pc ->
            { z80 | main = mainWithIndexRegisters, pc = pc, env = z80delta.env, interrupts = z80delta.interrupts }

        MainRegsWithPcAndCpuTime mainWithIndexRegisters pc cpuTimeCTime ->
            let
                env = z80delta.env
            in
            { z80 | main = mainWithIndexRegisters, pc = pc, env = { env | time = cpuTimeCTime }, interrupts = z80delta.interrupts }

        PcAndCpuTimeIncrement pc cpu_time ->
            { z80 | pc = pc, env = z80delta.env |> add_cpu_time_env cpu_time, interrupts = z80delta.interrupts }





