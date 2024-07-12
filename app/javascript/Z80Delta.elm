module Z80Delta exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, add_cpu_time_time)
import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters, ProgramCounter, Z80)


type Z80Delta
    = Whole Z80
    | MainRegsWithPcAndCpuTime MainWithIndexRegisters Int CpuTimeCTime
    | OnlyEnv Z80Env
    | MainRegsAndCpuTime MainWithIndexRegisters Int
    | FlagsWithMain FlagRegisters MainWithIndexRegisters
    | FlagsWithPCMainAndTime FlagRegisters Int MainWithIndexRegisters Int
    | FlagRegs FlagRegisters
    | MainRegs MainWithIndexRegisters
    | MainRegsWithPc MainWithIndexRegisters Int
    | FlagsAndAlt FlagRegisters FlagRegisters
    | EnvWithFlags Z80Env FlagRegisters
    | CpuTimeWithFlags CpuTimeCTime FlagRegisters
    | EnvWithFlagsAndPc Z80Env FlagRegisters Int
    | CpuTimeWithFlagsAndPc CpuTimeCTime FlagRegisters Int
    | MainRegsWithEnv MainWithIndexRegisters Z80Env
    | PcAndCpuTime ProgramCounter Int
    | SpAndCpuTime Int Int
    | EnvWithPc Z80Env Int
    | CpuTimeWithPc CpuTimeCTime Int
    | CpuTimeWithSpAndPc CpuTimeCTime Int Int
    | NoChange
    | OnlyPc Int
    | FlagsWithPcAndTime FlagRegisters Int CpuTimeCTime
    | InterruptsWithCpuTime InterruptRegisters CpuTimeCTime
    | MainRegsWithSpAndTime MainWithIndexRegisters Int CpuTimeCTime
    | OnlyTime CpuTimeCTime


type alias DeltaWithChanges =
    { delta : Z80Delta
    , interrupts : InterruptRegisters
    , pc : Int
    , time : CpuTimeCTime
    }


apply_delta : Z80 -> DeltaWithChanges -> Z80
apply_delta z80 z80delta =
    case z80delta.delta of
        Whole just_z80 ->
            just_z80

        MainRegsWithPcAndCpuTime mainRegisters pc cpu_time ->
          let
            env = z80.env
          in
            { z80 | pc = pc, env = { env | time = cpu_time}, main = mainRegisters, interrupts = z80delta.interrupts }

        OnlyEnv z80Env ->
            { z80 | pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        MainRegsAndCpuTime mainRegisters cpu_time ->
          let
            env = z80.env
          in
            { z80 | pc = z80delta.pc, env = { env | time = z80delta.time |> add_cpu_time_time cpu_time }, main = mainRegisters, interrupts = z80delta.interrupts }

        PcAndCpuTime pc cpu_time ->
          let
            env = z80.env
          in
            { z80 | pc = pc.pc, env = { env | time = z80delta.time |> add_cpu_time_time cpu_time } , interrupts = z80delta.interrupts }

        FlagsWithMain flagRegisters mainRegisters ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { env | time = z80delta.time }, main = mainRegisters, interrupts = z80delta.interrupts }

        FlagRegs flagRegisters ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagsAndAlt flagRegisters altFlags ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, alt_flags = altFlags, pc = z80delta.pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        EnvWithFlags z80Env flagRegisters ->
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = z80Env, interrupts = z80delta.interrupts }

        CpuTimeWithFlags time flagRegisters ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, pc = z80delta.pc, env = { env | time = time }, interrupts = z80delta.interrupts }

        MainRegsWithEnv mainRegisters z80Env ->
            { z80 | env = z80Env, pc = z80delta.pc, main = mainRegisters, interrupts = z80delta.interrupts }

        EnvWithPc z80Env programCounter ->
            { z80 | env = z80Env, pc = programCounter, interrupts = z80delta.interrupts }

        CpuTimeWithPc cpu_time programCounter ->
          let
            env = z80.env
          in
            { z80 | env = { env | time = cpu_time }, pc = programCounter, interrupts = z80delta.interrupts }

        FlagsWithPCMainAndTime flagRegisters pc mainWithIndexRegisters cpu_time ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, pc = pc, env = { env | time = z80delta.time |> add_cpu_time_time cpu_time }, main = mainWithIndexRegisters, interrupts = z80delta.interrupts }

        SpAndCpuTime sp cpu_time ->
          let
            env = z80.env
          in
            { z80 | pc = z80delta.pc, env = { env | time = z80delta.time |> add_cpu_time_time cpu_time, sp = sp }, interrupts = z80delta.interrupts }

        EnvWithFlagsAndPc z80Env flagRegisters pc ->
            { z80 | flags = flagRegisters, pc = pc, env = z80Env, interrupts = z80delta.interrupts }

        CpuTimeWithFlagsAndPc cpu_time flagRegisters pc ->
          let
            env = z80.env
          in
            { z80 | flags = flagRegisters, pc = pc, env = { env | time = cpu_time }, interrupts = z80delta.interrupts }

        NoChange ->
          let
            env = z80.env
          in
            { z80 | pc = z80delta.pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        MainRegs mainWithIndexRegisters ->
          let
            env = z80.env
          in
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        MainRegsWithPc mainWithIndexRegisters pc ->
          let
            env = z80.env
          in
            { z80 | main = mainWithIndexRegisters, pc = pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        OnlyPc pc ->
          let
            env = z80.env
          in
            { z80 | pc = pc, env = { env | time = z80delta.time }, interrupts = z80delta.interrupts }

        FlagsWithPcAndTime flags pc time ->
          let
            env = z80.env
          in
            { z80 | pc = pc, flags = flags, env = { env | time = time }, interrupts = z80delta.interrupts }

        CpuTimeWithSpAndPc time sp pc ->
          let
            env = z80.env
          in
            { z80 | pc = pc, env = { env | time = time, sp = sp }, interrupts = z80delta.interrupts }

        InterruptsWithCpuTime interruptRegisters cpuTimeCTime ->
          let
            env = z80.env
          in
            { z80 | pc = z80delta.pc, env = { env | time = cpuTimeCTime }, interrupts = interruptRegisters }

        MainRegsWithSpAndTime mainWithIndexRegisters sp cpuTimeCTime ->
          let
            env = z80.env
          in
            { z80 | main = mainWithIndexRegisters, pc = z80delta.pc, env = { env | sp = sp, time = cpuTimeCTime }, interrupts = z80delta.interrupts }

        OnlyTime cpuTimeCTime ->
          let
            env = z80.env
          in
            { z80 | pc = z80delta.pc, env = { env | time = cpuTimeCTime }, interrupts = z80delta.interrupts }
