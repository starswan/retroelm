module Z80Transform exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement, addCpuTimeTimeInc, cpuTimeIncrement10, cpuTimeIncrement11, cpuTimeIncrement15, cpuTimeIncrement4, cpuTimeIncrement6, cpuTimeIncrement8, increment7)
import TransformTypes exposing (InstructionDuration(..))
import Z80Env exposing (Z80Env, addCpuTimeEnvInc, setMem, setMem16, z80_push)
import Z80Flags exposing (FlagRegisters, IntWithFlags, dec, inc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7)
import Z80Ram exposing (getRamValue, setRamValue)
import Z80Types exposing (MainWithIndexRegisters, Z80, set_bc_main, set_de_main)


type InstructionLength
    = OneByteInstruction
    | TwoByteInstruction
    | ThreeByteInstruction
    | FourByteInstruction
    | JumpInstruction Int


type ChangeEnvOperation
    = Store16BitMemoryValue Int Int
    | Store8BitMemoryValue Int Int
    | ChangeSPRegister Int
    | PushValue Int
    | ApplyShifter0 Int CpuTimeIncrement
    | ApplyShifter1 Int CpuTimeIncrement
    | ApplyShifter2 Int CpuTimeIncrement
    | ApplyShifter3 Int CpuTimeIncrement
    | ApplyShifter4 Int CpuTimeIncrement
    | ApplyShifter5 Int CpuTimeIncrement
    | ApplyShifter6 Int CpuTimeIncrement
    | ApplyShifter7 Int CpuTimeIncrement


type ChangeMemoryOperation
    = IncrementMemory Int
    | DecrementMemory Int


type ChangeMainOperation
    = ChangeBCRegister Int
    | ChangeBRegister Int
    | ChangeCRegister Int
    | ChangeDRegister Int
    | ChangeERegister Int
    | ChangeDERegister Int
    | ChangeHLRegister Int
    | ChangeIXRegister Int
    | ChangeIYRegister Int
    | ChangeMainRegisters MainWithIndexRegisters


type ChangeMainFlagsOperation
    = ChangeMainFlagsHL Int
    | ChangeMainFlagsIX Int
    | ChangeMainFlagsIY Int
    | ChangeMainFlagsB Int
    | ChangeMainFlagsC Int
    | ChangeMainFlagsD Int
    | ChangeMainFlagsE Int


type Z80Operation
    = ChangeEnv ChangeEnvOperation
    | ChangeMain ChangeMainOperation
    | ChangeMemory ChangeMemoryOperation
    | ChangeMainWithFlags ChangeMainFlagsOperation FlagRegisters
    | ChangeFlagRegisters FlagRegisters
    | ChangeNothing


type alias Z80Transform =
    { pcIncrement : InstructionLength
    , time : CpuTimeCTime
    , timeIncrement : InstructionDuration
    , operation : Z80Operation
    }


executeTransform : Z80Transform -> Z80 -> Z80
executeTransform z80Transform z80 =
    let
        interrupts =
            z80.interrupts

        main =
            z80.main

        env =
            z80.env

        new_pc =
            case z80Transform.pcIncrement of
                OneByteInstruction ->
                    z80.pc + 1

                TwoByteInstruction ->
                    z80.pc + 2

                ThreeByteInstruction ->
                    z80.pc + 3

                FourByteInstruction ->
                    z80.pc + 4

                JumpInstruction pc_value ->
                    pc_value

        new_time =
            case z80Transform.timeIncrement of
                FourTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement4

                SixTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement6

                SevenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc increment7

                ZeroTStates ->
                    z80Transform.time

                EightTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement8

                TenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement10

                FifteenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement15

                ElevenTStates ->
                    z80Transform.time |> addCpuTimeTimeInc cpuTimeIncrement11
    in
    case z80Transform.operation of
        ChangeEnv envChange ->
            let
                env1 =
                    case envChange of
                        Store16BitMemoryValue addr value ->
                            env |> setMem16 addr value

                        ChangeSPRegister int ->
                            { env | sp = int }

                        PushValue value ->
                            env |> z80_push value

                        Store8BitMemoryValue addr value ->
                            env |> setMem addr value

                        ApplyShifter0 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter0 addr cpuTimeIncrement

                        ApplyShifter1 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter1 addr cpuTimeIncrement

                        ApplyShifter2 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter2 addr cpuTimeIncrement

                        ApplyShifter3 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter3 addr cpuTimeIncrement

                        ApplyShifter4 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter4 addr cpuTimeIncrement

                        ApplyShifter5 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter5 addr cpuTimeIncrement

                        ApplyShifter6 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter6 addr cpuTimeIncrement

                        ApplyShifter7 addr cpuTimeIncrement ->
                            z80 |> applyShifter shifter7 addr cpuTimeIncrement
            in
            { z80 | pc = new_pc |> Bitwise.and 0xFFFF, env = { env1 | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeMain mainChange ->
            let
                main_1 =
                    case mainChange of
                        ChangeBCRegister int ->
                            main |> set_bc_main int

                        ChangeDERegister int ->
                            main |> set_de_main int

                        ChangeHLRegister int ->
                            { main | hl = int }

                        ChangeIXRegister int ->
                            { main | ix = int }

                        ChangeIYRegister int ->
                            { main | iy = int }

                        ChangeBRegister int ->
                            { main | b = int }

                        ChangeCRegister int ->
                            { main | c = int }

                        ChangeDRegister int ->
                            { main | d = int }

                        ChangeERegister int ->
                            { main | e = int }

                        ChangeMainRegisters mainWithIndexRegisters ->
                            mainWithIndexRegisters
            in
            { z80 | main = main_1, pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeFlagRegisters flagRegisters ->
            { z80 | flags = flagRegisters, pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeMainWithFlags mainFlagChange flagRegisters ->
            let
                main_1 =
                    case mainFlagChange of
                        ChangeMainFlagsHL int ->
                            { main | hl = int }

                        ChangeMainFlagsIX int ->
                            { main | ix = int }

                        ChangeMainFlagsIY int ->
                            { main | iy = int }

                        ChangeMainFlagsB int ->
                            { main | b = int }

                        ChangeMainFlagsC int ->
                            { main | c = int }

                        ChangeMainFlagsD int ->
                            { main | d = int }

                        ChangeMainFlagsE int ->
                            { main | e = int }
            in
            { z80 | main = main_1, flags = flagRegisters, pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeMemory changeMemoryOperation ->
            let
                ( env1, flags ) =
                    case changeMemoryOperation of
                        IncrementMemory ramAddr ->
                            let
                                value =
                                    env.ram |> getRamValue ramAddr

                                valueWithFlags =
                                    z80.flags |> inc value
                            in
                            ( { env | ram = env.ram |> setRamValue ramAddr valueWithFlags.value }, valueWithFlags.flags )

                        DecrementMemory ramAddr ->
                            let
                                value =
                                    env.ram |> getRamValue ramAddr

                                valueWithFlags =
                                    z80.flags |> dec value
                            in
                            ( { env | ram = env.ram |> setRamValue ramAddr valueWithFlags.value }, valueWithFlags.flags )
            in
            { z80 | flags = flags, pc = new_pc |> Bitwise.and 0xFFFF, env = { env1 | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }

        ChangeNothing ->
            { z80 | pc = new_pc |> Bitwise.and 0xFFFF, env = { env | time = new_time }, interrupts = { interrupts | r = interrupts.r + 1 } }


applyShifter : (Int -> FlagRegisters -> IntWithFlags) -> Int -> CpuTimeIncrement -> Z80 -> Z80Env
applyShifter shifterFunc addr cpuTimeIncrement z80 =
    if addr >= 0x4000 then
        let
            value =
                z80.env.ram |> getRamValue (addr - 0x4000)

            result =
                z80.flags |> shifterFunc value

            env =
                z80.env

            --interrupts =
            --    z80.interrupts
            --env_1 =
            --    { env | time = value.time }
            env_2 =
                env |> setMem addr result.value |> addCpuTimeEnvInc cpuTimeIncrement
        in
        --{ z80 | pc = new_pc, env = env_2, interrupts = { interrupts | r = interrupts.r + 1 } }
        env_2

    else
        z80.env
