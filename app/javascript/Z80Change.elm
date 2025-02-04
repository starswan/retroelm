module Z80Change exposing (..)

import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement)
import TransformTypes exposing (InstructionDuration(..))
import Z80Flags exposing (FlagRegisters, IntWithFlags)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainFlagsOperation(..), ChangeMainOperation(..), InstructionLength, Z80Operation(..), Z80Transform)
import Z80Types exposing (Z80)


type Z80Change
    = FlagsWithBRegister IntWithFlags
    | FlagsWithCRegister IntWithFlags
    | FlagsWithDRegister IntWithFlags
    | FlagsWithERegister IntWithFlags
    | HLRegister Int FlagRegisters
    | IXRegister Int FlagRegisters
    | IYRegister Int FlagRegisters
    | FlagsWithHLRegister FlagRegisters Int InstructionDuration
    | FlagsWithIXRegister FlagRegisters Int InstructionDuration
    | FlagsWithIYRegister FlagRegisters Int InstructionDuration
    | Z80RegisterB Int
    | Z80RegisterC Int
    | Z80ChangeFlags FlagRegisters
    | Z80ChangeSetIndirect Int Int CpuTimeIncrement


type FlagChange
    = OnlyFlags FlagRegisters
    | FlagChangeB Int
    | FlagChangeC Int
    | FlagChangeD Int
    | FlagChangeE Int
    | FlagChangeH Int
    | FlagChangeL Int
    | ReturnWithPop CpuTimeIncrement
    | EmptyFlagChange CpuTimeIncrement
    | FlagChangePush Int


applyZ80Change : Z80Change -> InstructionLength -> CpuTimeCTime -> Z80 -> Z80Transform
applyZ80Change change new_pc cpu_time z80 =
    case change of
        FlagsWithBRegister intWithFlags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | flags = intWithFlags.flags, main = { main | b = intWithFlags.value } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsB intWithFlags.value) intWithFlags.flags }

        FlagsWithCRegister intWithFlags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | flags = intWithFlags.flags, main = { main | c = intWithFlags.value } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsC intWithFlags.value) intWithFlags.flags }

        FlagsWithDRegister intWithFlags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | flags = intWithFlags.flags, main = { main | d = intWithFlags.value } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsD intWithFlags.value) intWithFlags.flags }

        FlagsWithERegister intWithFlags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | flags = flagRegisters, main = { main | e = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsE intWithFlags.value) intWithFlags.flags }

        HLRegister int flags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | hl = int }, flags = flags }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsHL int) flags }

        IXRegister int flags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | ix = int }, flags = flags }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIX int) flags }

        IYRegister int flags ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | iy = int }, flags = flags }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIY int) flags }

        FlagsWithHLRegister flagRegisters int time ->
            --let
            --    main =
            --        z80.main
            --
            --    env =
            --        z80.env |> addCpuTimeEnvInc time
            --in
            --{ z80 | env = env, flags = flagRegisters, main = { main | hl = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsHL int) flagRegisters }

        Z80RegisterB int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | b = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeBRegister int) }

        Z80RegisterC int ->
            --let
            --    main =
            --        z80.main
            --in
            --{ z80 | main = { main | c = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMain (ChangeCRegister int) }

        Z80ChangeFlags flagRegisters ->
            --{ z80 | flags = flagRegisters }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeFlagRegisters flagRegisters }

        Z80ChangeSetIndirect addr value time ->
            --let
            --    env =
            --        z80.env |> setMem addr value |> addCpuTimeEnvInc time
            --in
            --{ z80 | env = env }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeEnv (Store8BitMemoryValue addr value) }

        FlagsWithIXRegister flagRegisters int cpuTimeIncrement ->
            --let
            --    main =
            --        z80.main
            --
            --    env =
            --        z80.env |> addCpuTimeEnvInc cpuTimeIncrement
            --in
            --{ z80 | env = env, flags = flagRegisters, main = { main | ix = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIX int) flagRegisters }

        FlagsWithIYRegister flagRegisters int cpuTimeIncrement ->
            --let
            --    main =
            --        z80.main
            --
            --    env =
            --        z80.env |> addCpuTimeEnvInc cpuTimeIncrement
            --in
            --{ z80 | env = env, flags = flagRegisters, main = { main | iy = int } }
            { pcIncrement = new_pc, time = cpu_time, timeIncrement = FourTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIY int) flagRegisters }
