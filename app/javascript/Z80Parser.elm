module Z80Parser exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict
import PCIncrement exposing (MediumPCIncrement(..), TriplePCIncrement)
import SingleWith8BitParameter exposing (DoubleWithRegisterChange(..), JumpChange(..), Single8BitChange(..), applySimple8BitDelta, maybeRelativeJump, singleWith8BitParam)
import TransformTypes exposing (InstructionDuration(..))
import Z80Env exposing (mem)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (Z80)


parseRelativeJump : CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseRelativeJump instrTime instrCode rom48k z80 =
    case maybeRelativeJump |> Dict.get instrCode of
        Just f ->
            let
                param =
                    mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram
            in
            case f param.value z80.flags of
                ActualJump jump ->
                    Just
                        { pcIncrement = JumpInstruction (Bitwise.and (z80.pc + 2 + jump) 0xFFFF)
                        , time = instrTime
                        , timeIncrement = EightTStates
                        , operation = ChangeNothing
                        }

                NoJump ->
                    Just
                        { pcIncrement = TwoByteInstruction
                        , time = instrTime
                        , timeIncrement = EightTStates
                        , operation = ChangeNothing
                        }

                FlagJump flagRegisters ->
                    Just
                        { pcIncrement = TwoByteInstruction
                        , time = instrTime
                        , timeIncrement = ZeroTStates
                        , operation = ChangeFlagRegisters flagRegisters
                        }

        Nothing ->
            Nothing




parseSingleByteWithParam : CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseSingleByteWithParam ctime instr_code rom48k z80 =
    case singleWith8BitParam |> Dict.get instr_code of
        Just ( f, pcInc ) ->
            let
                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ctime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (z80.pc + 2) 0xFFFF) ctime rom48k z80.env.ram

                -- duplicate of code in imm8 - add 3 to the cpu_time
                x =
                    z80 |> applySimple8BitDelta pcInc (param.time |> addCpuTimeTime 3) (f param.value)
            in
            Just x

        Nothing ->
            Nothing
