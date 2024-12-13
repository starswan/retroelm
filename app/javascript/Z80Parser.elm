module Z80Parser exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict
import PCIncrement exposing (MediumPCIncrement(..), TriplePCIncrement)
import SingleByteWithEnv exposing (singleByteZ80Env)
import SingleEnvWithMain exposing (singleEnvMainRegs)
import SingleNoParams exposing (singleWithNoParam)
import SingleWith8BitParameter exposing (doubleWithRegisters, maybeRelativeJump, singleWith8BitParam)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange, tripleMainRegs)
import Z80Env exposing (mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..), applyTripleChangeDelta, applyTripleFlagChange)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80)


parseTripleMain : Int -> Z80ROM -> Int -> Z80 -> Maybe Z80
parseTripleMain instrCode rom48k paramOffset z80 =
    case tripleMainRegs |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (z80 |> applyTripleMainChange (doubleParam.time |> addCpuTimeTime 6) pcInc (f doubleParam.value z80.main))

        Nothing ->
            Nothing


parseTriple16Flags : Int -> Z80ROM -> Int -> Z80 -> Maybe Z80
parseTriple16Flags instrCode rom48k paramOffset z80 =
    case triple16WithFlags |> Dict.get instrCode of
        Just f ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (z80 |> applyTripleFlagChange (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value z80.flags))

        Nothing ->
            Nothing


parseTriple16Param : Int -> Z80ROM -> Int -> Z80 -> Maybe Z80
parseTriple16Param instrCode rom48k paramOffset z80 =
    case tripleByteWith16BitParam |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            --Just (TripleChangeDelta pcInc (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value))
            Just (z80 |> applyTripleChangeDelta rom48k pcInc (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value))

        Nothing ->
            Nothing


parseRelativeJump : Int -> Z80ROM -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseRelativeJump instrCode rom48k instrTime z80 =
    case maybeRelativeJump |> Dict.get instrCode of
        Just f ->
            let
                param =
                    mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (JumpChangeDelta (param.time |> addCpuTimeTime 3) (f param.value z80.flags))

        Nothing ->
            Nothing


parseDoubleWithRegs : Int -> Z80ROM -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseDoubleWithRegs instrCode rom48k instrTime z80 =
    case doubleWithRegisters |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) instrTime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (z80.pc + 2) 0xFFFF) instrTime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (DoubleWithRegistersDelta pcInc (param.time |> addCpuTimeTime 3) (f z80.main param.value))

        Nothing ->
            Nothing


parseSingleEnvMain : Int -> Z80ROM -> Z80 -> Maybe DeltaWithChanges
parseSingleEnvMain instr_code rom48k z80 =
    case singleEnvMainRegs |> Dict.get instr_code of
        Just ( f, pcInc ) ->
            Just (MainWithEnvDelta pcInc (f z80.main rom48k z80.env))

        Nothing ->
            Nothing


parseSingleEnv : Int -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseSingleEnv instr_code ctime z80 =
    case singleByteZ80Env |> Dict.get instr_code of
        Just f ->
            Just (SingleEnvDelta ctime (f z80.env))

        Nothing ->
            Nothing


parseSingle : Int -> CpuTimeCTime -> Z80 -> Maybe DeltaWithChanges
parseSingle instr_code ctime z80 =
    case singleWithNoParam |> Dict.get instr_code of
        Just f ->
            Just (NoParamsDelta ctime f)

        Nothing ->
            Nothing


parseSingleWithParam : Int -> CpuTimeCTime -> Z80ROM -> Z80 -> Maybe DeltaWithChanges
parseSingleWithParam instr_code ctime rom48k z80 =
    case singleWith8BitParam |> Dict.get instr_code of
        Just ( f, pcInc ) ->
            let
                param =
                    case pcInc of
                        IncreaseByTwo ->
                            mem (Bitwise.and (z80.pc + 1) 0xFFFF) ctime rom48k z80.env.ram

                        IncreaseByThree ->
                            mem (Bitwise.and (z80.pc + 2) 0xFFFF) ctime rom48k z80.env.ram
            in
            -- duplicate of code in imm8 - add 3 to the cpu_time
            Just (Simple8BitDelta pcInc (param.time |> addCpuTimeTime 3) (f param.value))

        Nothing ->
            Nothing
