module Z80Parser exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict
import PCIncrement exposing (MediumPCIncrement(..), TriplePCIncrement)
import SingleWith8BitParameter exposing (doubleWithRegisters, maybeRelativeJump)
import TripleByte exposing (tripleByteWith16BitParam)
import TripleWithFlags exposing (triple16WithFlags)
import TripleWithMain exposing (TripleMainChange, applyTripleMainChange, tripleMainRegs)
import Z80Env exposing (mem, mem16)
import Z80Execute exposing (DeltaWithChanges(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80)


parseTripleMain : Int -> Z80ROM -> Int -> Z80 -> Maybe Z80
parseTripleMain instrCode rom48k paramOffset z80 =
    case tripleMainRegs |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k

                -- duplicate of code in imm16 - add 6 to the cpu_time
                x =
                    z80 |> applyTripleMainChange (doubleParam.time |> addCpuTimeTime 6) pcInc (f doubleParam.value z80.main)
            in
            Just x

        Nothing ->
            Nothing


parseTriple16Flags : Int -> Z80ROM -> Int -> Z80 -> Maybe DeltaWithChanges
parseTriple16Flags instrCode rom48k paramOffset z80 =
    case triple16WithFlags |> Dict.get instrCode of
        Just f ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (TripleFlagDelta (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value z80.flags))

        Nothing ->
            Nothing


parseTriple16Param : Int -> Z80ROM -> Int -> Z80 -> Maybe DeltaWithChanges
parseTriple16Param instrCode rom48k paramOffset z80 =
    case tripleByteWith16BitParam |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            -- duplicate of code in imm16 - add 6 to the cpu_time
            Just (TripleChangeDelta pcInc (doubleParam.time |> addCpuTimeTime 6) (f doubleParam.value))

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
