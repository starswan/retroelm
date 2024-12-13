module TripleWithFlags exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import Dict exposing (Dict)
import TransformTypes exposing (InstructionDuration(..))
import Z80Env exposing (mem16)
import Z80Flags exposing (FlagRegisters, c_FP, c_FS, get_flags)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (Z80)


type TripleWithFlagsChange
    = Skip3ByteInstruction
    | AbsoluteJump Int
    | TripleSetIndirect Int Int
    | AbsoluteCall Int


triple16WithFlags : Dict Int (Int -> FlagRegisters -> TripleWithFlagsChange)
triple16WithFlags =
    Dict.fromList
        [ ( 0x32, ld_indirect_nn_a )
        , ( 0xC2, jp_nz )
        , ( 0xC4, call_nz_nn )
        , ( 0xCA, jp_z_nn )
        , ( 0xCC, call_z_nn )
        , ( 0xD2, jp_nc_nn )
        , ( 0xDA, jp_c_nn )
        , ( 0xE2, jp_po_nn )
        , ( 0xEA, jp_pe_nn )
        , ( 0xF2, jp_p_nn )
        , ( 0xFA, jp_m_nn )
        ]


parseTriple16Flags : Int -> CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseTriple16Flags paramOffset _ instrCode rom48k z80 =
    case triple16WithFlags |> Dict.get instrCode of
        Just f ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            case f doubleParam.value z80.flags of
                --Just (applyTripleFlagChange doubleParam.time (f doubleParam.value z80.flags) z80)
                Skip3ByteInstruction ->
                    Just { pcIncrement = ThreeByteInstruction, time = doubleParam.time, timeIncrement = FourTStates, operation = ChangeNothing }

                AbsoluteJump int ->
                    Just { pcIncrement = JumpInstruction int, time = doubleParam.time, timeIncrement = FourTStates, operation = ChangeNothing }

                TripleSetIndirect addr value ->
                    Just { pcIncrement = ThreeByteInstruction, time = doubleParam.time, timeIncrement = SevenTStates, operation = ChangeEnv (Store16BitMemoryValue addr value) }

                AbsoluteCall int ->
                    Just { pcIncrement = JumpInstruction int, time = doubleParam.time, timeIncrement = FourTStates, operation = ChangeEnv (PushValue (Bitwise.and (z80.pc + 3) 0xFFFF)) }

        Nothing ->
            Nothing


jp_nz : Int -> FlagRegisters -> TripleWithFlagsChange
jp_nz param z80_flags =
    -- case 0xC2: jp(Fr!=0); break;
    --jp_z80 (z80.flags.fr /= 0) z80
    if z80_flags.fr /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_z_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_z_nn param z80_flags =
    -- case 0xCA: jp(Fr==0); break;
    --jp_z80 (z80.flags.fr == 0) z80
    if z80_flags.fr == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_nc_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_nc_nn param z80_flags =
    -- case 0xD2: jp((Ff&0x100)==0); break;
    --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) == 0)
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_c_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_c_nn param z80_flags =
    -- case 0xDA: jp((Ff&0x100)!=0); break;
    --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) /= 0)
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_po_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_po_nn param z80_flags =
    -- case 0xE2: jp((flags()&FP)==0); break;
    if Bitwise.and (z80_flags |> get_flags) c_FP == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_pe_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_pe_nn param z80_flags =
    -- case 0xEA: jp((flags()&FP)!=0); break;
    if Bitwise.and (z80_flags |> get_flags) c_FP /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_p_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_p_nn param z80_flags =
    -- case 0xF2: jp((Ff&FS)==0); break;
    if Bitwise.and z80_flags.ff c_FS == 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


jp_m_nn : Int -> FlagRegisters -> TripleWithFlagsChange
jp_m_nn param z80_flags =
    -- case 0xFA: jp((Ff&FS)!=0); break;
    if Bitwise.and z80_flags.ff c_FS /= 0 then
        AbsoluteJump param

    else
        Skip3ByteInstruction


ld_indirect_nn_a : Int -> FlagRegisters -> TripleWithFlagsChange
ld_indirect_nn_a param z80_flags =
    -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    TripleSetIndirect param z80_flags.a


call_nz_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_nz_nn param z80_flags =
    -- case 0xC4: call(Fr!=0); break;
    --call_z80 (z80.flags.fr /= 0) z80
    if z80_flags.fr /= 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction


call_z_nn : Int -> FlagRegisters -> TripleWithFlagsChange
call_z_nn param z80_flags =
    -- case 0xCC: call(Fr==0); break;
    --call_z80 (z80.flags.fr == 0) z80
    if z80_flags.fr == 0 then
        AbsoluteCall param

    else
        Skip3ByteInstruction
