module TripleByte exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import TransformTypes exposing (InstructionDuration(..))
import Z80Env exposing (mem, mem16)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), ChangeMainOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (Z80)


type TripleByteChange
    = NewBCRegister Int
    | NewDERegister Int
    | NewHLRegister Int
    | NewHLIndirect Int
    | NewIXRegister Int
    | NewIXIndirect Int
    | NewIYRegister Int
    | NewIYIndirect Int
    | NewSPRegister Int
    | NewPCRegister Int
    | CallImmediate Int
    | NewAIndirect Int


standardTriple16Bit : Dict Int ( Int -> TripleByteChange, TriplePCIncrement )
standardTriple16Bit =
    Dict.fromList
        [ ( 0x01, ( ld_bc_nn, IncrementByThree ) )
        , ( 0x11, ( ld_de_nn, IncrementByThree ) )
        , ( 0x21, ( ld_hl_nn, IncrementByThree ) )
        , ( 0x2A, ( ld_hl_indirect_nn, IncrementByThree ) )
        , ( 0x31, ( ld_sp_nn, IncrementByThree ) )
        , ( 0xC3, ( jp_nn, IncrementByThree ) )
        , ( 0xCD, ( call_0xCD, IncrementByThree ) )
        , ( 0x3A, ( ld_a_indirect_nn, IncrementByThree ) )
        ]

ixTriple16Bit : Dict Int ( Int -> TripleByteChange, TriplePCIncrement )
ixTriple16Bit =
    Dict.fromList
        [ ( 0xDD21, ( ld_ix_nn, IncrementByFour ) )
        , ( 0xDD2A, ( ld_ix_indirect_nn, IncrementByFour ) )
        ]

iyTriple16Bit : Dict Int ( Int -> TripleByteChange, TriplePCIncrement )
iyTriple16Bit =
    Dict.fromList
        [ ( 0xFD21, ( ld_iy_nn, IncrementByFour ) )
        , ( 0xFD2A, ( ld_iy_indirect_nn, IncrementByFour ) )
        ]

cbTriple16Bit : Dict Int ( Int -> TripleByteChange, TriplePCIncrement )
cbTriple16Bit =
    Dict.fromList
        [
        ]


parseTriple16Param : Dict Int ( Int -> TripleByteChange, TriplePCIncrement ) -> Int -> CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseTriple16Param operationDict paramOffset _ instrCode rom48k z80  =
    case operationDict |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k

                new_pc =
                    case pcInc of
                        IncrementByThree ->
                            --Bitwise.and (z80.pc + 3) 0xFFFF
                            ThreeByteInstruction

                        IncrementByFour ->
                            --Bitwise.and (z80.pc + 4) 0xFFFF
                            FourByteInstruction
            in
            case f doubleParam.value of
                NewBCRegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeBCRegister int) }

                NewDERegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeDERegister int) }

                NewHLRegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeHLRegister int) }

                NewSPRegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeEnv (ChangeSPRegister int) }

                NewPCRegister int ->
                    Just { pcIncrement = JumpInstruction int, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeNothing }

                CallImmediate int ->
                    let
                        pc_value =
                            case pcInc of
                                IncrementByThree ->
                                    Bitwise.and (z80.pc + 3) 0xFFFF

                                IncrementByFour ->
                                    Bitwise.and (z80.pc + 4) 0xFFFF
                    in
                    Just { pcIncrement = JumpInstruction int, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeEnv (PushValue pc_value) }

                NewIXRegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeIXRegister int) }

                NewIYRegister int ->
                    Just { pcIncrement = new_pc, time = doubleParam.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeIYRegister int) }

                NewHLIndirect int ->
                    let
                        env =
                            z80.env

                        value =
                            { env | time = doubleParam.time } |> mem16 int rom48k
                    in
                    Just { pcIncrement = new_pc, time = value.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeHLRegister value.value) }

                NewIXIndirect int ->
                    let
                        env =
                            z80.env

                        value =
                            { env | time = doubleParam.time } |> mem16 int rom48k
                    in
                    Just { pcIncrement = new_pc, time = value.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeIXRegister value.value) }

                NewIYIndirect int ->
                    let
                        env =
                            z80.env

                        value =
                            { env | time = doubleParam.time } |> mem16 int rom48k
                    in
                    Just { pcIncrement = new_pc, time = value.time, timeIncrement = TenTStates, operation = ChangeMain (ChangeIYRegister value.value) }

                NewAIndirect int ->
                    let
                        env =
                            z80.env

                        value =
                            mem int doubleParam.time rom48k env.ram

                        flags =
                            z80.flags
                    in
                    Just { pcIncrement = new_pc, time = value.time, timeIncrement = TenTStates, operation = ChangeFlagRegisters { flags | a = value.value } }

        Nothing ->
            Nothing


ld_bc_nn : Int -> TripleByteChange
ld_bc_nn param16 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    NewBCRegister param16


ld_de_nn : Int -> TripleByteChange
ld_de_nn param16 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    NewDERegister param16


ld_hl_nn : Int -> TripleByteChange
ld_hl_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewHLRegister param16


ld_ix_nn : Int -> TripleByteChange
ld_ix_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIXRegister param16


ld_iy_nn : Int -> TripleByteChange
ld_iy_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIYRegister param16


ld_sp_nn : Int -> TripleByteChange
ld_sp_nn param16 =
    -- case 0x31: SP=imm16(); break;
    NewSPRegister param16


jp_nn : Int -> TripleByteChange
jp_nn param16 =
    -- case 0xC3: MP=PC=imm16(); break;
    NewPCRegister param16


call_0xCD : Int -> TripleByteChange
call_0xCD param16 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    CallImmediate param16


ld_hl_indirect_nn : Int -> TripleByteChange
ld_hl_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    NewHLIndirect param16


ld_ix_indirect_nn : Int -> TripleByteChange
ld_ix_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIXIndirect param16


ld_iy_indirect_nn : Int -> TripleByteChange
ld_iy_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIYIndirect param16


ld_a_indirect_nn : Int -> TripleByteChange
ld_a_indirect_nn param16 =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    NewAIndirect param16
