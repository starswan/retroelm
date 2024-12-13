module TripleWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime)
import Dict exposing (Dict)
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (byte, shiftRightBy8)
import Z80Env exposing (mem16)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeEnvOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type TripleMainChange
    = Store16BitValue Int Int
    | Store8BitValue Int Int


standardTripleMain : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength )
standardTripleMain =
    Dict.fromList
        [ ( 0x22, ( ld_nn_indirect_hl, ThreeByteInstruction ) )
        ]

ixTripleMain : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength )
ixTripleMain =
    Dict.fromList
        [ ( 0xDD22, ( ld_nn_indirect_ix, FourByteInstruction ) )
        , ( 0xDD36, ( ld_indirect_ix_n, FourByteInstruction ) )
        ]

iyTripleMain : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength )
iyTripleMain =
    Dict.fromList
        [ ( 0xFD22, ( ld_nn_indirect_iy, FourByteInstruction ) )
        , ( 0xFD36, ( ld_indirect_iy_n, FourByteInstruction ) )
        ]

cbTripleMain : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength )
cbTripleMain =
    Dict.fromList
        [
        ]


parseTripleMain : Dict Int ( Int -> MainWithIndexRegisters -> TripleMainChange, InstructionLength ) -> Int -> CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseTripleMain operationDict paramOffset _ instrCode rom48k z80 =
    case operationDict |> Dict.get instrCode of
        Just ( f, new_pc ) ->
            let
                doubleParam =
                    z80.env |> mem16 (Bitwise.and (z80.pc + paramOffset) 0xFFFF) rom48k
            in
            case f doubleParam.value z80.main of
                Store16BitValue addr data ->
                    Just
                        { pcIncrement = new_pc
                        , time = doubleParam.time
                        , timeIncrement = SixTStates
                        , operation = ChangeEnv (Store16BitMemoryValue addr data)
                        }

                Store8BitValue addr data ->
                    Just
                        { pcIncrement = new_pc
                        , time = doubleParam.time
                        , timeIncrement = SevenTStates
                        , operation = ChangeEnv (Store8BitMemoryValue addr data)
                        }

        Nothing ->
            Nothing


ld_nn_indirect_hl : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_hl param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
    Store16BitValue param16 z80_main.hl


ld_nn_indirect_ix : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_ix param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 z80_main.ix


ld_nn_indirect_iy : Int -> MainWithIndexRegisters -> TripleMainChange
ld_nn_indirect_iy param16 z80_main =
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    Store16BitValue param16 z80_main.iy


ld_indirect_ix_n : Int -> MainWithIndexRegisters -> TripleMainChange
ld_indirect_ix_n param16 z80_main =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF |> byte

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue ((z80_main.ix + offset) |> Bitwise.and 0xFFFF) value


ld_indirect_iy_n : Int -> MainWithIndexRegisters -> TripleMainChange
ld_indirect_iy_n param16 z80_main =
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    let
        offset =
            param16 |> Bitwise.and 0xFF |> byte

        value =
            param16 |> shiftRightBy8
    in
    Store8BitValue ((z80_main.iy + offset) |> Bitwise.and 0xFFFF) value
