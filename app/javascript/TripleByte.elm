module TripleByte exposing (..)

import Dict exposing (Dict)
import Z80Address exposing (Z80Address, toInt)
import PCIncrement exposing (PCIncrement(..), TriplePCIncrement(..))
import Z80Address exposing (Z80Address)


type TripleByteChange
    = NewBCRegister Int
    | NewDERegister Int
    | NewHLRegister Z80Address
    | NewHLIndirect Z80Address
    | NewIXRegister Z80Address
    | NewIXIndirect Z80Address
    | NewIYRegister Z80Address
    | NewIYIndirect Z80Address
    | NewSPRegister Z80Address
    | NewPCRegister Z80Address
    | CallImmediate Z80Address


tripleByteWith16BitParam : Dict Int ( Z80Address -> TripleByteChange, TriplePCIncrement )
tripleByteWith16BitParam =
    Dict.fromList
        [ ( 0x01, ( ld_bc_nn, IncrementByThree ) )
        , ( 0x11, ( ld_de_nn, IncrementByThree ) )
        , ( 0x21, ( ld_hl_nn, IncrementByThree ) )
        , ( 0xDD21, ( ld_ix_nn, IncrementByFour ) )
        , ( 0xFD21, ( ld_iy_nn, IncrementByFour ) )
        , ( 0x2A, ( ld_hl_indirect_nn, IncrementByThree ) )
        , ( 0xDD2A, ( ld_ix_indirect_nn, IncrementByFour ) )
        , ( 0xFD2A, ( ld_iy_indirect_nn, IncrementByFour ) )
        , ( 0x31, ( ld_sp_nn, IncrementByThree ) )
        , ( 0xC3, ( jp_nn, IncrementByThree ) )
        , ( 0xCD, ( call_0xCD, IncrementByThree ) )
        ]


ld_bc_nn : Z80Address -> TripleByteChange
ld_bc_nn param16 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    NewBCRegister (param16 |> toInt)


ld_de_nn : Z80Address -> TripleByteChange
ld_de_nn param16 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    NewDERegister (param16 |> toInt)


ld_hl_nn : Z80Address -> TripleByteChange
ld_hl_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewHLRegister (param16)


ld_ix_nn : Z80Address -> TripleByteChange
ld_ix_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIXRegister (param16)


ld_iy_nn : Z80Address -> TripleByteChange
ld_iy_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    NewIYRegister (param16 )


ld_sp_nn : Z80Address -> TripleByteChange
ld_sp_nn param16 =
    -- case 0x31: SP=imm16(); break;
    NewSPRegister (param16 )


jp_nn : Z80Address -> TripleByteChange
jp_nn param16 =
    -- case 0xC3: MP=PC=imm16(); break;
    NewPCRegister (param16 )

call_0xCD : Z80Address -> TripleByteChange
call_0xCD param16 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    CallImmediate (param16)


ld_hl_indirect_nn : Z80Address -> TripleByteChange
ld_hl_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    NewHLIndirect param16


ld_ix_indirect_nn : Z80Address -> TripleByteChange
ld_ix_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIXIndirect param16


ld_iy_indirect_nn : Z80Address -> TripleByteChange
ld_iy_indirect_nn param16 =
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    NewIYIndirect param16
