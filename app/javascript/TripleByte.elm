module TripleByte exposing (..)

import Dict exposing (Dict)


type TripleByteChange
    = NewBCRegister Int
    | NewDERegister Int
    | NewHLRegister Int
    | NewSPRegister Int
    | NewPCRegister Int
    | CallImmediate Int


tripleByteWith16BitParam : Dict Int (Int -> TripleByteChange)
tripleByteWith16BitParam =
    Dict.fromList
        [ ( 0x01, ld_bc_nn )
        , ( 0x11, ld_de_nn )
        , ( 0x21, ld_hl_nn )
        , ( 0x31, ld_sp_nn )
        , ( 0xC3, jp_nn )
        , ( 0xCD, call_0xCD )
        ]


ld_bc_nn : Int -> TripleByteChange
ld_bc_nn param16 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    --let
    --    v =
    --        z80 |> imm16 rom48k
    --
    --    z80main =
    --        z80.main |> set_bc_main v.value
    --in
    NewBCRegister param16


ld_de_nn : Int -> TripleByteChange
ld_de_nn param16 =
    --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
    --let
    --    v =
    --        z80 |> imm16 rom48k
    --
    --    main_regs =
    --        z80.main |> set_de_main v.value
    --in
    NewDERegister param16


ld_hl_nn : Int -> TripleByteChange
ld_hl_nn param16 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    --let
    --    new_xy =
    --        z80 |> imm16 rom48k
    --
    --    --z80_1 = { z80 | env = new_xy.env, pc = new_xy.pc }
    --    --x = debug_log ("LD " ++ (ixiyhl |> toString) ++ "," ++ (new_xy.value |> toHexString)) ("pc = " ++ (z80.pc |> toHexString)) Nothing
    --    main =
    --        z80.main |> set_xy_ixiy new_xy.value ixiyhl
    --in
    ----{ z80_1 | main = main }
    NewHLRegister param16


ld_sp_nn : Int -> TripleByteChange
ld_sp_nn param16 =
    -- case 0x31: SP=imm16(); break;
    --let
    --    v =
    --        z80 |> imm16 rom48k
    --in
    ----{ z80 | env = v.env, pc = v.pc, sp = v.value }
    NewSPRegister param16


jp_nn : Int -> TripleByteChange
jp_nn param16 =
    -- case 0xC3: MP=PC=imm16(); break;
    --let
    --    v =
    --        z80 |> imm16 rom48k
    --
    --    --env = z80.env
    --    --z80_1 = { z80 | pc = v.pc, env = { env | time = v.time } }
    --    --y = debug_log "jp" (v.value |> subName) Nothing
    --in
    ----z80_1 |> set_pc v.value
    NewPCRegister param16

call_0xCD : Int -> TripleByteChange
call_0xCD param16 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    --let
    --    v =
    --        z80 |> imm16 rom48k
    --
    --    --env = z80.env
    --    --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
    --    --pushed = { env | time = v.time } |> z80_push v.pc
    --in
    ----{ z80_1 | env = pushed, pc = v.value }
    --PushWithCpuTimeAndPc v.pc v.time v.value
   CallImmediate param16