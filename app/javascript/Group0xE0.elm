module Group0xE0 exposing (..)

import Bitwise
import Dict exposing (Dict)
import GroupED exposing (group_ed)
import Utils exposing (shiftLeftBy8)
import Z80Address exposing (fromInt, toInt)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, out, z80_in, z80_pop, z80_push)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), Z80, get_de, get_xy, get_xy_ixiy, imm8, set_de_main)


miniDictE0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDictE0 =
    Dict.fromList
        [ ( 0xE1, pop_hl )
        , ( 0xE5, push_hl )
        , ( 0xE9, jp_hl )
        , ( 0xEB, ex_de_hl )
        ]


delta_dict_E0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_E0 =
    Dict.fromList
        [ ( 0xE3, ex_indirect_sp_hl )
        ]


delta_dict_lite_E0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_E0 =
    Dict.fromList
        [ ( 0xD3, execute_0xD3 )
        , ( 0xDB, execute_0xDB )
        , ( 0xED, group_ed )
        ]


pop_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
pop_hl ixiyhl rom48k z80 =
    -- case 0xE1: HL=pop(); break;
    -- case 0xE1: xy=pop(); break;
    let
        hl =
            z80.env |> z80_pop rom48k

        --env = z80.env
        --z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
        main =
            z80.main
    in
    case ixiyhl of
        IXIY_IX ->
            MainRegsWithSpPcAndTime { main | ix = hl.value } hl.sp z80.pc hl.time

        IXIY_IY ->
            MainRegsWithSpPcAndTime { main | iy = hl.value } hl.sp z80.pc hl.time


ex_indirect_sp_hl : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
ex_indirect_sp_hl ixiyhl rom48k z80 =
    -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
    -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
    let
        hl =
            z80.env |> z80_pop rom48k

        env =
            z80.env

        z80_1 =
            { z80 | env = { env | time = hl.time, sp = hl.sp } }

        pushed =
            z80_1.env |> z80_push (z80_1.main |> get_xy ixiyhl) |> addCpuTimeEnv 2

        --z80_2 = { z80_1 | env = pushed }
        main =
            z80_1.main
    in
    case ixiyhl of
        --IX -> { z80_2 | main = { main | ix = v.value } }
        --IY -> { z80_2 | main = { main | iy = v.value } }
        --HL -> { z80_2 | main = { main | hl = v.value } }
        IX ->
            MainRegsWithEnvAndPc { main | ix = hl.value } pushed z80.pc

        IY ->
            MainRegsWithEnvAndPc { main | iy = hl.value } pushed z80.pc

        HL ->
            MainRegsWithEnv { main | hl = hl.value } pushed


push_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
push_hl ixiyhl _ z80 =
    -- case 0xE5: push(HL); break;
    -- case 0xE5: push(xy); break;
    let
        pushed =
            z80.env |> z80_push (z80.main |> get_xy_ixiy ixiyhl)
    in
    --{ z80 | env = pushed }
    EnvWithPc pushed z80.pc


jp_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
jp_hl ixiyhl _ z80 =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    let
        xy =
            z80.main |> get_xy_ixiy ixiyhl

        --a = if Dict.member xy Z80Rom.c_COMMON_NAMES then
        --      Nothing
        --    else
        --      debug_log ("JP (" ++ (toString ixiyhl) ++ ")") (xy |> subName) Nothing
    in
    --{ z80 | pc = xy }
    OnlyPc xy


ex_de_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ex_de_hl ixiyhl _ z80 =
    -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
    let
        v =
            z80.main |> get_xy_ixiy ixiyhl |> toInt

        de =
            z80.main |> get_de

        --x = debug_log "EX DE,HL" ("DE " ++ (v |> toHexString) ++ " HL " ++ (de |> toHexString)) Nothing
        main =
            z80.main |> set_de_main v
    in
    --z80 |> set_de v |> set_hl de
    case ixiyhl of
        IXIY_IX ->
            MainRegs { main | ix = de |> fromInt }

        IXIY_IY ->
            MainRegs { main | iy = de |> fromInt }


execute_0xD3 : Z80ROM -> Z80 -> Z80Delta
execute_0xD3 rom48k z80 =
    -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
    let
        value =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        env_1 =
            z80.env

        env_2 =
            { env_1 | time = value.time }

        v =
            Bitwise.or value.value (shiftLeftBy8 z80.flags.a)

        env =
            out v z80.flags.a env_2 |> addCpuTimeEnv 4
    in
    EnvWithPc env value.pc


execute_0xDB : Z80ROM -> Z80 -> Z80Delta
execute_0xDB rom48k z80 =
    -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
    let
        imm8val =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        env_1 =
            z80.env

        z80_1 =
            { z80 | env = { env_1 | time = imm8val.time }, pc = imm8val.pc }

        v =
            Bitwise.or imm8val.value (shiftLeftBy8 z80_1.flags.a)

        a =
            z80_1.env |> z80_in v

        flags =
            z80_1.flags

        new_flags =
            { flags | a = a.value }
    in
    --{ z80_1 | env = a.env, flags = { flags | a = a.value } }
    CpuTimeWithFlagsAndPc imm8val.time new_flags imm8val.pc
