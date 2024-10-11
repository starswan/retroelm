module Group0xE0 exposing (..)

import Dict exposing (Dict)
import GroupED exposing (group_ed)
import Z80Delta exposing (Z80Delta(..), rst_delta)
import Z80Env exposing (addCpuTimeEnv, pop, z80_push)
import Z80Flags exposing (z80_and, z80_xor)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), Z80, get_de, get_xy, imm8, set_de_main)


delta_dict_E0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_E0 =
    Dict.fromList
        [ ( 0xE1, execute_0xE1 )
        , ( 0xE3, execute_0xE3 )
        , ( 0xE5, execute_0xE5 )
        , ( 0xE9, execute_0xE9 )
        ]


delta_dict_lite_E0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_E0 =
    Dict.fromList
        [ ( 0xE6, execute_0xE6 )
        , ( 0xE7, execute_0xE7 )
        , ( 0xEB, execute_0xEB )
        , ( 0xED, group_ed )
        , ( 0xEE, execute_0xEE )
        , ( 0xEF, execute_0xEF )
        ]


execute_0xE1 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE1 ixiyhl rom48k z80 =
    -- case 0xE1: HL=pop(); break;
    -- case 0xE1: xy=pop(); break;
    let
        hl =
            z80.env |> pop rom48k

        --env = z80.env
        --z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
        main =
            z80.main
    in
    case ixiyhl of
        IX ->
            MainRegsWithSpPcAndTime { main | ix = hl.value } hl.sp z80.pc hl.time

        IY ->
            MainRegsWithSpPcAndTime { main | iy = hl.value } hl.sp z80.pc hl.time

        HL ->
            MainRegsWithSpAndTime { main | hl = hl.value } hl.sp hl.time


execute_0xE3 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE3 ixiyhl rom48k z80 =
    -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
    -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
    let
        hl =
            z80.env |> pop rom48k

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


execute_0xE5 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE5 ixiyhl _ z80 =
    -- case 0xE5: push(HL); break;
    -- case 0xE5: push(xy); break;
    let
        pushed =
            z80.env |> z80_push (z80.main |> get_xy ixiyhl)
    in
    --{ z80 | env = pushed }
    EnvWithPc pushed z80.pc


execute_0xE6 : Z80ROM -> Z80 -> Z80Delta
execute_0xE6 rom48k z80 =
    -- case 0xE6: and(imm8()); break;
    let
        a =
            z80.env |> imm8 rom48k z80.pc

        env_1 =
            z80.env

        z80_1 =
            { z80 | env = { env_1 | time = a.time }, pc = a.pc }

        flags =
            z80_1.flags |> z80_and a.value
    in
    --{ z80_1 | flags = flags }
    FlagsWithPcAndTime flags a.pc a.time


execute_0xE7 : Z80ROM -> Z80 -> Z80Delta
execute_0xE7 _ z80 =
    z80 |> rst_delta 0xE7


execute_0xE9 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE9 ixiyhl _ z80 =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    let
        xy =
            z80.main |> get_xy ixiyhl

        --a = if Dict.member xy Z80Rom.c_COMMON_NAMES then
        --      Nothing
        --    else
        --      debug_log ("JP (" ++ (toString ixiyhl) ++ ")") (xy |> subName) Nothing
    in
    --{ z80 | pc = xy }
    OnlyPc xy


execute_0xEB : Z80ROM -> Z80 -> Z80Delta
execute_0xEB _ z80 =
    -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
    let
        v =
            z80.main.hl

        de =
            z80.main |> get_de

        --x = debug_log "EX DE,HL" ("DE " ++ (v |> toHexString) ++ " HL " ++ (de |> toHexString)) Nothing
        main =
            z80.main |> set_de_main v
    in
    --z80 |> set_de v |> set_hl de
    MainRegs { main | hl = de }


execute_0xEE : Z80ROM -> Z80 -> Z80Delta
execute_0xEE rom48k z80 =
    -- case 0xEE: xor(imm8()); break;
    let
        v =
            z80.env |> imm8 rom48k z80.pc

        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80.flags |> z80_xor v.value
    in
    --{ z80_1 | flags = flags }
    FlagsWithPcAndTime flags v.pc v.time


execute_0xEF : Z80ROM -> Z80 -> Z80Delta
execute_0xEF _ z80 =
    z80 |> rst_delta 0xEF
