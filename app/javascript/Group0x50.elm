module Group0x50 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..), delta_noop)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


delta_dict_50 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_50 =
    Dict.fromList
        [ ( 0x54, execute_0x54 )
        , ( 0x55, execute_0x55 )
        , ( 0x56, execute_0x56 )
        , ( 0x5C, execute_0x5C )
        , ( 0x5D, execute_0x5D )
        , ( 0x5E, execute_0x5E )
        ]


delta_dict_lite_50 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_50 =
    Dict.fromList
        [ -- case 0x40: break;
          ( 0x50, execute_0x50 )
        , ( 0x51, execute_0x51 )
        , ( 0x52, delta_noop )
        , ( 0x53, execute_0x53 )
        , ( 0x57, execute_0x57 )
        , ( 0x58, execute_0x58 )
        , ( 0x59, execute_0x59 )
        , ( 0x5A, execute_0x5A )
        , -- case 0x5B: break;
          ( 0x5B, delta_noop )
        , ( 0x5F, execute_0x5F )
        ]


execute_0x50 : Z80ROM -> Z80 -> Z80Delta
execute_0x50 rom z80 =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
    let
        main =
            z80.main
    in
    { main | d = z80.main.b } |> MainRegs


execute_0x51 : Z80ROM -> Z80 -> Z80Delta
execute_0x51 rom z80 =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
    let
        main =
            z80.main
    in
    { main | d = z80.main.c } |> MainRegs


execute_0x53 : Z80ROM -> Z80 -> Z80Delta
execute_0x53 rom z80 =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
    let
        main =
            z80.main
    in
    { main | d = z80.main.e } |> MainRegs


execute_0x54 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x54 ixiyhl rom z80 =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_h ixiyhl z80.main } |> MainRegs


execute_0x55 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x55 ixiyhl rom z80 =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_l ixiyhl z80.main } |> MainRegs


execute_0x56 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x56 ixiyhl rom48k z80 =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        main =
            z80.main

        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time


execute_0x57 : Z80ROM -> Z80 -> Z80Delta
execute_0x57 rom z80 =
    -- case 0x57: D=A; break;
    --z80 |> set_d z80.flags.a
    let
        main =
            z80.main
    in
    { main | d = z80.flags.a } |> MainRegs


execute_0x58 : Z80ROM -> Z80 -> Z80Delta
execute_0x58 rom z80 =
    -- case 0x58: E=B; break;
    --z80 |> set_e z80.main.b
    let
        main =
            z80.main
    in
    { main | e = z80.main.b } |> MainRegs


execute_0x59 : Z80ROM -> Z80 -> Z80Delta
execute_0x59 rom z80 =
    -- case 0x59: E=C; break;
    --z80 |> set_e z80.main.c
    let
        main =
            z80.main
    in
    { main | e = z80.main.c } |> MainRegs


execute_0x5A : Z80ROM -> Z80 -> Z80Delta
execute_0x5A rom z80 =
    -- case 0x5A: E=D; break;
    --z80 |> set_e z80.main.d
    let
        main =
            z80.main
    in
    { main | e = z80.main.d } |> MainRegs


execute_0x5C : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x5C ixiyhl rom z80 =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_h ixiyhl z80.main } |> MainRegs


execute_0x5D : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x5D ixiyhl rom z80 =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_l ixiyhl z80.main } |> MainRegs


execute_0x5E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x5E ixiyhl rom48k z80 =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time


execute_0x5F : Z80ROM -> Z80 -> Z80Delta
execute_0x5F rom z80 =
    -- case 0x5F: E=A; break;
    --z80 |> set_e z80.flags.a
    let
        main =
            z80.main
    in
    { main | e = z80.flags.a } |> MainRegs
