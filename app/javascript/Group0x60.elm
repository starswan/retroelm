module Group0x60 exposing (..)

import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), Z80, get_h, get_h_ixiy, get_l, get_l_ixiy, hl_deref_with_z80, set_h, set_h_ixiy, set_l, set_l_ixiy)


delta_dict_60 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_60 =
    Dict.fromList
        [ ( 0x66, execute_0x66 )
        , ( 0x6E, execute_0x6E )

        ]


miniDict60 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict60 =
    Dict.fromList
        [ ( 0x60, ld_h_b )
        , ( 0x61, ld_h_c )
        , ( 0x62, ld_h_d )
        , ( 0x63, ld_h_e )
        , ( 0x65, ld_h_l )
        , ( 0x67, ld_h_a )
        , ( 0x68, ld_l_b )
        , ( 0x69, ld_l_c )
        , ( 0x6A, ld_l_d )
        , ( 0x6B, ld_l_e )
        , ( 0x6C, ld_l_h )
        , ( 0x6F, ld_l_a )
        ]


ld_h_b : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_b ixiyhl rom z80 =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    --z80 |> set_h_z80 z80.main.b ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.main.b ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_h_c : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_c ixiyhl rom z80 =
    -- case 0x61: HL=HL&0xFF|C<<8; break;
    -- case 0x61: xy=xy&0xFF|C<<8; break;
    --z80 |> set_h_z80 z80.main.c ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.main.c ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_h_d : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_d ixiyhl rom z80 =
    -- case 0x62: HL=HL&0xFF|D<<8; break;
    -- case 0x62: xy=xy&0xFF|D<<8; break;
    --z80 |> set_h_z80 z80.main.d ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.main.d ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_h_e : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_e ixiyhl rom z80 =
    -- case 0x63: HL=HL&0xFF|E<<8; break;
    -- case 0x63: xy=xy&0xFF|E<<8; break;
    --z80 |> set_h_z80 z80.main.e ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.main.e ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_h_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_l ixiyhl rom z80 =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
    let
        value =
            z80.main |> set_h_ixiy (get_l_ixiy ixiyhl z80.main) ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x66 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x66 ixiyhl rom48k z80 =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_h_z80 value.value HL |> add_cpu_time 3
    MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)


ld_h_a : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_a ixiyhl rom z80 =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    --z80 |> set_h_z80 z80.flags.a ixiyhl
    let
        value =
            z80.main |> set_h_ixiy z80.flags.a ixiyhl
    in
    MainRegsWithPc value z80.pc


ld_l_b : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_b ixiyhl rom z80 =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --z80 |> set_l_z80 z80.main.b ixiyhl
    MainRegsWithPc (z80.main |> set_l_ixiy z80.main.b ixiyhl) z80.pc


ld_l_c : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_c ixiyhl rom z80 =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.main.c ixiyhl) z80.pc


ld_l_d : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_d ixiyhl rom z80 =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.main.d ixiyhl) z80.pc


ld_l_e : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_e ixiyhl rom z80 =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.main.e ixiyhl) z80.pc


ld_l_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_h ixiyhl rom z80 =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    MainRegsWithPc (z80.main |> set_l_ixiy (get_h_ixiy ixiyhl z80.main) ixiyhl) z80.pc


execute_0x6E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x6E ixiyhl rom48k z80 =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        main =
            z80.main
    in
    MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)


ld_l_a : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_a ixiyhl rom z80 =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    MainRegsWithPc (z80.main |> set_l_ixiy z80.flags.a ixiyhl) z80.pc
