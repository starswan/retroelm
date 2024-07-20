module Group0x60 exposing (..)

import CpuTimeCTime exposing (add_cpu_time_time)
import Z80Delta exposing (Z80Delta(..))
import Z80Types exposing (IXIYHL(..), Z80, get_h, get_l, hl_deref_with_z80, set_h, set_l)


execute_0x60 : IXIYHL -> Z80 -> Z80Delta
execute_0x60 ixiyhl z80 =
    -- case 0x60: HL=HL&0xFF|B<<8; break;
    -- case 0x60: xy=xy&0xFF|B<<8; break;
    --z80 |> set_h_z80 z80.main.b ixiyhl
    let
        value =
            z80.main |> set_h z80.main.b ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x61 : IXIYHL -> Z80 -> Z80Delta
execute_0x61 ixiyhl z80 =
    -- case 0x61: HL=HL&0xFF|C<<8; break;
    -- case 0x61: xy=xy&0xFF|C<<8; break;
    --z80 |> set_h_z80 z80.main.c ixiyhl
    let
        value =
            z80.main |> set_h z80.main.c ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x62 : IXIYHL -> Z80 -> Z80Delta
execute_0x62 ixiyhl z80 =
    -- case 0x62: HL=HL&0xFF|D<<8; break;
    -- case 0x62: xy=xy&0xFF|D<<8; break;
    --z80 |> set_h_z80 z80.main.d ixiyhl
    let
        value =
            z80.main |> set_h z80.main.d ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x63 : IXIYHL -> Z80 -> Z80Delta
execute_0x63 ixiyhl z80 =
    -- case 0x63: HL=HL&0xFF|E<<8; break;
    -- case 0x63: xy=xy&0xFF|E<<8; break;
    --z80 |> set_h_z80 z80.main.e ixiyhl
    let
        value =
            z80.main |> set_h z80.main.e ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x65 : IXIYHL -> Z80 -> Z80Delta
execute_0x65 ixiyhl z80 =
    -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
    -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
    --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
    let
        value =
            z80.main |> set_h (get_l ixiyhl z80.main) ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x66 : IXIYHL -> Z80 -> Z80Delta
execute_0x66 ixiyhl z80 =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        value =
            hl_deref_with_z80 ixiyhl z80

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_h_z80 value.value HL |> add_cpu_time 3
    MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> add_cpu_time_time 3)


execute_0x67 : IXIYHL -> Z80 -> Z80Delta
execute_0x67 ixiyhl z80 =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    --z80 |> set_h_z80 z80.flags.a ixiyhl
    let
        value =
            z80.main |> set_h z80.flags.a ixiyhl
    in
    MainRegsWithPc value z80.pc


execute_0x68 : IXIYHL -> Z80 -> Z80Delta
execute_0x68 ixiyhl z80 =
    -- case 0x68: HL=HL&0xFF00|B; break;
    -- case 0x68: xy=xy&0xFF00|B; break;
    --z80 |> set_l_z80 z80.main.b ixiyhl
    MainRegsWithPc (z80.main |> set_l z80.main.b ixiyhl) z80.pc


execute_0x69 : IXIYHL -> Z80 -> Z80Delta
execute_0x69 ixiyhl z80 =
    -- case 0x69: HL=HL&0xFF00|C; break;
    -- case 0x69: xy=xy&0xFF00|C; break;
    --z80 |> set_l_z80 z80.main.c ixiyhl
    MainRegsWithPc (z80.main |> set_l z80.main.c ixiyhl) z80.pc


execute_0x6A : IXIYHL -> Z80 -> Z80Delta
execute_0x6A ixiyhl z80 =
    -- case 0x6A: HL=HL&0xFF00|D; break;
    -- case 0x6A: xy=xy&0xFF00|D; break;
    --z80 |> set_l_z80 z80.main.d ixiyhl
    MainRegsWithPc (z80.main |> set_l z80.main.d ixiyhl) z80.pc


execute_0x6B : IXIYHL -> Z80 -> Z80Delta
execute_0x6B ixiyhl z80 =
    -- case 0x6B: HL=HL&0xFF00|E; break;
    -- case 0x6B: xy=xy&0xFF00|E; break;
    --z80 |> set_l_z80 z80.main.e ixiyhl
    MainRegsWithPc (z80.main |> set_l z80.main.e ixiyhl) z80.pc


execute_0x6C : IXIYHL -> Z80 -> Z80Delta
execute_0x6C ixiyhl z80 =
    -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
    -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
    --z80 |> set_l_z80 (get_h ixiyhl z80.main) ixiyhl
    MainRegsWithPc (z80.main |> set_l (get_h ixiyhl z80.main) ixiyhl) z80.pc


execute_0x6E : IXIYHL -> Z80 -> Z80Delta
execute_0x6E ixiyhl z80 =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        value =
            hl_deref_with_z80 ixiyhl z80

        main =
            z80.main

        env_1 =
            z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_l_z80 value.value HL |> add_cpu_time 3
    MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> add_cpu_time_time 3)


execute_0x6F : IXIYHL -> Z80 -> Z80Delta
execute_0x6F ixiyhl z80 =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    --z80 |> set_l_z80 z80.flags.a ixiyhl
    MainRegsWithPc (z80.main |> set_l z80.flags.a ixiyhl) z80.pc
