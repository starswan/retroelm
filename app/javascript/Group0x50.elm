module Group0x50 exposing (..)

import Z80Delta exposing (Z80Delta(..))
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


execute_0x50 : Z80 -> Z80Delta
execute_0x50 z80 =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
    let
        main =
            z80.main
    in
    { main | d = z80.main.b } |> MainRegs


execute_0x51 : Z80 -> Z80Delta
execute_0x51 z80 =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
    let
        main =
            z80.main
    in
    { main | d = z80.main.c } |> MainRegs


execute_0x53 : Z80 -> Z80Delta
execute_0x53 z80 =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
    let
        main =
            z80.main
    in
    { main | d = z80.main.e } |> MainRegs


execute_0x54 : IXIYHL -> Z80 -> Z80Delta
execute_0x54 ixiyhl z80 =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_h ixiyhl z80.main } |> MainRegs


execute_0x55 : IXIYHL -> Z80 -> Z80Delta
execute_0x55 ixiyhl z80 =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | d = get_l ixiyhl z80.main } |> MainRegs


execute_0x56 : IXIYHL -> Z80 -> Z80Delta
execute_0x56 ixiyhl z80 =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        main =
            z80.main

        value =
            hl_deref_with_z80 ixiyhl z80
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time


execute_0x57 : Z80 -> Z80Delta
execute_0x57 z80 =
    -- case 0x57: D=A; break;
    --z80 |> set_d z80.flags.a
    let
        main =
            z80.main
    in
    { main | d = z80.flags.a } |> MainRegs


execute_0x58 : Z80 -> Z80Delta
execute_0x58 z80 =
    -- case 0x58: E=B; break;
    --z80 |> set_e z80.main.b
    let
        main =
            z80.main
    in
    { main | e = z80.main.b } |> MainRegs


execute_0x59 : Z80 -> Z80Delta
execute_0x59 z80 =
    -- case 0x59: E=C; break;
    --z80 |> set_e z80.main.c
    let
        main =
            z80.main
    in
    { main | e = z80.main.c } |> MainRegs


execute_0x5A : Z80 -> Z80Delta
execute_0x5A z80 =
    -- case 0x5A: E=D; break;
    --z80 |> set_e z80.main.d
    let
        main =
            z80.main
    in
    { main | e = z80.main.d } |> MainRegs


execute_0x5C : IXIYHL -> Z80 -> Z80Delta
execute_0x5C ixiyhl z80 =
    -- case 0x5C: E=HL>>>8; break;
    --z80 |> set_e (get_h ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_h ixiyhl z80.main } |> MainRegs


execute_0x5D : IXIYHL -> Z80 -> Z80Delta
execute_0x5D ixiyhl z80 =
    -- case 0x5D: E=HL&0xFF; break;
    --z80 |> set_e (get_l ixiyhl z80.main)
    let
        main =
            z80.main
    in
    { main | e = get_l ixiyhl z80.main } |> MainRegs


execute_0x5E : IXIYHL -> Z80 -> Z80Delta
execute_0x5E ixiyhl z80 =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            hl_deref_with_z80 ixiyhl z80

        main =
            z80.main
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time


execute_0x5F : Z80 -> Z80Delta
execute_0x5F z80 =
    -- case 0x5F: E=A; break;
    --z80 |> set_e z80.flags.a
    let
        main =
            z80.main
    in
    { main | e = z80.flags.a } |> MainRegs
