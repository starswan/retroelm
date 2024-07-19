module Group0x00 exposing (..)

import Bitwise
import CpuTimeCTime exposing (add_cpu_time_time)
import Utils exposing (shiftLeftBy8)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (add_cpu_time_env, mem, set_mem)
import Z80Flags exposing (add16, dec, inc, rot)
import Z80Types exposing (IXIYHL, Z80, add_cpu_time, get_bc, get_xy, imm16, imm8, set_bc_main, set_xy)


execute_0x01 : Z80 -> Z80Delta
execute_0x01 z80 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    let
        v =
            imm16 z80

        z80main =
            z80.main |> set_bc_main v.value
    in
    MainRegsWithPcAndCpuTime z80main v.pc v.time


execute_0x02 : Z80 -> Z80Delta
execute_0x02 z80 =
    -- case 0x02: MP=(v=B<<8|C)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        addr =
            shiftLeftBy8 z80.main.b + z80.main.c
    in
    --{ z80 | env = z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 }
    OnlyEnv (z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3)


execute_0x03 : Z80 -> Z80Delta
execute_0x03 z80 =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    let
        z80_main =
            z80.main

        tmp_c =
            z80_main.c + 1

        ( reg_b, reg_c ) =
            if tmp_c == 256 then
                ( Bitwise.and (z80_main.b + 1) 0xFF, 0 )

            else
                ( z80_main.b, tmp_c )
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    MainRegsAndCpuTime { z80_main | b = reg_b, c = reg_c } 2


execute_0x04 : Z80 -> Z80Delta
execute_0x04 z80 =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80.main.b z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithMain new_b.flags { z80main | b = new_b.value }


execute_0x05 : Z80 -> Z80Delta
execute_0x05 z80 =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80.main.b z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithMain new_b.flags { z80main | b = new_b.value }


execute_0x06 : Z80 -> Z80Delta
execute_0x06 z80 =
    -- case 0x06: B=imm8(); break;
    let
        new_b =
            imm8 z80

        z80main =
            z80.main
    in
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    MainRegsWithPcAndCpuTime { z80main | b = new_b.value } new_b.pc new_b.time


execute_0x07 : Z80 -> Z80Delta
execute_0x07 z80 =
    -- case 0x07: rot(A*0x101>>>7); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
    FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x0101)))


ex_af : Z80 -> Z80Delta
ex_af z80 =
    --{ z80 | flags = z80.alt_flags, alt_flags = z80.flags }
    FlagsAndAlt z80.alt_flags z80.flags


execute_0x09 : IXIYHL -> Z80 -> Z80Delta
execute_0x09 ixiyhl z80 =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            get_xy ixiyhl z80.main

        new_xy =
            add16 xy (get_bc z80) z80.flags

        new_z80 =
            set_xy new_xy.value ixiyhl z80.main
    in
    --Whole ({ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time)
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time


execute_0x0A : Z80 -> Z80Delta
execute_0x0A z80 =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        z80_main =
            z80.main

        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        new_a =
            mem v z80.env

        new_flags =
            { z80_flags | a = new_a.value }
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags (new_a.time |> add_cpu_time_time 3) new_flags


execute_0x0B : Z80 -> Z80Delta
execute_0x0B z80 =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    let
        z80_main =
            z80.main

        tmp_c =
            z80_main.c - 1

        ( reg_b, reg_c ) =
            if tmp_c < 0 then
                ( Bitwise.and (z80_main.b - 1) 0xFF, 0xFF )

            else
                ( z80_main.b, tmp_c )
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    MainRegsAndCpuTime { z80_main | b = reg_b, c = reg_c } 2


execute_0x0C : Z80 -> Z80Delta
execute_0x0C z80 =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80.main.c z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithMain new_c.flags { z80main | c = new_c.value }


execute_0x0D : Z80 -> Z80Delta
execute_0x0D z80 =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80.main.c z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithMain new_c.flags { z80main | c = new_c.value }


execute_0x0E : Z80 -> Z80Delta
execute_0x0E z80 =
    -- case 0x0E: C=imm8(); break;
    let
        z80main =
            z80.main

        new_c =
            imm8 z80
    in
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    MainRegsWithPcAndCpuTime { z80main | c = new_c.value } new_c.pc new_c.time


execute_0x0F : Z80 -> Z80Delta
execute_0x0F z80 =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
    FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)))
