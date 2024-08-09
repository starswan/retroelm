module Group00 exposing (..)

import Bitwise
import CpuTimeCTime exposing (add_cpu_time_time)
import Dict exposing (Dict)
import Utils exposing (shiftLeftBy8)
import Z80Delta exposing (Z80Delta(..), delta_noop)
import Z80Env exposing (mem)
import Z80Flags exposing (add16, dec, inc, rot)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_bc, get_xy, imm16, imm8, set_bc_main, set_xy)


delta_dict_00 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_00 =
    Dict.fromList
        [ ( 0x09, execute_0x09 )
        ]


delta_dict_lite_00 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_00 =
    Dict.fromList
        [ ( 0x00, delta_noop )
        , ( 0x01, execute_0x01 )
        , ( 0x02, execute_0x02 )
        , ( 0x03, execute_0x03 )
        , ( 0x04, execute_0x04 )
        , ( 0x05, execute_0x05 )
        , ( 0x06, execute_0x06 )
        , ( 0x07, execute_0x07 )
        , ( 0x08, ex_af )
        , ( 0x0A, execute_0x0A )
        , ( 0x0B, execute_0x0B )
        , ( 0x0C, execute_0x0C )
        , ( 0x0D, execute_0x0D )
        , ( 0x0E, execute_0x0E )
        , ( 0x0F, execute_0x0F )
        ]


execute_0x01 : Z80ROM -> Z80 -> Z80Delta
execute_0x01 rom48k z80 =
    -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
    let
        v =
            z80 |> imm16 rom48k

        z80main =
            z80.main |> set_bc_main v.value
    in
    MainRegsWithPcAndCpuTime z80main v.pc v.time


execute_0x02 : Z80ROM -> Z80 -> Z80Delta
execute_0x02 rom48k z80 =
    -- case 0x02: MP=(v=B<<8|C)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        addr =
            shiftLeftBy8 z80.main.b + z80.main.c
    in
    --{ z80 | env = z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 }
    --OnlyEnv (z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3)
    SetMem8WithTime addr z80.flags.a 3


execute_0x03 : Z80ROM -> Z80 -> Z80Delta
execute_0x03 rom48k z80 =
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


execute_0x04 : Z80ROM -> Z80 -> Z80Delta
execute_0x04 rom48k z80 =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80.main.b z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithMain new_b.flags { z80main | b = new_b.value }


execute_0x05 : Z80ROM -> Z80 -> Z80Delta
execute_0x05 rom48k z80 =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80.main.b z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithMain new_b.flags { z80main | b = new_b.value }


execute_0x06 : Z80ROM -> Z80 -> Z80Delta
execute_0x06 rom48k z80 =
    -- case 0x06: B=imm8(); break;
    let
        new_b =
            z80 |> imm8 rom48k

        z80main =
            z80.main
    in
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    MainRegsWithPcAndCpuTime { z80main | b = new_b.value } new_b.pc new_b.time


execute_0x07 : Z80ROM -> Z80 -> Z80Delta
execute_0x07 rom48k z80 =
    -- case 0x07: rot(A*0x101>>>7); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
    FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x0101)))


ex_af : Z80ROM -> Z80 -> Z80Delta
ex_af rom48k z80 =
    --{ z80 | flags = z80.alt_flags, alt_flags = z80.flags }
    FlagsAndAlt z80.alt_flags z80.flags


execute_0x09 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x09 ixiyhl rom48k z80 =
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


execute_0x0A : Z80ROM -> Z80 -> Z80Delta
execute_0x0A rom48k z80 =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        z80_main =
            z80.main

        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        new_a =
            z80.env |> mem v rom48k

        new_flags =
            { z80_flags | a = new_a.value }
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags (new_a.time |> add_cpu_time_time 3) new_flags


execute_0x0B : Z80ROM -> Z80 -> Z80Delta
execute_0x0B z80rom z80 =
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


execute_0x0C : Z80ROM -> Z80 -> Z80Delta
execute_0x0C z80rom z80 =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80.main.c z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithMain new_c.flags { z80main | c = new_c.value }


execute_0x0D : Z80ROM -> Z80 -> Z80Delta
execute_0x0D z80rom z80 =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80.main.c z80.flags

        z80main =
            z80.main
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithMain new_c.flags { z80main | c = new_c.value }


execute_0x0E : Z80ROM -> Z80 -> Z80Delta
execute_0x0E rom48k z80 =
    -- case 0x0E: C=imm8(); break;
    let
        z80main =
            z80.main

        new_c =
            z80 |> imm8 rom48k
    in
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    MainRegsWithPcAndCpuTime { z80main | c = new_c.value } new_c.pc new_c.time


execute_0x0F : Z80ROM -> Z80 -> Z80Delta
execute_0x0F rom48k z80 =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
    FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)))
