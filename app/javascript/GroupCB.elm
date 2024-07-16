module GroupCB exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimePcAndValue, add_cpu_time_time)
import Dict exposing (Dict)
import Utils exposing (byte, char, toHexString)
import Z80Debug exposing (debug_todo)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (add_cpu_time_env, m1, mem, set_mem)
import Z80Flags exposing (IntWithFlags, bit, c_F53, shifter, shifter0)
import Z80Types exposing (IXIY, IXIYHL(..), IntWithFlagsTimeAndPC, Z80, a_with_z80, add_cpu_time, b_with_z80, c_with_z80, d_with_z80, e_with_z80, get_ixiy_xy, h_with_z80, hl_deref_with_z80, inc_pc, inc_pc2, l_with_z80, set_flag_regs, set_h, set_l)


group_cb_dict : Dict Int (Z80 -> Z80Delta)
group_cb_dict =
    Dict.fromList
        [--( 0x00, execute_CB00 )
         --, ( 0x01, execute_CB01 )
         --, ( 0x02, execute_CB02 )
         --, ( 0x03, execute_CB03 )
         --, ( 0x04, execute_CB04 )
         --, ( 0x05, execute_CB05 )
         --, ( 0x06, execute_CB06 )
         --, ( 0x07, execute_CB07 )
        ]



-- case 0x00: B=shifter(o,B); break;
-- case 0x01: C=shifter(o,C); break;
-- case 0x02: D=shifter(o,D); break;
-- case 0x03: E=shifter(o,E); break;
-- case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break;
-- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
-- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
-- case 0x07: A=shifter(o,A); break;
--       let
--         raw = z80 |> load408bit caseval HL
--         --z = debug_log "group_cb raw" (raw.value |> toHexString2) Nothing
--         value = shifter o raw.value z80.flags
--         --w = debug_log "group_cb value" (value.value |> toHexString2) Nothing
--         env_1 = z80.env
--         x = { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs value.flags |> set408bit caseval value.value HL
--       in
--         Whole x


execute_CB0007 : CpuTimePcAndValue -> Z80 -> IntWithFlagsTimeAndPC
execute_CB0007 raw z80 =
    let
        --o = Bitwise.and (c_value |> shiftRightBy 3) 7
        --caseval = Bitwise.and c_value 0xC7
        --z = debug_log "group_cb raw" (raw.value |> toHexString2) Nothing
        value =
            shifter0 raw.value z80.flags

        --w = debug_log "group_cb value" (value.value |> toHexString2) Nothing
        --env_1 = z80.env
        --x = { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs value.flags |> set408bit caseval value.value HL
    in
    --Whole x
    IntWithFlagsTimeAndPC value.value value.flags raw.time raw.pc


execute_CB00 : Z80 -> Z80Delta
execute_CB00 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> b_with_z80)

        main =
            z80.main
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc { main | b = x.value } x.time


execute_CB01 : Z80 -> Z80Delta
execute_CB01 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> c_with_z80)

        main =
            z80.main
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc { main | c = x.value } x.time


execute_CB02 : Z80 -> Z80Delta
execute_CB02 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> d_with_z80)

        main =
            z80.main
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc { main | d = x.value } x.time


execute_CB03 : Z80 -> Z80Delta
execute_CB03 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> e_with_z80)

        main =
            z80.main
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc { main | e = x.value } x.time


execute_CB04 : Z80 -> Z80Delta
execute_CB04 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> h_with_z80 HL)
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc (z80.main |> set_h x.value HL) x.time


execute_CB05 : Z80 -> Z80Delta
execute_CB05 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> l_with_z80 HL)
    in
    FlagsWithPCMainAndCpuTime x.flags x.pc (z80.main |> set_l x.value HL) x.time


execute_CB06 : Z80 -> Z80Delta
execute_CB06 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> hl_deref_with_z80 HL)

        env =
            z80.env

        env_1 =
            { env | time = x.time } |> set_mem z80.main.hl x.value
    in
    EnvWithFlagsAndPc env_1 x.flags x.pc


execute_CB07 : Z80 -> Z80Delta
execute_CB07 z80 =
    let
        x =
            z80 |> execute_CB0007 (z80 |> a_with_z80)

        flags =
            x.flags
    in
    FlagsWithPcAndTime { flags | a = x.value } x.pc x.time


group_cb : Z80 -> Z80Delta
group_cb tmp_z80 =
    --	private void group_cb()
    --	{
    --		int v, c = env.m1(PC, IR|R++&0x7F);
    --		PC = (char)(PC+1); time += 4;
    --		int o = c>>>3 & 7;
    --		switch(c & 0xC7) {
    let
        new_r =
            Bitwise.and (tmp_z80.interrupts.r + 1) 0x7F

        ir_or_r =
            Bitwise.or tmp_z80.interrupts.ir new_r

        c =
            m1 tmp_z80.pc (Bitwise.or tmp_z80.interrupts.ir ir_or_r) tmp_z80.env

        env =
            tmp_z80.env

        old_z80 =
            { tmp_z80 | env = { env | time = c.time } }

        new_pc =
            old_z80 |> inc_pc

        z80 =
            { old_z80 | pc = new_pc } |> add_cpu_time 4

        o =
            Bitwise.and (c.value |> shiftRightBy 3) 7

        caseval =
            Bitwise.and c.value 0xC7

        --y = debug_log "group_cb caseval" (caseval |> toHexString2) Nothing
        -- The original plan with this is not clever enough - there are
        -- 3 middle bits (0-7) that are captured by the 'o' value.
        -- Tests required for CB xx for all values of xx
        cb_func =
            group_cb_dict |> Dict.get c.value
    in
    case cb_func of
        Just f ->
            z80 |> f

        Nothing ->
            -- case 0x00: B=shifter(o,B); break;
            -- case 0x01: C=shifter(o,C); break;
            -- case 0x02: D=shifter(o,D); break;
            -- case 0x03: E=shifter(o,E); break;
            -- case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break;
            -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
            -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
            -- case 0x07: A=shifter(o,A); break;
            if caseval < 0x08 then
                let
                    raw =
                        z80 |> load408bit caseval HL

                    --z = debug_log "group_cb raw" (raw.value |> toHexString2) Nothing
                    value =
                        shifter o raw.value z80.flags

                    --w = debug_log "group_cb value" (value.value |> toHexString2) Nothing
                    env_1 =
                        z80.env

                    x =
                        { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs value.flags |> set408bit caseval value.value HL
                in
                Whole x

            else if caseval >= 0x40 && caseval <= 0x47 then
                -- case 0x40: bit(o,B); break;
                -- case 0x41: bit(o,C); break;
                -- case 0x42: bit(o,D); break;
                -- case 0x43: bit(o,E); break;
                -- case 0x44: bit(o,HL>>>8); break;
                -- case 0x45: bit(o,HL&0xFF); break;
                -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
                -- case 0x47: bit(o,A); break;
                let
                    raw =
                        z80 |> load408bit caseval HL

                    flags =
                        bit o raw.value z80.flags

                    env_1 =
                        z80.env

                    x =
                        { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs flags
                in
                Whole x

            else if caseval >= 0x80 && caseval <= 0x87 then
                -- case 0x80: B=B&~(1<<o); break;
                -- case 0x81: C=C&~(1<<o); break;
                -- case 0x82: D=D&~(1<<o); break;
                -- case 0x83: E=E&~(1<<o); break;
                -- case 0x84: HL&=~(0x100<<o); break;
                -- case 0x85: HL&=~(1<<o); break;
                -- case 0x86: v=env.mem(HL)&~(1<<o); time+=4; env.mem(HL,v); time+=3; break;
                -- case 0x87: A=A&~(1<<o); break;
                let
                    raw =
                        z80 |> load408bit caseval HL

                    result =
                        Bitwise.and raw.value (1 |> shiftLeftBy o |> complement)

                    env_1 =
                        z80.env

                    x =
                        { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
                in
                Whole x

            else if caseval >= 0xC0 && caseval <= 0xC7 then
                -- case 0xC0: B=B|1<<o; break;
                -- case 0xC1: C=C|1<<o; break;
                -- case 0xC2: D=D|1<<o; break;
                -- case 0xC3: E=E|1<<o; break;
                -- case 0xC4: HL|=0x100<<o; break;
                -- case 0xC5: HL|=1<<o; break;
                -- case 0xC6: v=env.mem(HL)|1<<o; time+=4; env.mem(HL,v); time+=3; break;
                -- case 0xC7: A=A|1<<o; break;
                let
                    raw =
                        z80 |> load408bit caseval HL

                    result =
                        Bitwise.or raw.value (1 |> shiftLeftBy o)

                    env_1 =
                        z80.env

                    x =
                        { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
                in
                Whole x

            else
                debug_todo "group_cb" (caseval |> toHexString) z80 |> Whole



--		}
--	}
--
--	private void group_xy_cb(int xy)
--	{
--		int pc = PC;
--		int a = MP = (char)(xy + (byte)env.mem(pc));
--		time += 3;
--		int c = env.mem((char)(pc+1));
--		PC = (char)(pc+2);
--		time += 5;
--		int v = env.mem(a);
--		time += 4;
--		int o = c>>>3 & 7;


group_xy_cb : IXIY -> Z80 -> Z80Delta
group_xy_cb ixiyhl z80 =
    let
        xy =
            get_ixiy_xy ixiyhl z80.main

        offset =
            mem z80.pc z80.env

        a =
            char (xy + byte offset.value)

        env_1 =
            z80.env

        z80_1 =
            { z80 | env = { env_1 | time = offset.time |> add_cpu_time_time 3 } }

        c =
            z80_1.env |> mem (char (z80.pc + 1))

        new_pc =
            z80_1 |> inc_pc2

        env_2 =
            z80_1.env

        z80_2 =
            { z80_1 | env = { env_2 | time = c.time |> add_cpu_time_time 5 }, pc = new_pc }

        v1 =
            z80_2.env |> mem a

        env_3 =
            z80_2.env

        z80_3 =
            { z80_2 | env = { env_3 | time = v1.time |> add_cpu_time_time 4 } }

        o =
            Bitwise.and (shiftRightBy 3 c.value) 7

        --		switch(c&0xC0) {
        --			case 0x00: v = shifter(o, v); break;
        --			case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
        --			case 0x80: v &= ~(1<<o); break;
        --			case 0xC0: v |= 1<<o; break;
        --		}
        v2 =
            case Bitwise.and c.value 0xC0 of
                0x00 ->
                    shifter o v1.value z80_3.flags

                0x40 ->
                    let
                        flags =
                            bit o v1.value z80_3.flags
                    in
                    IntWithFlags v1.value { flags | ff = Bitwise.or (Bitwise.and flags.ff (complement c_F53)) (shiftRightBy (Bitwise.and 8 c_F53) a) }

                0x80 ->
                    IntWithFlags (Bitwise.and v1.value (complement (shiftLeftBy o 1))) z80_3.flags

                _ ->
                    IntWithFlags (Bitwise.or v1.value (shiftLeftBy o 1)) z80_3.flags

        new_env =
            set_mem a v2.value z80_3.env

        --y = debug_log "xy_cb2" ((z80.pc |> toHexString) ++ " c " ++ (c.value |> toHexString2) ++
        --                                                   " set " ++ (a |> toHexString) ++
        --                                                   " from " ++ (v1.value |> toHexString2) ++
        --                                                   " to " ++ (v2.value |> toHexString2)) new_env
        --		env.mem(a, v);
        --		time += 3;
        z80_4 =
            { z80_3 | flags = v2.flags, env = new_env |> add_cpu_time_env 3 }

        --		switch(c&0x07) {
        --			case 0: B = v; break;
        --			case 1: C = v; break;
        --			case 2: D = v; break;
        --			case 3: E = v; break;
        --			case 4: HL = HL&0x00FF | v<<8; break;
        --			case 5: HL = HL&0xFF00 | v; break;
        --			case 7: A = v; break;
        --		}
        caseval =
            Bitwise.and c.value 0x07
    in
    if caseval /= 6 then
        set408bit caseval v2.value HL z80_4 |> Whole

    else
        z80_4 |> Whole


-- There appear to be many situations where we already know that we don't need all
-- this complexity as we're just doing LD A,B or something similar - so stop using it in those cases


load408bit : Int -> IXIYHL -> Z80 -> CpuTimePcAndValue
load408bit c_value ixiyhl z80 =
    case Bitwise.and c_value 0x07 of
        0 ->
            b_with_z80 z80

        1 ->
            c_with_z80 z80

        2 ->
            d_with_z80 z80

        3 ->
            e_with_z80 z80

        4 ->
            h_with_z80 ixiyhl z80

        5 ->
            l_with_z80 ixiyhl z80

        6 ->
            hl_deref_with_z80 ixiyhl z80

        _ ->
            a_with_z80 z80


set408bit : Int -> Int -> IXIYHL -> Z80 -> Z80
set408bit c value ixiyhl z80 =
    case Bitwise.and c 0x07 of
        0 ->
            z80 |> set_b value

        1 ->
            z80 |> set_c value

        2 ->
            z80 |> set_d value

        3 ->
            z80 |> set_e value

        4 ->
            set_h_z80 value ixiyhl z80

        5 ->
            set_l_z80 value ixiyhl z80

        6 ->
            { z80 | env = set_mem z80.main.hl value z80.env }

        _ ->
            z80 |> set_a value


set_b : Int -> Z80 -> Z80
set_b value z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | b = value } }


set_c : Int -> Z80 -> Z80
set_c value z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | c = value } }


set_d : Int -> Z80 -> Z80
set_d value z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | d = value } }


set_e : Int -> Z80 -> Z80
set_e value z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | e = value } }

set_h_z80 : Int -> IXIYHL -> Z80 -> Z80
set_h_z80 value ixiyhl z80 =
    let
        main =
            z80.main |> set_h value ixiyhl
    in
    { z80 | main = main }


set_l_z80 : Int -> IXIYHL -> Z80 -> Z80
set_l_z80 value ixiyhl z80 =
    let
        main =
            z80.main |> set_l value ixiyhl
    in
    { z80 | main = main }


set_a : Int -> Z80 -> Z80
set_a value z80 =
    let
        z80_flags =
            z80.flags
    in
    { z80 | flags = { z80_flags | a = value } }

