module GroupCB exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimePcAndValue, addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Address exposing (addIndexOffset, incrementBy1, incrementBy2, toInt)
import Z80Debug exposing (debugTodo)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, m1, mem, setMem)
import Z80Flags exposing (IntWithFlags, bit, c_F53, shifter)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), IntWithFlagsTimeAndPC, Z80, a_with_z80, add_cpu_time, b_with_z80, c_with_z80, d_with_z80, e_with_z80, get_ixiy_xy, h_with_z80, hl_deref_with_z80, l_with_z80, set408bit)


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
--execute_CB0007 : CpuTimePcAndValue -> Z80 -> IntWithFlagsTimeAndPC
--execute_CB0007 raw z80 =
--    let
--        --o = Bitwise.and (c_value |> shiftRightBy 3) 7
--        --caseval = Bitwise.and c_value 0xC7
--        --z = debug_log "group_cb raw" (raw.value |> toHexString2) Nothing
--        value =
--            shifter0 raw.value z80.flags
--
--        --w = debug_log "group_cb value" (value.value |> toHexString2) Nothing
--        --env_1 = z80.env
--        --x = { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs value.flags |> set408bit caseval value.value HL
--    in
--    --Whole x
--    IntWithFlagsTimeAndPC value.value value.flags raw.time raw.pc
--execute_CB06 : Z80ROM -> Z80 -> Z80Delta
--execute_CB06 rom48k z80 =
--    let
--        x =
--            z80 |> execute_CB0007 (z80 |> hl_deref_with_z80 HL rom48k)
--
--        env =
--            z80.env
--
--        env_1 =
--            { env | time = x.time } |> setMem (z80.main.hl |> toInt) x.value
--env_1 = case z80.main.hl of
--  ROMAddress int -> { env | time = x.time }
--  RAMAddress ramAddress ->
--    { env | time = x.time } |> setMem ramAddress x.value
--    in
--    EnvWithFlagsAndPc env_1 x.flags x.pc


group_cb : Z80ROM -> Z80 -> Z80Delta
group_cb rom48k tmp_z80 =
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
            tmp_z80.env |> m1 (tmp_z80.pc |> toInt) (Bitwise.or tmp_z80.interrupts.ir ir_or_r) rom48k

        env =
            tmp_z80.env

        old_z80 =
            { tmp_z80 | env = { env | time = c.time } }

        new_pc =
            old_z80.pc |> incrementBy1

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
            -- we can't actually remove this until we have implemented (and tested) all of CB40 - CB7F
            --            if caseval >= 0x40 && caseval <= 0x47 then
            --                -- case 0x40: bit(o,B); break;
            --                -- case 0x41: bit(o,C); break;
            --                -- case 0x42: bit(o,D); break;
            --                -- case 0x43: bit(o,E); break;
            --                -- case 0x44: bit(o,HL>>>8); break;
            --                -- case 0x45: bit(o,HL&0xFF); break;
            --                -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
            --                -- case 0x47: bit(o,A); break;
            --                let
            --                    raw =
            --                        z80 |> load408bit caseval HL rom48k
            --
            --                    flags =
            --                        bit o raw.value z80.flags
            --
            --                    --env_1 =
            --                    --    z80.env
            --                    --x =
            --                    --    { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs flags
            --                in
            --                --Whole x
            --                CpuTimeWithFlagsAndPc raw.time flags raw.pc
            --
            --            else if caseval >= 0x80 && caseval <= 0x87 then
            if caseval >= 0x80 && caseval <= 0x87 then
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
                        z80 |> load408bit caseval HL rom48k

                    result =
                        Bitwise.and raw.value (1 |> shiftLeftBy o |> complement)

                    --env_1 =
                    --    z80.env
                    --x =
                    --    { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
                in
                --Whole x
                PcTimeSet408Bit raw.pc raw.time caseval result

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
                        z80 |> load408bit caseval HL rom48k

                    result =
                        Bitwise.or raw.value (1 |> shiftLeftBy o)

                    --env_1 =
                    --    z80.env
                    --x =
                    --    { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
                in
                --Whole x
                PcTimeSet408Bit raw.pc raw.time caseval result

            else
                debugTodo "group_cb" (caseval |> toHexString) z80 |> Whole



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


group_xy_cb : IXIY -> Z80ROM -> Z80 -> Z80Delta
group_xy_cb ixiyhl rom48k z80 =
    let
        xy =
            get_ixiy_xy ixiyhl z80.main

        offset =
            mem (z80.pc |> toInt) z80.env.time rom48k z80.env.ram

        addr =
            --char (xy + byte offset.value)
            xy |> addIndexOffset offset.value |> toInt

        env_1 =
            z80.env

        c =
            --mem (char (z80.pc + 1)) (offset.time |> addCpuTimeTime 3) rom48k z80.env.ram
            mem ((z80.pc |> incrementBy1) |> toInt) (offset.time |> addCpuTimeTime 3) rom48k z80.env.ram

        new_pc =
            z80.pc |> incrementBy2

        v1 =
            mem addr (c.time |> addCpuTimeTime 5) rom48k z80.env.ram

        z80_3 =
            { z80 | pc = new_pc, env = { env_1 | time = v1.time |> addCpuTimeTime 4 } }

        o =
            Bitwise.and (shiftRightBy 3 c.value) 7

        cAndC0 =
            Bitwise.and c.value 0xC0

        --		switch(c&0xC0) {
        --			case 0x00: v = shifter(o, v); break;
        --			case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
        --			case 0x80: v &= ~(1<<o); break;
        --			case 0xC0: v |= 1<<o; break;
        --		}
        v2 =
            case cAndC0 of
                0x00 ->
                    shifter o v1.value z80_3.flags

                0x40 ->
                    let
                        flags =
                            bit o v1.value z80_3.flags
                    in
                    IntWithFlags v1.value { flags | ff = Bitwise.or (Bitwise.and flags.ff (complement c_F53)) (shiftRightBy (Bitwise.and 8 c_F53) addr) }

                0x80 ->
                    IntWithFlags (Bitwise.and v1.value (complement (shiftLeftBy o 1))) z80_3.flags

                _ ->
                    IntWithFlags (Bitwise.or v1.value (shiftLeftBy o 1)) z80_3.flags

        new_env =
            if cAndC0 == 0x40 then
                z80_3.env

            else
                z80_3.env |> setMem addr v2.value |> addCpuTimeEnv 3

        --case a of
        --  ROMAddress int -> z80_3.env
        --  RAMAddress ramAddress ->
        --    z80_3.env |> setMem ramAddress v2.value |> addCpuTimeEnv 3
        --y = debug_log "xy_cb2" ((z80.pc |> toHexString) ++ " c " ++ (c.value |> toHexString2) ++
        --                                                   " set " ++ (a |> toHexString) ++
        --                                                   " from " ++ (v1.value |> toHexString2) ++
        --                                                   " to " ++ (v2.value |> toHexString2)) new_env
        --		env.mem(a, v);
        --		time += 3;
        z80_4 =
            { z80_3 | flags = v2.flags, env = new_env }

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
    if (caseval /= 6) && (cAndC0 /= 0x40) then
        z80_4 |> set408bit caseval v2.value HL |> Whole

    else
        z80_4 |> Whole



-- There appear to be many situations where we already know that we don't need all
-- this complexity as we're just doing LD A,B or something similar - so stop using it in those cases


load408bit : Int -> IXIYHL -> Z80ROM -> Z80 -> CpuTimePcAndValue
load408bit c_value ixiyhl rom48k z80 =
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
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        _ ->
            a_with_z80 z80
