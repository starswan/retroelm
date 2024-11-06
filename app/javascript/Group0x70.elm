module Group0x70 exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, setMem)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), Z80, env_mem_hl_ixiy, get_h, get_h_ixiy, get_l, get_l_ixiy, hl_deref_with_z80_ixiy)


miniDict70 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict70 =
    Dict.fromList
        [ ( 0x70, ld_indirect_hl_b )
        , ( 0x71, ld_indirect_hl_c )
        , ( 0x72, ld_indirect_hl_d )
        , ( 0x73, ld_indirect_hl_e )
        , ( 0x74, ld_indirect_hl_h )
        , ( 0x75, ld_indirect_hl_l )
        , ( 0x77, ld_indirect_hl_a )
        , ( 0x7C, ld_a_h )
        , ( 0x7D, ld_a_l )
        , ( 0x7E, ld_a_indirect_hl )
        ]


delta_dict_lite_70 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_70 =
    Dict.fromList
        [ -- case 0x76: halt(); break;
          ( 0x76, execute_0x76_halt ) -- needs time_limit moving (is it a constant?)
        ]


execute_0x7077_ixiy : IXIY -> Z80ROM -> Z80 -> Int -> Z80Delta
execute_0x7077_ixiy ixiyhl rom48k z80 value =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    let
        mem_target =
            z80 |> env_mem_hl_ixiy ixiyhl rom48k

        env_1 =
            z80.env

        new_env =
            { env_1 | time = mem_target.time }
                |> setMem mem_target.value value
                |> addCpuTimeEnv 3
    in
    --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    SetMem8WithCpuTimeIncrementAndPc mem_target.value value mem_target.time 3 mem_target.pc


ld_indirect_hl_b : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_b ixiyhl rom48k z80 =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time }
    --             |> set_mem mem_target.value z80.main.b
    --             |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 z80.main.b


ld_indirect_hl_c : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_c ixiyhl rom48k z80 =
    -- case 0x71: env.mem(HL,C); time+=3; break;
    -- case 0x71: env.mem(getd(xy),C); time+=3; break;
    --let
    --    mem_target = z80 |> env_mem_hl ixiyhl
    --    env_1 = z80.env
    --    new_env = { env_1 | time = mem_target.time }
    --              |> set_mem mem_target.value z80.main.c
    --              |> add_cpu_time_env 3
    --in
    --    --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 z80.main.c


ld_indirect_hl_d : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_d ixiyhl rom48k z80 =
    -- case 0x72: env.mem(HL,D); time+=3; break;
    -- case 0x72: env.mem(getd(xy),D); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.d |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 z80.main.d


ld_indirect_hl_e : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_e ixiyhl rom48k z80 =
    -- case 0x73: env.mem(HL,E); time+=3; break;
    -- case 0x73: env.mem(getd(xy),E); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.e |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 z80.main.e


ld_indirect_hl_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_h ixiyhl rom48k z80 =
    -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
    -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_h HL z80.main) |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 (get_h HL z80.main)


ld_indirect_hl_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_l ixiyhl rom48k z80 =
    -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
    -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_l HL z80.main) |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 (get_l HL z80.main)



--
--	private void halt()
--	{
--		halted = true;
--		int n = time_limit-time+3 >> 2;
--		if(n>0) {
--			n = env.halt(n, IR|R&0x7F);
--			R+=n; time+=4*n;
--		}
--	}


execute_0x76_halt : Z80ROM -> Z80 -> Z80Delta
execute_0x76_halt rom z80 =
    let
        interrupts =
            z80.interrupts

        n =
            shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)

        ( new_interrupts, time ) =
            if n > 0 then
                -- turns out env.halt(n, r) just returns n...?
                --{ z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
                ( { interrupts | r = interrupts.r + n }, z80.env.time |> addCpuTimeTime (4 * n) )

            else
                ( interrupts, z80.env.time )

        --z80
    in
    --{ z80_1 | interrupts = { interrupts | halted = True } }
    InterruptsWithCpuTime { new_interrupts | halted = True } time


ld_indirect_hl_a : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_a ixiyhl rom48k z80 =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.flags.a
    --in
    --   { z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077_ixiy ixiyhl rom48k z80 z80.flags.a


ld_a_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_a_h ixiyhl rom z80 =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    --z80 |> set_a (get_h ixiyhl z80.main)
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = get_h_ixiy ixiyhl z80.main }


ld_a_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_a_l ixiyhl rom z80 =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    --z80 |> set_a (get_l ixiyhl z80.main)
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = get_l_ixiy ixiyhl z80.main }


ld_a_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_a_indirect_hl ixiyhl rom48k z80 =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80_ixiy ixiyhl rom48k

        --env_1 = z80.env
        flags =
            z80.flags
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    FlagsWithPcAndTime { flags | a = value.value } value.pc value.time
