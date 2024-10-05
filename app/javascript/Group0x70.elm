module Group0x70 exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..), delta_noop)
import Z80Env exposing (addCpuTimeEnv, setMem)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), Z80, env_mem_hl, get_h, get_l, hl_deref_with_z80)


delta_dict_70 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_70 =
    Dict.fromList
        [ ( 0x70, execute_0x70 )
        , ( 0x71, execute_0x71 )
        , ( 0x72, execute_0x72 )
        , ( 0x73, execute_0x73 )
        , ( 0x74, execute_0x74 )
        , ( 0x75, execute_0x75 )
        , ( 0x77, execute_0x77 )
        , ( 0x7C, execute_0x7C )
        , ( 0x7D, execute_0x7D )
        , ( 0x7E, execute_0x7E )
        ]


delta_dict_lite_70 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_70 =
    Dict.fromList
        [ -- case 0x76: halt(); break;
          ( 0x76, execute_0x76_halt )
        , ( 0x78, execute_0x78 )
        , ( 0x79, execute_0x79 )
        , ( 0x7A, execute_0x7A )
        , ( 0x7B, execute_0x7B )
        , -- case 0x7F: break;
          ( 0x7F, delta_noop )
        ]


execute_0x7077 : IXIYHL -> Z80ROM -> Z80 -> Int -> Z80Delta
execute_0x7077 ixiyhl rom48k z80 value =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    let
        mem_target =
            z80 |> env_mem_hl ixiyhl rom48k

        env_1 =
            z80.env

        new_env =
            { env_1 | time = mem_target.time }
                |> setMem mem_target.value value
                |> addCpuTimeEnv 3
    in
    --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --EnvWithPc new_env mem_target.pc
    SetMem8WithCpuTimeIncrementAndPc mem_target.value value mem_target.time 3 mem_target.pc


execute_0x70 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x70 ixiyhl rom48k z80 =
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
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 z80.main.b


execute_0x71 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x71 ixiyhl rom48k z80 =
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
    --    EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 z80.main.c


execute_0x72 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x72 ixiyhl rom48k z80 =
    -- case 0x72: env.mem(HL,D); time+=3; break;
    -- case 0x72: env.mem(getd(xy),D); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.d |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 z80.main.d


execute_0x73 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x73 ixiyhl rom48k z80 =
    -- case 0x73: env.mem(HL,E); time+=3; break;
    -- case 0x73: env.mem(getd(xy),E); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.e |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 z80.main.e


execute_0x74 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x74 ixiyhl rom48k z80 =
    -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
    -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_h HL z80.main) |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --    EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 (get_h HL z80.main)


execute_0x75 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x75 ixiyhl rom48k z80 =
    -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
    -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_l HL z80.main) |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl rom48k z80 (get_l HL z80.main)



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


execute_0x77 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x77 ixiyhl rom48k z80 =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.flags.a
    --in
    --   { z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077 ixiyhl rom48k z80 z80.flags.a


execute_0x78 : Z80ROM -> Z80 -> Z80Delta
execute_0x78 rom z80 =
    -- case 0x78: A=B; break;
    --z80 |> set_a z80.main.b
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = z80.main.b }


execute_0x79 : Z80ROM -> Z80 -> Z80Delta
execute_0x79 rom z80 =
    -- case 0x79: A=C; break;
    --z80 |> set_a z80.main.c
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = z80.main.c }


execute_0x7A : Z80ROM -> Z80 -> Z80Delta
execute_0x7A rom z80 =
    -- case 0x7A: A=D; break;
    --z80 |> set_a z80.main.d
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = z80.main.d }


execute_0x7B : Z80ROM -> Z80 -> Z80Delta
execute_0x7B rom z80 =
    -- case 0x7B: A=E; break;
    --z80 |> set_a z80.main.e
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = z80.main.e }


execute_0x7C : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x7C ixiyhl rom z80 =
    -- case 0x7C: A=HL>>>8; break;
    -- case 0x7C: A=xy>>>8; break;
    --z80 |> set_a (get_h ixiyhl z80.main)
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = get_h ixiyhl z80.main }


execute_0x7D : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x7D ixiyhl rom z80 =
    -- case 0x7D: A=HL&0xFF; break;
    -- case 0x7D: A=xy&0xFF; break;
    --z80 |> set_a (get_l ixiyhl z80.main)
    let
        flags =
            z80.flags
    in
    FlagRegs { flags | a = get_l ixiyhl z80.main }


execute_0x7E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x7E ixiyhl rom48k z80 =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
        flags =
            z80.flags
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    FlagsWithPcAndTime { flags | a = value.value } value.pc value.time
