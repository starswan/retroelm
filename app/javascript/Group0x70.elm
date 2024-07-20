module Group0x70 exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (add_cpu_time_time)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (add_cpu_time_env, set_mem)
import Z80Types exposing (IXIYHL(..), Z80, env_mem_hl, get_h, get_l, hl_deref_with_z80)
execute_0x7077: IXIYHL -> Z80 -> Int -> Z80Delta
execute_0x7077 ixiyhl z80 value =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    let
       mem_target = z80 |> env_mem_hl ixiyhl
       env_1 = z80.env
       new_env = { env_1 | time = mem_target.time }
                 |> set_mem mem_target.value value
                 |> add_cpu_time_env 3
    in
       --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
       EnvWithPc new_env mem_target.pc

execute_0x70: IXIYHL -> Z80 -> Z80Delta
execute_0x70 ixiyhl z80 =
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
    execute_0x7077 ixiyhl z80 z80.main.b

execute_0x71: IXIYHL -> Z80 -> Z80Delta
execute_0x71 ixiyhl z80 =
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
    execute_0x7077 ixiyhl z80 z80.main.c

execute_0x72: IXIYHL -> Z80 -> Z80Delta
execute_0x72 ixiyhl z80 =
    -- case 0x72: env.mem(HL,D); time+=3; break;
    -- case 0x72: env.mem(getd(xy),D); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.d |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.d

execute_0x73: IXIYHL -> Z80 -> Z80Delta
execute_0x73 ixiyhl z80 =
    -- case 0x73: env.mem(HL,E); time+=3; break;
    -- case 0x73: env.mem(getd(xy),E); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.e |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.e

execute_0x74: IXIYHL -> Z80 -> Z80Delta
execute_0x74 ixiyhl z80 =
   -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
   -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_h HL z80.main) |> add_cpu_time_env 3
   --in
   --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
   --    EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 (get_h HL z80.main)

execute_0x75: IXIYHL -> Z80 -> Z80Delta
execute_0x75 ixiyhl z80 =
   -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
   -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_l HL z80.main) |> add_cpu_time_env 3
   --in
   --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
   --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 (get_l HL z80.main)
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

halt: Z80 -> Z80Delta
halt z80 =
   let
      interrupts = z80.interrupts
      n = shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
      (new_interrupts, time) = if n > 0 then
                                  -- turns out env.halt(n, r) just returns n...?
                                  --{ z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
                                 ({ interrupts | r = interrupts.r + n }, z80.env.time |> add_cpu_time_time (4 * n))
                               else
                                 (interrupts, z80.env.time)
                 --z80
    in
      --{ z80_1 | interrupts = { interrupts | halted = True } }
      InterruptsWithCpuTime { new_interrupts | halted = True } time

execute_0x77: IXIYHL -> Z80 -> Z80Delta
execute_0x77 ixiyhl z80 =
   -- case 0x77: env.mem(HL,A); time+=3; break;
   -- case 0x77: env.mem(getd(xy),A); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.flags.a
   --in
   --   { z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077 ixiyhl z80 z80.flags.a

execute_0x78: Z80 -> Z80Delta
execute_0x78 z80 =
   -- case 0x78: A=B; break;
   --z80 |> set_a z80.main.b
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.b }

execute_0x79: Z80 -> Z80Delta
execute_0x79 z80 =
   -- case 0x79: A=C; break;
   --z80 |> set_a z80.main.c
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.c }

execute_0x7A: Z80 -> Z80Delta
execute_0x7A z80 =
   -- case 0x7A: A=D; break;
   --z80 |> set_a z80.main.d
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.d }

execute_0x7B: Z80 -> Z80Delta
execute_0x7B z80 =
   -- case 0x7B: A=E; break;
   --z80 |> set_a z80.main.e
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.e }

execute_0x7C: IXIYHL -> Z80 -> Z80Delta
execute_0x7C ixiyhl z80 =
   -- case 0x7C: A=HL>>>8; break;
   -- case 0x7C: A=xy>>>8; break;
   --z80 |> set_a (get_h ixiyhl z80.main)
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = (get_h ixiyhl z80.main) }

execute_0x7D: IXIYHL -> Z80 -> Z80Delta
execute_0x7D ixiyhl z80 =
   -- case 0x7D: A=HL&0xFF; break;
   -- case 0x7D: A=xy&0xFF; break;
   --z80 |> set_a (get_l ixiyhl z80.main)
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = (get_l ixiyhl z80.main) }

execute_0x7E: IXIYHL -> Z80 -> Z80Delta
execute_0x7E ixiyhl z80 =
   -- case 0x7E: A=env.mem(HL); time+=3; break;
   -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      --env_1 = z80.env
      flags = z80.flags
   in
      --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
      --EnvWithFlagsAndPc { env_1 | time = value.time } { flags | a = value.value } value.pc
      FlagsWithPcAndTime { flags | a = value.value } value.pc value.time

