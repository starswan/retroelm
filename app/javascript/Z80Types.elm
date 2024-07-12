module Z80Types exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimePcAndValue, add_cpu_time_time)
import Z80Env exposing (Z80Env, Z80EnvWithPC, mem, mem16, z80_push)
import Z80Flags exposing (FlagRegisters)
type alias MainRegisters =
   {
      b:   Int,
      c:   Int,
      d:   Int,
      e:   Int,
      hl:  Int
   }
type alias MainWithIndexRegisters =
   {
      b:   Int,
      c:   Int,
      d:   Int,
      e:   Int,
      hl:  Int,
      ix:  Int,
      iy:  Int
   }

type alias ProgramCounter =
    {
        pc: Int
    }

type alias InterruptRegisters =
   {
      ir:  Int,
      r:   Int,
      --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iff: Int,
      iM: Int,
      halted: Bool
   }

type alias Z80 =
   {
      env: Z80Env,
      pc:  Int,
      main: MainWithIndexRegisters,
      flags: FlagRegisters,
      alt_main: MainRegisters,
      alt_flags: FlagRegisters,
      interrupts: InterruptRegisters,
      time_limit: Int
   }

type alias EnvWithPCAndValue =
   {
        env: Z80Env,
        pc: Int,
        value: Int
   }

--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}
imm8: Z80 -> CpuTimePcAndValue
imm8 z80 =
    let
        v = mem z80.pc z80.env
        new_pc = Bitwise.and (z80.pc + 1) 0xFFFF
        env_1 = v.time |> add_cpu_time_time 3
    in
        CpuTimePcAndValue env_1 new_pc v.value

-- would need the side-effect of mem call as well
--imm8_discard: Z80 -> Z80
--imm8_discard z80 =
--    z80 |> inc_pc |> add_cpu_time 3
--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}
imm16: Z80 -> CpuTimePcAndValue
imm16 z80 =
    let
        v = mem16 z80.pc z80.env
        pc = Bitwise.and (z80.pc + 2) 0xFFFF
        env = v.time |> add_cpu_time_time 6
    in
        CpuTimePcAndValue env pc v.value

--	private void jp(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) PC = a;
--	}
jp_z80: Bool -> Z80 -> Z80
jp_z80 y z80 =
   let
      a = imm16 z80
      env = z80.env
      z80_1 = { z80 | pc = a.pc, env = { env | time = a.time } }
   in
      if y then
         { z80_1 | pc = a.value }
      else
         z80_1

jp: Bool -> Z80 -> CpuTimeAndPc
jp y z80 =
  let
     a = imm16 z80
  in
    if y then
      CpuTimeAndPc a.time a.value
    else
      CpuTimeAndPc a.time a.pc

--	private void call(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) {push(PC); PC = a;}
--	}
call_z80: Bool -> Z80 -> Z80
call_z80 y z80 =
   let
      a = imm16 z80
      env = z80.env
      z80_2 = { z80 | pc = a.pc, env = { env | time = a.time } }
   in
     if y then
      let
         --b = debug_log "call" (a.value |> subName) Nothing
         --z80_1 = z80_2 |> push z80_2.pc |> set_pc a.value
         pushed = z80_2.env |> z80_push z80_2.pc
         z80_1 = { z80_2 | env = pushed, pc = a.value }
      in
         z80_1
     else
       z80_2

call: Bool -> Z80 -> Z80EnvWithPC
call y z80 =
   let
      a = imm16 z80
      env = z80.env
      z80_2 = { z80 | pc = a.pc, env = { env | time = a.time } }
   in
     if y then
      let
         --b = debug_log "call" (a.value |> subName) Nothing
         --z80_1 = z80_2 |> push z80_2.pc |> set_pc a.value
         pushed = z80_2.env |> z80_push z80_2.pc
         --z80_1 = { z80_2 | env = pushed, pc = a.value }
      in
         Z80EnvWithPC pushed a.value
     else
       Z80EnvWithPC z80_2.env a.pc

rst_z80: Int -> Z80 -> Z80
rst_z80 c z80 =
    --z80 |> push z80.pc |> set_pc (c - 199)
    let
        pushed  = z80.env |> z80_push z80.pc
    in
        { z80 | env = pushed, pc = (c - 199) }
