module Z80Types exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimePcAndValue, add_cpu_time_time)
import Z80Env exposing (Z80Env, mem, mem16)
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

type alias IntWithPcAndEnv =
    {
        value: Int,
        pc: Int,
        env: Z80Env
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

