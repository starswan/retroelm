module Z80Types exposing (..)

import Bitwise
import Utils exposing (shiftRightBy8)
import Z80Env exposing (Z80Env, add_cpu_time_env, mem, mem16, set_mem)
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
      sp:  Int,
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
type alias EnvWithStackPointer =
   {
        env: Z80Env,
        sp: Int
   }

type alias EnvWithStackPointerAndValue =
   {
        env: Z80Env,
        sp: Int,
        value: Int
   }

type alias EnvWithPCAndValue =
   {
        env: Z80Env,
        pc: Int,
        value: Int
   }

--public void push(int v) {
--	int sp;
--	time++;
--	env.mem((char)((sp=SP)-1), v>>>8);
--	time += 3;
--	env.mem(SP = (char)(sp-2), v&0xFF);
--	time += 3;
--}
push: Int -> Z80 -> EnvWithStackPointer
push v z80 =
   let
      --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
      sp_minus_1 = Bitwise.and (z80.sp - 1) 0xFFFF
      new_sp = Bitwise.and (z80.sp - 2) 0xFFFF
      env_2 = z80.env
             |> add_cpu_time_env 1
             |> set_mem sp_minus_1 (shiftRightBy8 v)
             |> add_cpu_time_env 3
             |> set_mem new_sp (Bitwise.and v 0xFF)
             |> add_cpu_time_env 3
   in
      EnvWithStackPointer env_2 new_sp

pop: Z80 -> EnvWithStackPointerAndValue
pop z80 =
   let
      v = z80.env |> mem16 z80.sp
      env = v.env |> add_cpu_time_env 6
   in
      EnvWithStackPointerAndValue env (z80.sp + 2) v.value

--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}
imm8: Z80 -> EnvWithPCAndValue
imm8 z80 =
    let
        v = mem z80.pc z80.env
        new_pc = Bitwise.and (z80.pc + 1) 0xFFFF
        env_1 = v.env |> add_cpu_time_env 3
    in
        EnvWithPCAndValue env_1 new_pc v.value

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
imm16: Z80 -> EnvWithPCAndValue
imm16 z80 =
    let
        v = mem16 z80.pc z80.env
        pc = Bitwise.and (z80.pc + 2) 0xFFFF
        env = v.env |> add_cpu_time_env 6
    in
        EnvWithPCAndValue env pc v.value

