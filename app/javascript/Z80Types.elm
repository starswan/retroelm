module Z80Types exposing (..)

import Z80Env exposing (Z80Env)
import Z80Flags exposing (FlagRegisters)
type alias MainRegisters =
   {
      b:   Int,
      c:   Int,
      d:   Int,
      e:   Int,
      hl:  Int
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
      main: MainRegisters,
      flags: FlagRegisters,
      alt_main: MainRegisters,
      alt_flags: FlagRegisters,
      ix: Int,
      iy: Int,
      interrupts: InterruptRegisters,
      time_limit: Int
   }

type alias IntWithPcAndEnv =
    {
        value: Int,
        pc: Int,
        env: Z80Env
    }