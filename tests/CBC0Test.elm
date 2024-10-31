module CBC0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, incrementBy1, incrementBy2, incrementBy3, toInt)
import Z80Env exposing (mem, setMem)
import Z80Rom

suite : Test
suite =
   let
       int_addr = 0x5800
       addr = int_addr |> fromInt
       sp = 0xF765
       hl = 0x1234
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80main = old_z80.main
       z80 = { old_z80 | pc = addr, env = { old_z80env | sp = sp |> fromInt }, main = { z80main | hl = hl |> fromInt } }
       flags = z80.flags
       z80env = z80.env
       z80rom = Z80Rom.constructor
   in
       describe "Bit instructions (CB)"
         [
            test "0xDD 0xCB nn 0xC6 SET 0, (IX + n)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr |> incrementBy1) 0xCB
                               |> setMem (addr |> incrementBy2) 0x06
                               |> setMem (addr |> incrementBy3) 0xC6
                               |> setMem (0xA086 |> fromInt) 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 |> fromInt },
                                                        main = { z80main | ix=0xA080 |> fromInt, hl = 0x6545 |> fromInt, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem (0xA086|>fromInt) new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((int_addr + 4), 0x11) (new_z80.pc |> toInt, mem_value.value)
         ]
