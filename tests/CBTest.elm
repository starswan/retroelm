module CBTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, setMem)
import Z80Rom

suite : Test
suite =
   let
       addr = 0x5800
       sp = 0xF765
       hl = 0x1234
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80main = old_z80.main
       z80 = { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }
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
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x06
                               |> setMem (addr + 3) 0xC6
                               |> setMem 0xA086 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA086 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0x11) (new_z80.pc, mem_value.value)
            ,test "0xFD 0xCB nn 0x9E RES 3, (IY + n) -ve" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0xFE
                               |> setMem (addr + 3) 0x9E
                               |> setMem 0xA07E 0xFF
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA07E new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0xF7) (new_z80.pc, mem_value.value)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (SET)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x02
                               |> setMem (addr + 3) 0x66
                               |> setMem 0xA082 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x10) (new_z80.pc, new_z80.flags.fr)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (CLEAR)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x02
                               |> setMem (addr + 3) 0x66
                               |> setMem 0xA082 0xEF
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix=0xA080, hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x00) (new_z80.pc, new_z80.flags.fr)
         ]
