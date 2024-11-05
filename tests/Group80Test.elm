module Group80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (setMem)
import Z80Rom

suite : Test
suite =
   let
       addr = 30000
       old_z80 = Z80.constructor
       z80 = { old_z80 | pc = addr }
       flags = z80.flags
       z80env = z80.env
       z80main = z80.main
       z80rom = Z80Rom.constructor
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "0x86 ADD A, (HL)"
         [
            test "doit" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0x86
                               |> setMem 0x5050 0x11
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 }, flags = { flags | a = 0x76 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
               in
                  Expect.equal {pc=(addr + 1), a=0x87}  {pc=new_z80.pc, a=new_z80.flags.a}
         ]
         ,describe "0xFD 0x86 0x01 ADD A,(IY + n)"
         [
            test "doit" <|
            \_ ->
               let
                  alt = z80.alt_main
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0x86
                               |> setMem (addr + 2) 0x01
                               |> setMem 0x5051 0x11
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 }, flags = { flags | a = 0x76 },
                                                        main = { z80main | iy = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
               in
                  Expect.equal {pc=(addr + 3), a=0x87}  {pc=new_z80.pc, a=new_z80.flags.a}
         ]
      ]
