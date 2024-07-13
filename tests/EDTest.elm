module EDTest exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, set_mem, set_mem16)

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
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "E instructions"
         [
            test "0xED 0x6F RLD" <|
            \_ ->
               let
                  new_env = z80env |> set_mem addr 0xED
                                   |> set_mem (addr + 1) 0x6F
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x47 } }
               in
                  Expect.equal ((addr + 2), 0x40) (new_z80.pc, new_z80.flags.a)
            ,test "LDIR ED B0" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xED
                               |> set_mem (addr + 1) 0xB0
                               |> set_mem (0x5050) 0xA0
                               |> set_mem (0x5051) 0xA5
                               |> set_mem (0x5052) 0xAA
                               |> set_mem (0x5053) 0xBA
                               |> set_mem (0x5054) 0xB5
                  z80_1 = execute_instruction { z80 | env = { new_env | sp = 0xFF77 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
                  new_z80 = z80_1 |> execute_instruction |> execute_instruction |> execute_instruction |> execute_instruction
                  mem_vals = [(new_z80.env |> mem 0x6000).value,
                              (new_z80.env |> mem 0x6001).value,
                              (new_z80.env |> mem 0x6002).value,
                              (new_z80.env |> mem 0x6003).value,
                              (new_z80.env |> mem 0x6004).value]
               in
                  Expect.equal {pc=(addr + 2), b=0x00, c=0x00,d=0x60,e=0x05,hl=0x5055,mem=[0xA0, 0xA5, 0xAA, 0xBA, 0xB5]}
                  {pc=new_z80.pc, b=new_z80.main.b, c=new_z80.main.c,e=new_z80.main.e,d=new_z80.main.d,hl=new_z80.main.hl, mem=mem_vals}
            ,test "0xED 78 IN A, (C)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xED
                               |> set_mem (addr + 1) 0x78
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xFF, 0xFF) (new_z80.pc, new_z80.flags.fr, new_z80.flags.a)
            ,test "0xE1 POP HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xE1
                               |> set_mem sp 0x45
                               |> set_mem (sp + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x3445, sp + 2) (new_z80.pc, new_z80.main.hl, new_z80.env.sp)
            ,test "0xFD 0xE1 POP IY" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0xE1
                               |> set_mem sp 0x45
                               |> set_mem (sp + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F } }
               in
                  Expect.equal ((addr + 2), 0x3445, sp + 2) (new_z80.pc, new_z80.main.iy, new_z80.env.sp)
            ,test "0xE3 EX (SP),HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xE3
                               |> set_mem sp 0x45
                               |> set_mem (sp + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0xA000 } }
               in
                  Expect.equal
                  {pc=new_z80.pc, sp=new_z80.env.sp, hl=new_z80.main.hl, top=(new_z80.env |> mem16 sp).value}
                  {pc = (addr + 1), sp = sp, hl = 0x3445, top=0xA000}
            ,test "0xDD 0xE3 EX (SP),IX" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0xE3
                               |> set_mem sp 0x45
                               |> set_mem (sp + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | ix = 0xA000 } }
               in
                  Expect.equal
                  {pc=new_z80.pc, sp=new_z80.env.sp, ix=new_z80.main.ix, top=(new_z80.env |> mem16 sp).value}
                  {pc = (addr + 2), sp = sp, ix = 0x3445, top=0xA000}
         ]
      ]
