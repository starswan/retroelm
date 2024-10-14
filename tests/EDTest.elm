module EDTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (Z80Address(..), fromInt, incRamAddress, increment, toInt)
import Z80Env exposing (mem, mem16, setMem)
import Z80Rom
import Z80WriteableAddress exposing (Z80WriteableAddress(..))

suite : Test
suite =
   let
       addr = Z80MemoryAddress 0
       addr_plus_1 = Z80MemoryAddress 1
       ram_addr_plus_1 = RAMAddress addr_plus_1
       addr_plus_2 = Z80MemoryAddress 2
       addr_plus_3 = Z80MemoryAddress 3
       pc_addr = RAMAddress addr
       pc_addr_plus_2 = RAMAddress addr_plus_2
       sp = Z80MemoryAddress (0xF765 - 0x4000)
       sp_plus_1 = Z80MemoryAddress (0xF766 - 0x4000)
       sp_addr = RAMAddress sp
       hl = 0x1234 |> fromInt
       old_z80 = Z80.constructor
       old_z80env = old_z80.env
       z80main = old_z80.main
       z80 = { old_z80 | pc = pc_addr, env = { old_z80env | sp = sp_addr }, main = { z80main | hl = hl } }
       flags = z80.flags
       z80env = z80.env
       z80rom = Z80Rom.constructor
   in
   describe "0xEn instructions" -- Nest as many descriptions as you like.
      [
         describe "ED instructions"
         [
            test "0xED 0x6F RLD" <|
            \_ ->
               let
                  new_env = z80env |> setMem addr 0xED
                                   |> setMem (addr_plus_1) 0x6F
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 |> fromInt }, flags = { flags | a = 0x47 } }
               in
                  Expect.equal (2, 0x40) (new_z80.pc |> toInt, new_z80.flags.a)
            , test "0xED 0x7B LD SP,(nn)" <|
            \_ ->
               let
                  new_env = z80env |> setMem addr 0xED
                                   |> setMem (addr_plus_1) 0x7B
                                   |> setMem (addr_plus_2) 0x98
                                   |> setMem (addr_plus_3) 0xA4
                                   |> setMem (Z80MemoryAddress(0xA498 - 0x4000)) 0x01
                                   |> setMem (Z80MemoryAddress(0xA499 - 0x4000)) 0x02
                                   |> setMem (Z80MemoryAddress(0xA49A - 0x4000)) 0x03
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 |> fromInt }, flags = { flags | a = 0x47 } }
               in
                  Expect.equal ((4), 0x0201) (new_z80.pc |> toInt, new_z80.env.sp |> toInt)
            ,test "LDIR ED B0" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xED
                               |> setMem (addr_plus_1) 0xB0
                               |> setMem (Z80MemoryAddress(0x5050 - 0x4000)) 0xA0
                               |> setMem (Z80MemoryAddress(0x5051 - 0x4000)) 0xA5
                               |> setMem (Z80MemoryAddress(0x5052 - 0x4000)) 0xAA
                               |> setMem (Z80MemoryAddress(0x5053 - 0x4000)) 0xBA
                               |> setMem (Z80MemoryAddress(0x5054 - 0x4000)) 0xB5
                  z80_1 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 |> fromInt },
                                                        main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
                  new_z80 = z80_1 |> execute_instruction z80rom |> execute_instruction z80rom |> execute_instruction z80rom |> execute_instruction z80rom
                  mem_vals = [(mem (0x6000 |> fromInt) new_z80.env.time z80rom new_z80.env.ram).value,
                              (mem (0x6001 |> fromInt) new_z80.env.time z80rom new_z80.env.ram).value,
                              (mem (0x6002 |> fromInt) new_z80.env.time z80rom new_z80.env.ram).value,
                              (mem (0x6003 |> fromInt) new_z80.env.time z80rom new_z80.env.ram).value,
                              (mem (0x6004 |> fromInt) new_z80.env.time z80rom new_z80.env.ram).value]
               in
                  Expect.equal {pc=pc_addr_plus_2, b=0x00, c=0x00,d=0x60,e=0x05,hl=0x5055 |> fromInt,mem=[0xA0, 0xA5, 0xAA, 0xBA, 0xB5]}
                  {pc=new_z80.pc, b=new_z80.main.b, c=new_z80.main.c,e=new_z80.main.e,d=new_z80.main.d,hl=new_z80.main.hl, mem=mem_vals}
            ,test "0xED 78 IN A, (C)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xED
                               |> setMem (addr_plus_1) 0x78
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5, c = 0x5F }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal {pc=(pc_addr_plus_2), fr=0xFF, a=0xFF} {pc=new_z80.pc, fr=new_z80.flags.fr, a=new_z80.flags.a}
            ,test "0xE1 POP HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xE1
                               |> setMem sp 0x45
                               |> setMem (sp_plus_1) 0x34
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5, c = 0x5F }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal (ram_addr_plus_1, 0x3445, (sp_addr |> toInt) + 2) (new_z80.pc, (new_z80.main.hl |> toInt), (new_z80.env.sp |> toInt))
            --,test "0xFD 0xE1 POP IY" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xFD
            --                   |> setMem (addr_plus_1) 0xE1
            --                   |> setMem sp 0x45
            --                   |> setMem (sp_plus_1) 0x34
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, b = 0xA5, c = 0x5F } }
            --   in
            --      Expect.equal ((addr + 2), 0x3445, sp + 2) (new_z80.pc, new_z80.main.iy, new_z80.env.sp)
            --,test "0xE3 EX (SP),HL" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xE3
            --                   |> setMem sp 0x45
            --                   |> setMem (sp + 1) 0x34
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0xA000 } }
            --   in
            --      Expect.equal
            --      {pc=new_z80.pc, sp=new_z80.env.sp, hl=new_z80.main.hl, top=(new_z80.env |> mem16 sp z80rom).value}
            --      {pc = (addr + 1), sp = sp, hl = 0x3445, top=0xA000}
            --,test "0xDD 0xE3 EX (SP),IX" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xDD
            --                   |> setMem (addr + 1) 0xE3
            --                   |> setMem sp 0x45
            --                   |> setMem (sp + 1) 0x34
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | ix = 0xA000 } }
            --   in
            --      Expect.equal
            --      {pc=new_z80.pc, sp=new_z80.env.sp, ix=new_z80.main.ix, top=(new_z80.env |> mem16 sp z80rom).value}
            --      {pc = (addr + 2), sp = sp, ix = 0x3445, top=0xA000}
            --,test "0xEB (EX DE, HL)" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xEB
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 },
            --                                            main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
            --   in
            --      Expect.equal {pc=(addr + 1), hl=0x6000, d=0x50, e=0x51}
            --      {pc=new_z80.pc, hl=new_z80.main.hl, d=new_z80.main.d, e =new_z80.main.e}
            --,test "0xE5 PUSH HL" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xE5
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0xA000 } }
            --   in
            --      Expect.equal
            --      {pc=new_z80.pc, sp=new_z80.env.sp, top=(new_z80.env |> mem16 (sp - 2) z80rom).value}
            --      {pc = (addr + 1), sp = (sp - 2), top=0xA000}
            --,test "0xFD 0xE5 PUSH IY" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xFD
            --                   |> setMem (addr + 1) 0xE5
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | iy = 0xA000 } }
            --   in
            --      Expect.equal
            --      {pc=new_z80.pc, sp=new_z80.env.sp, top=(new_z80.env |> mem16 (sp - 2) z80rom).value}
            --      {pc = (addr + 2), sp = (sp - 2), top=0xA000}
         ]
      ]
