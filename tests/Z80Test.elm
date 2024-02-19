module Z80Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, set_mem, set_mem16)

suite : Test
suite =
   let
       addr = 30000
       old_z80 = Z80.constructor
       z80 = { old_z80 | pc = addr }
       flags = z80.flags
       z80env = z80.env
       z80main = z80.main
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "NOP"
            [
               test "0x00" <|
                  \_ ->
                     let
                        z80inc = { z80 | env = z80env |> set_mem addr 0x00 } |> Z80.execute_instruction
                     in
                        Expect.equal ((addr + 1), 4) (z80inc.pc, z80inc.env.time.cpu_time - z80.env.time.cpu_time)
            ],
         describe "16 bit load immediate"
         [
            test "0x01 LD BC,nn" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x01
                            |> set_mem (addr + 1) 0x34
                            |> set_mem (addr + 2) 0x45
                  z80_after_01 = { z80 | env = new_env } |> Z80.execute_instruction
               in
                  Expect.equal ((addr + 3), 0x45, 0x34) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
            , test "0x11 LD DE,nn" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x11
                               |> set_mem (addr + 1) 0x34
                               |> set_mem (addr + 2) 0x45
                  new_z80 = execute_instruction { z80 | env = new_env }
               in
                  Expect.equal ((addr + 3), 0x45, 0x34) (new_z80.pc, new_z80.main.d, new_z80.main.e)
            ,test "0x21 - LD HL, nn" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x21
                               |> set_mem (addr + 1) 0xC6
                               |> set_mem (addr + 2) 0x15
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x15C6) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x21 - LD IY, nn" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x21
                               |> set_mem (addr + 2) 0x05
                               |> set_mem (addr + 3) 0x07
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x0705) (new_z80.pc, new_z80.iy)
         ],
         describe "INSTR 0x02"
            [
               test "0x02" <|
               \_ ->
                  let
                     z80inc = { z80 | env = z80env |> set_mem addr 0x02,
                                      main = { z80main | b = 0x45, c = 0x34 },
                                                         flags = { flags | a = 0x27 } }
                     z80_after_01 = Z80.execute_instruction z80inc
                     mem_value = mem 0x4534 z80_after_01.env
                  in
                     Expect.equal ((addr + 1), 0x27) (z80_after_01.pc, mem_value.value)
            ],
         describe "INC BC"
         [
            test "0x03" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x03,
                                                             main = { z80main | b = 0x45, c = 0xFF } }
               in
                  Expect.equal ((addr + 1), 0x46, 0x00) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
         ],
         describe "DEC B"
         [
            test "0x05" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x05,
                                                             main = { z80main | b = 0x45 } }
               in
                  Expect.equal ((addr + 1), 0x44) (z80_after_01.pc, z80_after_01.main.b)
         ],
         describe "RLCA"
         [
            test "0x07" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x07
                            |> set_mem (addr + 1) 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             flags = { flags | a = 0x87 } }
               in
                  -- This is RLCA - bit 7 goes into bit 0 and carry flag
                  Expect.equal ((addr + 1), 0x0F) (z80_after_01.pc, z80_after_01.flags.a)
         ],
         describe "EXX"
         [
            test "0x08" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x08,
                                                             flags = { flags | a = 0x87 } }
               in
                  Expect.equal ((addr + 1), 0x87, z80.alt_flags) (z80_after_01.pc, z80_after_01.alt_flags.a, z80_after_01.flags)
         ],
         describe "ADD HL, 16-bit"
         [
            test "0x09 ADD HL, BC" <|
            \_ ->
               let
                  z80_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x09,
                                                       ix = 0x27, main = { z80main | b = 0x01, c = 0x02, hl = 0x0304 } }
               in
                  Expect.equal ((addr + 1), 0x0406, 0x27) (z80_01.pc, z80_01.main.hl, z80_01.ix)
            ,test "0xDD 0x09 ADD IX, BC" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0xDD |> set_mem (addr + 1) 0x09,
                                                             ix = 0x05, main = { z80main | b = 0x01, c = 0x02, hl = 0x3445 } }
               in
                  Expect.equal ((addr + 2), 0x3445, 0x0107) (z80_after_01.pc, z80_after_01.main.hl, z80_after_01.ix)
            ,test "0x19 ADD HL, DE" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x19,
                                                             main = { z80main | d = 0x12, e = 0x23, hl = 0x3445 } }
               in
                  Expect.equal ((addr + 1), 0x4668) (z80_after_01.pc, z80_after_01.main.hl)
            ,test "0xFD 0x19 ADD IY, DE" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0xFD |> set_mem (addr + 1) 0x19,
                                                             iy = 0x05, main = { z80main | d = 0x01, e = 0x02, hl = 0x3445 } }
               in
                  Expect.equal ((addr + 2), 0x3445, 0x0107) (z80_after_01.pc, z80_after_01.main.hl, z80_after_01.iy)
            ,test "0x29 ADD HL,HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x29
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 1), 0x8668) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x29 ADD IX, IX" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0xDD |> set_mem (addr + 1) 0x29,
                                                             ix = 0x05, main = { z80main | b = 0x01, c = 0x02, hl = 0x3445 } }
               in
                  Expect.equal ((addr + 2), 0x3445, 0x000A) (z80_after_01.pc, z80_after_01.main.hl, z80_after_01.ix)
            ,test "0x39 ADD HL,SP" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x39
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x4321,
                                                        main = { z80main | hl = 0x1234 } }
               in
                  Expect.equal ((addr + 1), 0x5555) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x39 ADD IY,SP" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x39
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x4321, iy = 0x1234,
                                                        main = { z80main | hl = 0x4234 } }
               in
                  Expect.equal ((addr + 2), 0x5555) (new_z80.pc, new_z80.iy)
         ],
         describe "LD A, (16 bit)"
         [
            test "0x0A - LD A,(BC)" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x0A
                            |> set_mem 0x4546 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             main = { z80main | b = 0x45, c = 0x46 } }
               in
                  Expect.equal ((addr + 1), 0x78) (z80_after_01.pc, z80_after_01.flags.a)
            ,test "0x1A - LD A,(DE)" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x1A
                            |> set_mem 0x4546 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             main = { z80main | d = 0x45, e = 0x46 } }
               in
                  Expect.equal ((addr + 1), 0x78) (z80_after_01.pc, z80_after_01.flags.a)
         ],
         describe "DEC 16 bit"
         [
            test "0x0B DEC BC" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x0B,
                                                             main = { z80main | b = 0x45, c = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x44, 0xFF) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
            ,test "0x1B DEC DE" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x1B,
                                                             main = { z80main | d = 0x45, e = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x44, 0xFF) (z80_after_01.pc, z80_after_01.main.d, z80_after_01.main.e)
            ,test "0x2B DEC HL" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x2B
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x64FF) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x2B DEC IX" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0xDD
                            |> set_mem (addr + 1) 0x2B
                  new_z80 = execute_instruction { z80 | env = new_env, ix = 0,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6500, 0xFFFF) (new_z80.pc, new_z80.main.hl, new_z80.ix)
            ,test "0x3B DEC SP" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x3B
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8756,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x8755) (new_z80.pc, new_z80.sp)
         ],
         describe "INC 8 bit"
         [
            test "INC B - 0x04" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x04,
                                                             main = { z80main | b = 0x45 } }
               in
                  Expect.equal ((addr + 1), 0x46) (z80_after_01.pc, z80_after_01.main.b)
            ,test "INC C - 0x0C" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x0C,
                                                             main = { z80main | b = 0x45, c = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x45, 0x01) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
            ,test "INC D 0x14" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x14
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x38 } }
               in
                  Expect.equal ((addr + 1), 0x66, 0xFF) (new_z80.pc, new_z80.main.d, new_z80.main.e)
            ,test "INC E - 0x1C" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x1C,
                                                             main = { z80main | d = 0x45, e = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x45, 0x01) (z80_after_01.pc, z80_after_01.main.d, z80_after_01.main.e)
            ,test "0x24 INC H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x24
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6545,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6645, 0x6545) (new_z80.pc, new_z80.main.hl, new_z80.iy)
            ,test "0xFD 0x24 INC IYH" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x24
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6545,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0x6645) (new_z80.pc, new_z80.main.hl, new_z80.iy)
            ,test "0x25 DEC H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x25
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6445) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x25 DEC IXH" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x25
                  new_z80 = execute_instruction { z80 | env = new_env, ix = 0x0045,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0xFF45) (new_z80.pc, new_z80.main.hl, new_z80.ix)
            ,test "0x2C INC L" <|
             \_ ->
                let
                   new_env = z80env
                             |> set_mem addr 0x2C
                   new_z80 = execute_instruction { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x6501) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2C INC IYL" <|
             \_ ->
                let
                   new_env = z80env
                             |> set_mem addr 0xFD
                             |> set_mem (addr + 1) 0x2C
                   new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6500,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 2), 0x6500, 0x6501) (new_z80.pc, new_z80.main.hl, new_z80.iy)
            ,test "0x2D DEC L" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x2D
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x65FF) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2D DEC IYL" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0xFD
                            |> set_mem (addr + 1) 0x2D
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6500,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x65FF, 0x6500) (new_z80.pc, new_z80.iy, new_z80.main.hl)
            ,test "0x3C INC A" <|
             \_ ->
                let
                   new_env = z80env
                             |> set_mem addr 0x3C
                   new_z80 = execute_instruction { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x3A) (new_z80.pc, new_z80.flags.a)
         ],
         describe "DEC 8 bit"
         [
            test "DEC C - 0x0D" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x0D,
                                                             main = { z80main | b = 0x45, c = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x45, 0xFF) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
            ,test "DEC E - 0x1D" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x1D,
                                                             main = { z80main | d = 0x45, e = 0x00 } }
               in
                  Expect.equal ((addr + 1), 0x45, 0xFF) (z80_after_01.pc, z80_after_01.main.d, z80_after_01.main.e)
            ,test "DEC A - 0x3D" <|
            \_ ->
               let
                   new_env = z80env
                             |> set_mem addr 0x3D
                   new_z80 = execute_instruction { z80 | env = new_env,
                                                         main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
                in
                   Expect.equal ((addr + 1), 0x38) (new_z80.pc, new_z80.flags.a)
         ],
         describe "LD 8-bit,n"
         [
            test "0x06 LD B,n" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x06
                            |> set_mem (addr + 1) 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env }
               in
                  Expect.equal ((addr + 2), 0x78) (z80_after_01.pc, z80_after_01.main.b)
            ,test "LD C,n - 0x0E" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x0E
                            |> set_mem (addr + 1) 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             main = { z80main | b = 0x45, c = 0x00 } }
               in
                  Expect.equal ((addr + 2), 0x45, 0x78) (z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c)
            ,test "LD D,n - 0x16" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x16
                               |> set_mem (addr + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x38 } }
               in
                  Expect.equal ((addr + 2), 0x34, 0xFF) (new_z80.pc, new_z80.main.d, new_z80.main.e)
            ,test "LD E,n - 0x1E" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x1E
                            |> set_mem (addr + 1) 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             main = { z80main | d = 0x45, e = 0x00 } }
               in
                  Expect.equal ((addr + 2), 0x78) (z80_after_01.pc, z80_after_01.main.e)
            ,test "0x26 - LD H,n" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x26
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal ((addr + 2), 0x0545) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x26 - LD IYH,n" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x26
                               |> set_mem (addr + 2) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6545,
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal ((addr + 3), 0x6545, 0x0545) (new_z80.pc, new_z80.main.hl, new_z80.iy)
            ,test "0x2E LD L,n" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x2E
                            |> set_mem (addr + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6534) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x2E LD IXL,n" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0xDD
                            |> set_mem (addr + 1) 0x2E
                            |> set_mem (addr + 2) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env, ix = 0x6500,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x6534, 0x6500) (new_z80.pc, new_z80.ix, new_z80.main.hl)
            ,test "LD A,n - 0x3E" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x3E
                            |> set_mem (addr + 1) 0x78
                  z80_after_01 = execute_instruction { z80 | env = new_env,
                                                             main = { z80main | d = 0x45, e = 0x00 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x78) (z80_after_01.pc, z80_after_01.flags.a)
         ],
         describe "RRCA"
         [
            test "RRCA - 0x0F" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x0F,
                                                             flags = { flags | a = 0x80 } }
               in
                  Expect.equal ((addr + 1), 0x40) (z80_after_01.pc, z80_after_01.flags.a)
         ],
         describe "DJNZ - 0x10"
         [
            test "Jump" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x10 |> set_mem (addr + 1) 0x02,
                                                             main = { z80main | b = 0x45 } }
               in
                  Expect.equal ((addr + 4), 0x44) (z80_after_01.pc, z80_after_01.main.b)
            , test "Dont jump" <|
            \_ ->
               let
                  z80_after_01 = execute_instruction { z80 | env = z80env |> set_mem addr 0x10 |> set_mem (addr + 1) 0x02,
                                                             main = { z80main | b = 0x01 } }
               in
                  Expect.equal ((addr + 2), 0x00) (z80_after_01.pc, z80_after_01.main.b)
         ],
         describe "LD (DE), A"
         [
            test "Do it 0x12" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x12
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0x45 },
                                                                           flags = { flags | a = 0x38 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0x38) (new_z80.pc, mem_value.value)
         ],
         describe "INC DE"
         [
            test "Do it 0x13" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x13
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x38 } }
               in
                  Expect.equal ((addr + 1), 0x66, 0x00) (new_z80.pc, new_z80.main.d, new_z80.main.e)
         ],
         describe "DEC D"
         [
            test "Do it 0x15" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x15
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x38 } }
               in
                  Expect.equal ((addr + 1), 0x64, 0xFF) (new_z80.pc, new_z80.main.d, new_z80.main.e)
         ],
         describe "RLA"
         [
            test "Do it 0x17" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x17
                               |> set_mem (addr + 1) 0x34
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x72) (new_z80.pc, new_z80.flags.a)
         ],
         describe "JR n"
         [
            test "Do it 0x18" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x18
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x39 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "RRA"
         [
            test "Do it 0x1F" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x1F
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | d = 0x65, e = 0xFF },
                                                                           flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x1C) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x20 - JR NZ,n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x20
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x20
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 1 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
            ,test "Jump backwards" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x20
                               |> set_mem (addr + 1) 0xFB
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 1 } }
               in
                  Expect.equal (addr - 3) new_z80.pc
         ],
         describe "0x22 - LD (nn), HL"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x22
                               |> set_mem (addr + 1) 0x77
                               |> set_mem (addr + 2) 0x55
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x5D9F }, flags = { flags | a = 0x39 } }
                  mem_value = mem16 0x5577 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x5D9F) (new_z80.pc, mem_value.value)
         ],
         describe "16 bit Increment"
         [
            test "0x23 INC HL" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x23
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x6546) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x23 INC IX" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x23
                  new_z80 = execute_instruction { z80 | env = new_env, ix = 0xFFFF,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x6545, 0) (new_z80.pc, new_z80.main.hl, new_z80.ix)
         ],
         describe "0x27 - DAA"
         [
            test "No idea how to do this..." <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x27
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x39) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x28 JR Z, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x28
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | fr = 1 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x28
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, fr = 0 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "load reg indirect"
         [
            test "0x2A LD HL,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x2A
                               |> set_mem (addr + 1) 0x34
                               |> set_mem (addr + 2) 0x54
                               |> set_mem16 0x5434 0x8723
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 3), 0x8723) (new_z80.pc, new_z80.main.hl)
            ,test "0xFD 0x2A LD IY,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x2A
                               |> set_mem (addr + 2) 0x34
                               |> set_mem (addr + 3) 0x54
                               |> set_mem16 0x5434 0x8723
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x4334,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 4), 0x8723) (new_z80.pc, new_z80.iy)
            ,test "0x3A LD A,(nn)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x3A
                               |> set_mem (addr + 1) 0x34
                               |> set_mem (addr + 2) 0x54
                               |> set_mem 0x5434 0x87
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x4334 } }
               in
                  Expect.equal ((addr + 3), 0x87) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x2F CPL"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                            |> set_mem addr 0x2F
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6500 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0xC6) (new_z80.pc, new_z80.flags.a)
         ],
         describe "0x30 JR NC, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x30
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | ff = 0x100 } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x30
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, ff = 0xFF } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "0x31 - LD SP, nn"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x31
                               |> set_mem (addr + 1) 0x05
                               |> set_mem (addr + 2) 0x07
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x0705) (new_z80.pc, new_z80.sp)
         ],
         describe "0x32 - LD (nn), A"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x32
                               |> set_mem (addr + 1) 0x77
                               |> set_mem (addr + 2) 0x55
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x5577 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x39) (new_z80.pc, mem_value.value)
         ],
         describe "0x33 INC SP"
         [
            test "Do it" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x33
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x8766) (new_z80.pc, new_z80.sp)
         ],
         describe "16 bit indirect"
         [
            test "0x34 INC (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x34
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0x79) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x34 INC (IX)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x34
                               |> set_mem (addr + 2) 0xFF
                               |> set_mem 0x6544 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix = 0x6545,
                                                        main = { z80main | hl = 0x2545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6544 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x79) (new_z80.pc, mem_value.value)
            ,test "0x35 DEC (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x35
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0x77, 119) (new_z80.pc, mem_value.value, new_z80.flags.fr)
            ,test "0x35 DEC (HL) going to zero" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x35
                               |> set_mem 0x6545 0x01
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0x00, 0) (new_z80.pc, mem_value.value, new_z80.flags.fr)
            ,test "0xFD 0x35 DEC (IY + n)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x35
                               |> set_mem (addr + 2) 0x01
                               |> set_mem 0x6546 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, iy = 0x6545,
                                                        main = { z80main | hl = 0x2545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6546 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x77, 0x2545) (new_z80.pc, mem_value.value, new_z80.main.hl)
         ],
         describe "Indirect indexed load"
         [
            test "0x36 LD (HL),n" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x36
                               |> set_mem (addr + 1) 0xA5
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 2), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x36 LD (IX + m),n" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x36
                               |> set_mem (addr + 2) 0x00
                               |> set_mem (addr + 3) 0xA5
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix = 0x6545,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 4), 0xA5) (new_z80.pc, mem_value.value)
         ],
         describe "Flags"
         [
            test "0x37 SCF" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x37
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | ff = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x100) (new_z80.pc, (Bitwise.and new_z80.flags.ff 0x100))
           ,test "0x3F CCF" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x3F
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | ff = 0x100 } }
               in
                  Expect.equal ((addr + 1), 0) (new_z80.pc, (Bitwise.and new_z80.flags.ff 0x100))
         ],
         describe "0x38 JR C, n"
         [
            test "Dont jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x38
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | ff = 0xFF } }
               in
                  Expect.equal (addr + 2) new_z80.pc
            ,test "Jump" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x38
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        flags = { flags | a = 0x39, ff = 0x100 } }
               in
                  Expect.equal (addr + 7) new_z80.pc
         ],
         describe "8 bit loads"
         [
            test "0x41 LD B,C" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x41
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.b)
            ,test "0x44 LD B,H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x44
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x65) (new_z80.pc, new_z80.main.b)
            ,test "0xDD 0x44 LD B,IXH" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x44
                  new_z80 = execute_instruction { z80 | env = new_env, ix = 0x2398,
                                                        main = { z80main | hl = 0x6545, c = 0x76 } }
               in
                  Expect.equal (addr + 2, 0x23) (new_z80.pc, new_z80.main.b)
            ,test "0x48 LD C,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x48
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.c)
            ,test "0x53 LD D,E" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x53
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, e = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.d)
            ,test "0x5A LD E,D" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x5A
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, d = 0x34 } }
               in
                  Expect.equal (addr + 1, 0x34) (new_z80.pc, new_z80.main.e)
            ,test "0x5E LD E, (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x5E
                               |> set_mem 0x6545 0x27
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, d = 0x34 } }
               in
                  Expect.equal (addr + 1, 0x27) (new_z80.pc, new_z80.main.e)
            ,test "0x60 LD H,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x60
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x7645) (new_z80.pc, new_z80.main.hl)
            ,test "0x6F LD L,A" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x6F
                  new_z80 = execute_instruction { z80 | env = new_env, flags = { flags | a = 0x6F },
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal (addr + 1, 0x656F) (new_z80.pc, new_z80.main.hl)
            ,test "0x87 ADD A,A" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x87
                  new_z80 = execute_instruction { z80 | env = new_env, flags = { flags | a = 0x02 },
                                                        main = { z80main | hl = 0x6545 } }
               in
                  Expect.equal (addr + 1, 0x04) (new_z80.pc, new_z80.flags.a)
            ,test "0x78 LD A,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x78
                  new_z80 = execute_instruction { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.flags.a)
            ,test "0xFD 0x60 LD IYH,B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x60
                  new_z80 = execute_instruction { z80 | env = new_env, iy = 0x6545,
                                                        main = { z80main | hl = 0x6545, b = 0x76 } }
               in
                  Expect.equal (addr + 2, 0x7645, 0x6545) (new_z80.pc, new_z80.iy, new_z80.main.hl)
            ,test "0x66 LD H,(HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x66
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 1), 0x7845) (new_z80.pc, new_z80.main.hl)
            ,test "0xDD 0x66 LD H,(IX+m)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x66
                               |> set_mem (addr + 2) 0x02
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix = 0x6543,
                                                        main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 3), 0x7845, 0x6543) (new_z80.pc, new_z80.main.hl, new_z80.ix)
            ,test "0x70 LD (HL),B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x70
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0xFD 0x70 LD (IY+m), B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x70
                               |> set_mem (addr + 2) 0x02
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, iy = 0x6543,
                                                        main = { z80main | hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 3), 0xA5) (new_z80.pc, mem_value.value)
            ,test "0x74 LD (HL),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0x74
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 1), 0x65) (new_z80.pc, mem_value.value)
            ,test "0xFD 0x74 LD (IY+m),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0x74
                               |> set_mem (addr + 2) 0x02
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, iy = 0x6543,
                                                        main = { z80main | hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
            ,test "0xDD 0x74 LD (IX+m),H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0x74
                               |> set_mem (addr + 2) 0x02
                               |> set_mem 0x6545 0x78
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix = 0x6543,
                                                        main = { z80main | hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
         ],
         describe "0xB8 - -xBF CP"
         [
            test "0xBC CP H greater" <|
            \_ ->
               let
                  new_z80 = execute_instruction { z80 | env = z80env |> set_mem addr 0xBC,
                                                        main = { z80main | hl = 0x0245 },
                                                        flags = { flags | a = 0x06 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 6, fb = -3, ff = 4, fr = 4 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ,test "0xBC CP H less" <|
            \_ ->
               let
                  new_z80 = execute_instruction { z80 | env = z80env |> set_mem addr 0xBC,
                                                        main = { z80main | hl = 0x0645 },
                                                        flags = { flags | a = 0x02 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 2, fb = -7, ff = -44, fr = 252 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ,test "0xBC CP H equal" <|
            \_ ->
               let
                  new_z80 = execute_instruction { z80 | env = z80env |> set_mem addr 0xBC,
                                                        main = { z80main | hl = 0x0645 },
                                                        flags = { flags | a = 0x06 } }
               in
                  Expect.equal { pc = (addr + 1), fa = 6, fb = -7, ff = 0, fr = 0 }
                  { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
         ],
         describe "Bit instructions (CB)"
         [
            test "0xCB 0x00 RLC B" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x00
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545, b = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.b)
            ,test "0xCB 0x01 RLC C" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x01
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                         main = { z80main | hl = 0x6545, c = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.c)
            ,test "0xCB 0x02 RLC D" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x02
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.d)
            ,test "0xCB 0x04 RLC H" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x04
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x5045, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA045) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x05 RLC L" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x05
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x50A0) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x06 RLC (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x06
                               |> set_mem 0x6545 0x31
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env
               in
                  Expect.equal ((addr + 2), 0x62) (new_z80.pc, mem_value.value)
            ,test "0xCB 0x07 RLC A" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xCB
                               |> set_mem (addr + 1) 0x07
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765,
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x30 } }
               in
                  Expect.equal ((addr + 2), 0x60) (new_z80.pc, new_z80.flags.a)
         ],
         describe "CALL(0xCD) and RET(C9)"
         [
            test "Call followed by return" <|
            \_ ->
               let
                  stackp = 0xF000
                  start = 0x5000
                  new_env = z80env
                               |> set_mem start 0xC9
                               |> set_mem (start + 1) 0xCD
                               |> set_mem (start + 2) (Bitwise.and start 0xFF)
                               |> set_mem (start + 3) (shiftRightBy 8 start)
                  z80_1 = { z80 | env = new_env, sp = stackp + 2, pc = (start + 1),
                                  flags = { flags | a = 0x30 } } |> execute_instruction
                  mem_value = z80_1.env |> mem16 stackp
                  z80_2 = z80_1 |> execute_instruction
               in
                  Expect.equal
                  {addr=start, sp1=stackp, stacked=start + 4, addr4=start + 4, sp2=stackp + 2}
                  {addr=z80_1.pc, sp1=z80_1.sp, stacked=mem_value.value, addr4=z80_2.pc, sp2=z80_2.sp}
         ],
         describe "LDIR ED B0"
         [
            test "loop and copy" <|
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
                  z80_1 = execute_instruction { z80 | env = new_env, sp = 0xFF77,
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
         ],
         describe "ADD A, n (0xC6)"
         [
            test "doit" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xC6
                               |> set_mem (addr + 1) 0x16
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0xFF77,
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
               in
                  Expect.equal {pc=(addr + 2), a=0x76}
                  {pc=new_z80.pc, a=new_z80.flags.a}
         ],
         describe "EX DE, HL (0xEB)"
         [
            test "doit" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xEB
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0xFF77,
                                                        main = { z80main | hl = 0x5051, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }, flags = { flags | a = 0x60 } }
               in
                  Expect.equal {pc=(addr + 1), hl=0x6000, d=0x50, e=0x51}
                  {pc=new_z80.pc, hl=new_z80.main.hl, d=new_z80.main.d, e =new_z80.main.e}
         ],
         describe "IX things"
         [
            test "0xDD 0xCB nn 0xC6 SET 0, (IX + n)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0xCB
                               |> set_mem (addr + 2) 0x06
                               |> set_mem (addr + 3) 0xC6
                               |> set_mem 0xA086 0x10
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix=0xA080,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA086 new_z80.env
               in
                  Expect.equal ((addr + 4), 0x11) (new_z80.pc, mem_value.value)
            ,test "0xFD 0xCB nn 0x9E RES 3, (IY + n) -ve" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xFD
                               |> set_mem (addr + 1) 0xCB
                               |> set_mem (addr + 2) 0xFE
                               |> set_mem (addr + 3) 0x9E
                               |> set_mem 0xA07E 0xFF
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, iy=0xA080,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0xA07E new_z80.env
               in
                  Expect.equal ((addr + 4), 0xF7) (new_z80.pc, mem_value.value)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (SET)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0xCB
                               |> set_mem (addr + 2) 0x02
                               |> set_mem (addr + 3) 0x66
                               |> set_mem 0xA082 0x10
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix=0xA080,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x10) (new_z80.pc, new_z80.flags.fr)
            ,test "0xDD 0xCB nn 0x66 BIT 4, (IX + n) (CLEAR)" <|
            \_ ->
               let
                  new_env = z80env
                               |> set_mem addr 0xDD
                               |> set_mem (addr + 1) 0xCB
                               |> set_mem (addr + 2) 0x02
                               |> set_mem (addr + 3) 0x66
                               |> set_mem 0xA082 0xEF
                  new_z80 = execute_instruction { z80 | env = new_env, sp = 0x8765, ix=0xA080,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 4), 0x00) (new_z80.pc, new_z80.flags.fr)
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
         ]
      ]
      --   -- Expect.equal is designed to be used in pipeline style, like this.
      --   , test "reverses a known string" <|
      --       \_ ->
      --           "ABCDEFG"
      --               |> String.reverse
      --               |> Expect.equal "GFEDCBA"
      --   ]