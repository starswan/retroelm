module Z80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (Z80Address(..), fromInt, toInt)
import Z80Env exposing (setMem)
import Z80Rom
import Z80WriteableAddress exposing (Z80WriteableAddress(..))

suite : Test
suite =
   let
       addrInt = 30000
       addr = addrInt |> fromInt
       writeAddr = Z80MemoryAddress (addrInt - 0x4000)
       writeAddr_plus_1 = Z80MemoryAddress (addrInt +1  - 0x4000)
       writeAddr_plus_2 = Z80MemoryAddress (addrInt +2  - 0x4000)
       writeAddr_plus_3 = Z80MemoryAddress (addrInt +3  - 0x4000)
       old_z80 = Z80.constructor
       z80 = { old_z80 | pc = addr }
       flags = z80.flags
       z80env = z80.env
       z80main = z80.main
       z80rom = Z80Rom.constructor
   in
   describe "Z80.execute_instruction" -- Nest as many descriptions as you like.
      [
         describe "8 bit loads"
         [
            --test "0x41 LD B,C" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x41
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, c = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.b)
            --,test "0x44 LD B,H" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x44
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, c = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x65) (new_z80.pc, new_z80.main.b)
            --,test "0xDD 0x44 LD B,IXH" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xDD
            --                   |> setMem (addr + 1) 0x44
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | ix = 0x2398, hl = 0x6545, c = 0x76 } }
            --   in
            --      Expect.equal (addr + 2, 0x23) (new_z80.pc, new_z80.main.b)
            --,test "0x48 LD C,B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x48
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, b = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.c)
            --,test "0x53 LD D,E" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x53
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, e = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.main.d)
            --,test "0x5A LD E,D" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x5A
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, d = 0x34 } }
            --   in
            --      Expect.equal (addr + 1, 0x34) (new_z80.pc, new_z80.main.e)
            --,test "0x5E LD E, (HL)" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x5E
            --                   |> setMem 0x6545 0x27
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, d = 0x34 } }
            --   in
            --      Expect.equal (addr + 1, 0x27) (new_z80.pc, new_z80.main.e)
            --,test "0x60 LD H,B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x60
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, b = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x7645) (new_z80.pc, new_z80.main.hl)
            --,test "0x6F LD L,A" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x6F
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env, flags = { flags | a = 0x6F },
            --                                            main = { z80main | hl = 0x6545 } }
            --   in
            --      Expect.equal (addr + 1, 0x656F) (new_z80.pc, new_z80.main.hl)
            --,test "0x87 ADD A,A" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x87
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env, flags = { flags | a = 0x02 },
            --                                            main = { z80main | hl = 0x6545 } }
            --   in
            --      Expect.equal (addr + 1, 0x04) (new_z80.pc, new_z80.flags.a)
            --,test "0x78 LD A,B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | hl = 0x6545, b = 0x76 } }
            --   in
            --      Expect.equal (addr + 1, 0x76) (new_z80.pc, new_z80.flags.a)
            --,test "0xFD 0x60 LD IYH,B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xFD
            --                   |> setMem (addr + 1) 0x60
            --      new_z80 = execute_instruction z80rom { z80 | env = new_env,
            --                                            main = { z80main | iy = 0x6545, hl = 0x6545, b = 0x76 } }
            --   in
            --      Expect.equal (addr + 2, 0x7645, 0x6545) (new_z80.pc, new_z80.main.iy, new_z80.main.hl)
            --,test "0x66 LD H,(HL)" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x66
            --                   |> setMem 0x6545 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | hl = 0x6545 }, flags = { flags | a = 0x39 } }
            --   in
            --      Expect.equal ((addr + 1), 0x7845) (new_z80.pc, new_z80.main.hl)
            --,test "0xDD 0x66 LD H,(IX+m)" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xDD
            --                   |> setMem (addr + 1) 0x66
            --                   |> setMem (addr + 2) 0x02
            --                   |> setMem 0x6545 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | ix = 0x6543, hl = 0x6545 }, flags = { flags | a = 0x39 } }
            --   in
            --      Expect.equal ((addr + 3), 0x7845, 0x6543) (new_z80.pc, new_z80.main.hl, new_z80.main.ix)
            --,test "0x70 LD (HL),B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x70
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
            --      mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
            --   in
            --      Expect.equal ((addr + 1), 0xA5) (new_z80.pc, mem_value.value)
            --,test "0xFD 0x70 LD (IY+m), B" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xFD
            --                   |> setMem (addr + 1) 0x70
            --                   |> setMem (addr + 2) 0x02
            --                   |> setMem 0x6545 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | hl = 0x2545, iy = 0x6543, b = 0xA5 }, flags = { flags | a = 0x39 } }
            --      mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
            --   in
            --      Expect.equal ((addr + 3), 0xA5) (new_z80.pc, mem_value.value)
            --,test "0x74 LD (HL),H" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0x74
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
            --      mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
            --   in
            --      Expect.equal ((addr + 1), 0x65) (new_z80.pc, mem_value.value)
            --,test "0xFD 0x74 LD (IY+m),H" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xFD
            --                   |> setMem (addr + 1) 0x74
            --                   |> setMem (addr + 2) 0x02
            --                   |> setMem 0x6545 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | iy = 0x6543, hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
            --      mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
            --   in
            --      Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
            --,test "0xDD 0x74 LD (IX+m),H" <|
            --\_ ->
            --   let
            --      new_env = z80env
            --                   |> setMem addr 0xDD
            --                   |> setMem (addr + 1) 0x74
            --                   |> setMem (addr + 2) 0x02
            --                   |> setMem 0x6545 0x78
            --      new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
            --                                            main = { z80main | ix = 0x6543, hl = 0x2545, b = 0xA5 }, flags = { flags | a = 0x39 } }
            --      mem_value = mem 0x6545 new_z80.env.time  z80rom new_z80.env.ram
            --   in
            --      Expect.equal ((addr + 3), 0x25) (new_z80.pc, mem_value.value)
         ],
         describe "0xB8 - -xBF CP"
         [
            --test "0xBC CP H greater" <|
            --\_ ->
            --   let
            --      new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
            --                                            main = { z80main | hl = 0x0245 },
            --                                            flags = { flags | a = 0x06 } }
            --   in
            --      Expect.equal { pc = (addr + 1), fa = 6, fb = -3, ff = 4, fr = 4 }
            --      { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            --,test "0xBC CP H less" <|
            --\_ ->
            --   let
            --      new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
            --                                            main = { z80main | hl = 0x0645 },
            --                                            flags = { flags | a = 0x02 } }
            --   in
            --      Expect.equal { pc = (addr + 1), fa = 2, fb = -7, ff = -44, fr = 252 }
            --      { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            --,test "0xBC CP H equal" <|
            --\_ ->
            --   let
            --      new_z80 = execute_instruction z80rom { z80 | env = z80env |> setMem addr 0xBC,
            --                                            main = { z80main | hl = 0x0645 },
            --                                            flags = { flags | a = 0x06 } }
            --   in
            --      Expect.equal { pc = (addr + 1), fa = 6, fb = -7, ff = 0, fr = 0 }
            --      { pc = new_z80.pc, fa = new_z80.flags.fa, fb =  new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
         ],
         describe "16 bit Pop"
         [
            test "POP HL (0xE1)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xE1
                               |> setMem 0xFF77 0x16
                               |> setMem 0xFF78 0x56
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
               in
                  Expect.equal {pc=(addr + 1), hl=0x5616, sp=0xFF79}  {sp=new_z80.env.sp, pc=new_z80.pc, hl=new_z80.main.hl}
         ]
      ]
