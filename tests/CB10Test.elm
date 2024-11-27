module CB10Test exposing (..)

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
            test "0xCB 0x10 RL B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x10
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, b = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.b)
            ,test "0xCB 0x11 RL C" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x11
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                         main = { z80main | hl = 0x6545, c = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.c)
            ,test "0xCB 0x12 RL D" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x12
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.d)
            ,test "0xCB 0x13 RL E" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x13
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, e = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA0) (new_z80.pc, new_z80.main.e)
            ,test "0xCB 0x14 RL H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x14
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x5045, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0xA045) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x15 RL L" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x15
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x50A0) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x16 RL (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x16
                               |> setMem 0x6545 0x31
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 2), 0x62) (new_z80.pc, mem_value.value)
            ,test "0xDD 0xCB 0x16 0x45 RL (IX + d)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xDD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x45
                               |> setMem (addr + 3) 0x16
                               |> setMem 0x6545 0x31
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | ix = 0x6500, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0x62) (new_z80.pc, mem_value.value)
            ,test "0xFD 0xCB 0x16 0x45 RL (IY + d)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xFD
                               |> setMem (addr + 1) 0xCB
                               |> setMem (addr + 2) 0x45
                               |> setMem (addr + 3) 0x16
                               |> setMem 0x6545 0x31
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | iy = 0x6500, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 4), 0x62) (new_z80.pc, mem_value.value)
            ,test "0xCB 0x17 RL A" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x17
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x30 } }
               in
                  Expect.equal ((addr + 2), 0x60) (new_z80.pc, new_z80.flags.a)
           , test "0xCB 0x18 RR B" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x18
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, b = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x28) (new_z80.pc, new_z80.main.b)
            ,test "0xCB 0x19 RR C" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x19
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                         main = { z80main | hl = 0x6545, c = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x28) (new_z80.pc, new_z80.main.c)
            ,test "0xCB 0x1A RR D" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1A
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x28) (new_z80.pc, new_z80.main.d)
            ,test "0xCB 0x1B RR E" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1B
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x6545, e = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x28) (new_z80.pc, new_z80.main.e)
            ,test "0xCB 0x1C RR H" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1C
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x5045, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x2845) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x1D RR L" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1D
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0x8765 },
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x39 } }
               in
                  Expect.equal ((addr + 2), 0x5028) (new_z80.pc, new_z80.main.hl)
            ,test "0xCB 0x1E RR (HL)" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1E
                               |> setMem 0x6545 0x31
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x6545, b = 0xA5 }, flags = { flags | a = 0x39 } }
                  mem_value = mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
               in
                  Expect.equal ((addr + 2), 0x18) (new_z80.pc, mem_value.value)
            ,test "0xCB 0x1F RR A" <|
            \_ ->
               let
                  new_env = z80env
                               |> setMem addr 0xCB
                               |> setMem (addr + 1) 0x1F
                  new_z80 = execute_instruction z80rom { z80 | env = new_env,
                                                        main = { z80main | hl = 0x5050, d = 0x50 }, flags = { flags | a = 0x30 } }
               in
                  Expect.equal ((addr + 2), 0x18) (new_z80.pc, new_z80.flags.a)
         ]
