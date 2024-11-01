module GroupF0Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, setMem, setMem16)
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
         describe "0xF5 PUSH AF"
         [
            test "doit" <|
            \_ ->
               let
                  alt = z80.alt_main
                  new_env = z80env
                               |> setMem addr 0xF5
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 }, flags = { flags | a = 0x76 },
                                                        alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
                  pushed = new_z80.env |> mem16 0xFF75 z80rom
               in
                  Expect.equal {pc=(addr + 1), sp=0xFF75, push=0x7640}  {pc=new_z80.pc, sp=new_z80.env.sp, push=pushed.value}
         ]
         ,describe "0xF9 LD SP,HL"
         [
            test "doit" <|
            \_ ->
               let
                  alt = z80.alt_main
                  new_env = z80env
                               |> setMem addr 0xF9
                  new_z80 = execute_instruction z80rom { z80 | env = { new_env | sp = 0xFF77 },
                                                        alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 },
                                                        main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 } }
               in
                  Expect.equal {pc=(addr + 1), sp=0x5050}  {pc=new_z80.pc, sp=new_z80.env.sp}
         ]
      ]
