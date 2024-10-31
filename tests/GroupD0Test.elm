module GroupD0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr }

        flags =
            z80.flags

        z80env =
            z80.env

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "16 bit Pop"
            [ test "POP DE (0xD1)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xD1
                                |> setMem 0xFF77 0x16
                                |> setMem 0xFF78 0x56

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 }
                                    , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = addr + 1, d = 0x56, e = 0x16, sp = 0xFF79 } { sp = new_z80.env.sp, pc = new_z80.pc, d = new_z80.main.d, e = new_z80.main.e }
            ]
        , test "0xD9 EXX" <|
            \_ ->
                let
                    alt =
                        z80.alt_main

                    new_env =
                        z80env
                            |> setMem addr 0xD9
                            |> setMem (addr + 1) 0x16

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 }
                                , alt_main = { alt | hl = 0x4040, b = 0x67, c = 0x34, d = 0x12, e = 0x81 }
                                , main = { z80main | hl = 0x5050, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                in
                Expect.equal { pc = addr + 1, hl = 0x4040 } { pc = new_z80.pc, hl = new_z80.main.hl }
        ]
