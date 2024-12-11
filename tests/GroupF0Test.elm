module GroupF0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (mem16, setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            30000

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr |> fromInt }

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
        [ test "0xF5 PUSH AF" <|
            \_ ->
                let
                    alt =
                        z80.alt_main

                    new_env =
                        z80env
                            |> setMem (addr |> fromInt) 0xF5

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 |> fromInt }
                                , flags = { flags | a = 0x76 }
                                , alt_main = { alt | hl = 0x4040 |> fromInt, b = 0x67, c = 0x34, d = 0x12, e = 0x81 }
                                , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }

                    pushed =
                        new_z80.env |> mem16 (0xFF75 |> fromInt) z80rom
                in
                Expect.equal { pc = addr + 1, sp = 0xFF75, push = 0x7640 } { pc = new_z80.pc |> toInt, sp = new_z80.env.sp |> toInt, push = pushed.address |> toInt }
        , test "0xF9 LD SP,HL" <|
            \_ ->
                let
                    alt =
                        z80.alt_main

                    new_env =
                        z80env
                            |> setMem (addr |> fromInt) 0xF9

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77 |> fromInt }
                                , alt_main = { alt | hl = 0x4040 |> fromInt, b = 0x67, c = 0x34, d = 0x12, e = 0x81 }
                                , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                in
                Expect.equal { pc = addr + 1, sp = 0x5050 } { pc = new_z80.pc |> toInt, sp = new_z80.env.sp |> toInt }
        ]
