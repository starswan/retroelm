module GroupE0Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (mem16, setMem)
import Z80Rom


suite : Test
suite =
    let
        int_addr =
            30000

        addr =
            int_addr |> fromInt

        old_z80 =
            Z80.constructor

        z80 =
            { old_z80 | pc = addr }

        z80env =
            z80.env

        z80main =
            z80.main

        z80rom =
            Z80Rom.constructor
    in
    describe "Z80.execute_instruction"
        -- Nest as many descriptions as you like.
        [ describe "RST nn"
            [ test "0xD7 RST 10" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xD7
                                |> setMem (0xFF77 |> fromInt) 0x16
                                |> setMem (0xFF78 |> fromInt) 0x56

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = 0x10, sp = 0xFF75 } { sp = new_z80.env.sp |> toInt, pc = new_z80.pc |> toInt }
            , test "0xDF RST 18" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDF
                                |> setMem (0xFF75 |> fromInt) 0x16
                                |> setMem (0xFF76 |> fromInt) 0x56

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = 0x18, sp = 0xFF75, mem = int_addr + 1 } { sp = new_z80.env.sp |> toInt, pc = new_z80.pc |> toInt, mem = mem16 (0xFF75 |> fromInt) z80rom new_z80.env |> .address |> toInt }
            ]
        , describe "16 bit Pop"
            [ test "POP HL (0xE1)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xE1
                                |> setMem (0xFF77 |> fromInt) 0x16
                                |> setMem (0xFF78 |> fromInt) 0x56

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0xFF77 |> fromInt }
                                    , main = { z80main | hl = 0x5050 |> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                }
                    in
                    Expect.equal { pc = int_addr + 1, hl = 0x5616, sp = 0xFF79 } { sp = new_z80.env.sp |> toInt, pc = new_z80.pc |> toInt, hl = new_z80.main.hl |> toInt }
            ]
        ]
