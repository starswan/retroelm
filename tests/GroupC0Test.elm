module GroupC0Test exposing (..)

import Bitwise exposing (shiftRightBy)
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
            { old_z80 | pc = addr|> fromInt }

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
        [ test "POP BC (0xC1)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xC1
                            |> setMem 0xFF77 0x16
                            |> setMem 0xFF78 0x56

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77|> fromInt }
                                , main = { z80main | hl = 0x5050|> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                            }
                in
                Expect.equal { pc = addr + 1, b = 0x56, c = 0x16, sp = 0xFF79 } { sp = new_z80.env.sp|> toInt, pc = new_z80.pc|> toInt, b = new_z80.main.b, c = new_z80.main.c }
        , describe "0xC2 - JP NZ,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xC2
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal (addr + 3) (new_z80.pc|> toInt)
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xC2
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal 0x3405 (new_z80.pc |> toInt)
            ]
        , test "0xC6 ADD A,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xC6
                            |> setMem (addr + 1) 0x16

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0xFF77|> fromInt }
                                , main = { z80main | hl = 0x5050|> fromInt, d = 0x60, e = 0x00, b = 0x00, c = 0x05 }
                                , flags = { flags | a = 0x60 }
                            }
                in
                Expect.equal { pc = addr + 2, a = 0x76 }
                    { pc = new_z80.pc|> toInt, a = new_z80.flags.a }
        , describe "0xCA - JP Z,nn"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCA
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal (addr + 3) (new_z80.pc|> toInt)
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCA
                                |> setMem (addr + 1) 0x05
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 0 }
                                }
                    in
                    Expect.equal 0x3405 (new_z80.pc|> toInt)
            ]
        , describe "CALL(0xCD) and RET(C9)"
            [ test "Call followed by return" <|
                \_ ->
                    let
                        stackp =
                            0xF000

                        start =
                            0x5000

                        new_env =
                            z80env
                                |> setMem start 0xC9
                                |> setMem (start + 1) 0xCD
                                |> setMem (start + 2) (Bitwise.and start 0xFF)
                                |> setMem (start + 3) (shiftRightBy 8 start)

                        z80_1 =
                            { z80
                                | env = { new_env | sp = stackp + 2 |> fromInt}
                                , pc = start + 1|> fromInt
                                , flags = { flags | a = 0x30 }
                            }
                                |> execute_instruction z80rom

                        mem_value =
                            z80_1.env |> mem16 stackp z80rom

                        z80_2 =
                            z80_1 |> execute_instruction z80rom
                    in
                    Expect.equal
                        { addr = start, sp1 = stackp, stacked = start + 4, addr4 = start + 4, sp2 = stackp + 2 }
                        { addr = z80_1.pc|> toInt, sp1 = z80_1.env.sp|> toInt, stacked = mem_value.value, addr4 = z80_2.pc|> toInt, sp2 = z80_2.env.sp|> toInt }
            ]
        ]
