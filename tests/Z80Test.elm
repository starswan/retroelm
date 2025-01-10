module Z80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, setMem)
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
        [ describe "8 bit loads"
            [ test "0x87 ADD A,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x87

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x02 }
                                    , main = { z80main | hl = 0x6545 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x04 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "0xB8 - -xBF CP"
            [ test "0xBC CP H greater" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0245 }
                                    , flags = { flags | a = 0x06 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -3, ff = 4, fr = 4 }
                        { pc = new_z80.pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H less" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0645 }
                                    , flags = { flags | a = 0x02 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 2, fb = -7, ff = -44, fr = 252 }
                        { pc = new_z80.pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H equal" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0645 }
                                    , flags = { flags | a = 0x06 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -7, ff = 0, fr = 0 }
                        { pc = new_z80.pc, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ]
        ]
