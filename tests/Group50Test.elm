module Group50Test exposing (..)

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
            [ test "0x53 LD D,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x53

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc, new_z80.main.d )
            , test "0x5A LD E,D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x5A

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x34 ) ( new_z80.pc, new_z80.main.e )
            , test "0x5E LD E, (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x5E
                                |> setMem 0x6545 0x27

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x27 ) ( new_z80.pc, new_z80.main.e )
            ]
        ]
