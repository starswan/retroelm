module Group60Test exposing (..)

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
            [ test "0x60 LD H,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x60

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc, new_z80.main.hl )
            , test "0x6F LD L,A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x6F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x6F }
                                    , main = { z80main | hl = 0x6545 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x656F ) ( new_z80.pc, new_z80.main.hl )
            , test "0xFD 0x60 LD IYH,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x60

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6545, hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645, 0x6545 ) ( new_z80.pc, new_z80.main.iy, new_z80.main.hl )
            , test "0x66 LD H,(HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x66
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7845 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x66 LD H,(IX+m)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x66
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6543, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x7845, 0x6543 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
            ]
        ]
