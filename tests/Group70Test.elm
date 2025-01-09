module Group70Test exposing (..)

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
            [ test "0x78 LD A,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc, new_z80.flags.a )
            , test "0x70 LD (HL),B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x70

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x70 LD (IY+m), B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x70
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x2545, iy = 0x6543, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0x74 LD (HL),H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x74

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc, mem_value.value )
            , test "0xFD 0x74 LD (IY+m),H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x74
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6543, hl = 0x2545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x25 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x74 LD (IX+m),H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x74
                                |> setMem (addr + 2) 0x02
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6543, hl = 0x2545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x25 ) ( new_z80.pc, mem_value.value )
            ]
        ]
