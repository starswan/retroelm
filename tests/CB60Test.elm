module CB60Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

        addr_1 =
            addr + 1

        sp =
            0xF765

        hl =
            0x1234

        old_z80 =
            Z80.constructor

        old_z80env =
            old_z80.env

        z80main =
            old_z80.main

        z80 =
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            z80.env |> setMem addr 0xCB

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x60 BIT 4,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x60

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem addr_1 0x60

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, b = 0x10 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x61 BIT 4,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x61

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x61

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, c = 0x10 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x62 BIT 4,D"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x62

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x20 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x62

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x10 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x63 BIT 4,E"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x63

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0x20 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x63

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, e = 0x54 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x64 BIT 4,H"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x64

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x45 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x64

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x1545, e = 0x54 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x65 BIT 4,L"
            [ test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x65

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6555 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            , test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem (addr + 1) 0x65

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6440 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            ]
        , describe "0xCB 0x66 BIT 4,(HL)"
            [ test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x66
                                |> setMem 0x6545 0x10

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            , test "BIT 4, (IX + d) unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x66
                                |> setMem 0x6545 0x00

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6540, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "BIT 4, (IX + d) set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0xCB
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x66
                                |> setMem 0x6545 0x10

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6540, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 4, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x67 BIT 4,A"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x67

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x50 }
                                    , flags = { flags | a = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr + 1) 0x67

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545, d = 0x51 }
                                    , flags = { flags | a = 0x10 }
                                }
                    in
                    Expect.equal ( addr + 2, True ) ( new_z80.pc, new_z80.flags.fr /= 0 )
            ]
        ]
