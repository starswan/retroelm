module CB58Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, incrementBy2, incrementBy3, toInt)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr_int =
            0x5800

        addr =
            0x5800 |> fromInt

        addr_1 =
            addr_int + 1 |> fromInt

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
            { old_z80 | pc = addr, env = { old_z80env | sp = sp|> fromInt }, main = { z80main | hl = hl|> fromInt } }

        flags =
            z80.flags

        z80env =
            z80.env |> setMem addr 0xCB

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x58 BIT 3,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x58

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765|> fromInt }
                                    , main = { z80main | hl = 0x6545|> fromInt, b = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xCB
                                |> setMem addr_1 0x58

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765|> fromInt }
                                    , main = { z80main | hl = 0x6545|> fromInt, b = 0x08 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x59 BIT 3,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x59

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765|> fromInt }
                                    , main = { z80main | hl = 0x6545|> fromInt, c = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr_int + 2, False ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_1 0x59

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765|> fromInt }
                                    , main = { z80main | hl = 0x6545|> fromInt, c = 0x08 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
            ]
        , test "0xCB 0x5A BIT 3,D (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5A

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, d = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x5A BIT 3,D (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5A

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, d = 0x08 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x4B BIT 3,E (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5B

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, e = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x5B BIT 3,E (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5B

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, e = 0x08 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5C BIT 3,H (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5C

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6445|> fromInt }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x5C BIT 3,H (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5C

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x0845|> fromInt }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5D BIT 3,L (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5D

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6444|> fromInt }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x5D BIT 3,L (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5D

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6508|> fromInt }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x05E BIT 3,(HL) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5E
                            |> setMem (0x6545 |> fromInt) 0x50

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545|> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x05E BIT 3,(HL) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5E
                            |> setMem (0x6545 |> fromInt) 0x08

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545|> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xDD 0xCB 0x05 0x5E BIT 3, (IX + d) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem addr_1 0xCB
                            |> setMem (addr |> incrementBy2) 0x05
                            |> setMem (addr |> incrementBy3) 0x5E
                            |> setMem (0x6545 |> fromInt) 0x50

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | ix = 0x6540|> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                in
                Expect.equal ( addr_int + 4, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x5E BIT 3, (IX + d) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem addr_1 0xCB
                            |> setMem (addr |> incrementBy2) 0x05
                            |> setMem (addr |> incrementBy3) 0x5E
                            |> setMem (0x6545 |> fromInt) 0x08

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | ix = 0x6540|> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }

                in
                Expect.equal ( addr_int + 4, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x5F BIT 3,A (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5F

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, d = 0x50 }
                                , flags = { flags | a = 0x30 }
                            }
                in
                Expect.equal ( addr_int + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x5F BIT 3,A (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem addr_1 0x5F

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765|> fromInt }
                                , main = { z80main | hl = 0x6545|> fromInt, d = 0x51 }
                                , flags = { flags | a = 0x08 }
                            }
                in
                Expect.equal ( addr_int + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        ]
