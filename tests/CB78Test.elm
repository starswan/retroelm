module CB78Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr_int =
            0x5800
            
        addr = addr_int |> fromInt

        addr_int_1 =
            addr_int + 1

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
            { old_z80 | pc = addr, env = { old_z80env | sp = sp |> fromInt }, main = { z80main | hl = hl |> fromInt } }

        flags =
            z80.flags

        z80env =
            z80.env |> setMem addr_int 0xCB

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ describe "0xCB 0x78 BIT 7,B"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int_1 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x00 }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int_1 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x80 }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x79 BIT 7,C"
            [ test "unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int_1 0x79

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x00 }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int_1 0x79

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x80 }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7A BIT 7,D"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7A

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, d = 0x00 }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7A

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x80 }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7B BIT 7,E"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x00 }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x84 }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7C BIT 7,H"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x7F45|> fromInt }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x8045|> fromInt }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7D BIT 7,L"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x647F|> fromInt }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6580|> fromInt }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7E BIT 7,(HL)"
            [ test "set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7E
                                |> setMem 0x6545 0x80

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545|> fromInt }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            , test "BIT 7, (IX + d) unset" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int 0xDD
                                |> setMem (addr_int_1) 0xCB
                                |> setMem (addr_int + 2) 0x05
                                |> setMem (addr_int + 3) 0x7E
                                |> setMem 0x6545 0x7F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 |> fromInt}
                                }
                    in
                    Expect.equal ( addr_int + 4, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "BIT 7, (IX + d) set" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr_int 0xDD
                                |> setMem (addr_int_1) 0xCB
                                |> setMem (addr_int + 2) 0x05
                                |> setMem (addr_int + 3) 0x7E
                                |> setMem 0x6545 0x80

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6540 |> fromInt}
                                }
                    in
                    Expect.equal ( addr_int + 4, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        , describe "0xCB 0x7F BIT 7,A"
            [ test "(unset)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x7F }
                                }
                    in
                    Expect.equal ( addr_int +2, 0x00 ) ( new_z80.pc|> toInt, new_z80.flags.fr )
            , test "(set)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem (addr_int_1) 0x7F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x80 }
                                }
                    in
                    Expect.equal ( addr_int +2, True ) ( new_z80.pc|> toInt, new_z80.flags.fr /= 0 )
            ]
        ]
