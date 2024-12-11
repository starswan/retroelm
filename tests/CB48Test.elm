module CB48Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, toInt)
import Z80Env exposing (setMem)
import Z80Rom


suite : Test
suite =
    let
        addr =
            0x5800

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
            { old_z80 | pc = addr |> fromInt, env = { old_z80env | sp = sp |> fromInt }, main = { z80main | hl = hl |> fromInt } }

        flags =
            z80.flags

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 0x48 BIT 1,B (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x48

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, b = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x48 BIT 1,B (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x48

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, b = 0x02 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x49 BIT 1,C (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x49

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, c = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, False ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x49 BIT 1,C (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x49

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, c = 0x02 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x4A BIT 1,D (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4A

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4A BIT 1,D (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4A

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x02 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4B BIT 1,E (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4B

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x00 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4B BIT 1,E (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4B

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, e = 0x02 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4C BIT 1,H (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4C

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6445 |> fromInt }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4C BIT 1,H (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4C

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x0245 |> fromInt }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4D BIT 1,L (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4D

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6444 |> fromInt }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4D BIT 1,L (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4D

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6502 |> fromInt }
                            }
                in
                Expect.equal ( addr + 2, 0x02 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x04E BIT 1,(HL) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4E
                            |> setMem 0x6545 0x50

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x04E BIT 1,(HL) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4E
                            |> setMem 0x6545 0x02

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xDD 0xCB 0x05 0x4E BIT 1, (IX + d) unset" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x05
                            |> setMem (addr + 3) 0x4E
                            |> setMem 0x6545 0x50

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | ix = 0x6540 |> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 4, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xDD 0xCB 0x05 0x4E BIT 1, (IX + d) set" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0xCB
                            |> setMem (addr + 2) 0x05
                            |> setMem (addr + 3) 0x4E
                            |> setMem 0x6545 0x02

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | ix = 0x6540 |> fromInt, b = 0xA5 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 4, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        , test "0xCB 0x4F BIT 1,A (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4F

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x50 }
                                , flags = { flags | a = 0x38 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc |> toInt, new_z80.flags.fr )
        , test "0xCB 0x4F BIT 1,A (set)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x4F

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 |> fromInt }
                                , main = { z80main | hl = 0x6545 |> fromInt, d = 0x51 }
                                , flags = { flags | a = 0x02 }
                            }
                in
                Expect.equal ( addr + 2, True ) ( new_z80.pc |> toInt, new_z80.flags.fr /= 0 )
        ]
