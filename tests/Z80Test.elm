module Z80Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Address exposing (fromInt, toInt)
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
            { old_z80 | pc = addr |> fromInt }

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
            [ test "0x41 LD B,C" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x41

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0x44 LD B,H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x44

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0xDD 0x44 LD B,IXH" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x44

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x2398 |> fromInt, c = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x23 ) ( new_z80.pc |> toInt, new_z80.main.b )
            , test "0x48 LD C,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x48

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.c )
            , test "0x53 LD D,E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x53

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | e = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.main.d )
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
                                    , main = { z80main | d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x34 ) ( new_z80.pc |> toInt, new_z80.main.e )
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
                                    , main = { z80main | hl = 0x6545 |> fromInt, d = 0x34 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x27 ) ( new_z80.pc |> toInt, new_z80.main.e )
            , test "0x60 LD H,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x60

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 |> fromInt, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7645 ) ( new_z80.pc |> toInt, new_z80.main.hl |> toInt )
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
                                    , main = { z80main | hl = 0x6545 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 1, 0x656F ) ( new_z80.pc |> toInt, new_z80.main.hl |> toInt )
            , test "0x87 ADD A,A" <|
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
                                }
                    in
                    Expect.equal ( addr + 1, 0x04 ) ( new_z80.pc |> toInt, new_z80.flags.a )
            , test "0x78 LD A,B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x76 ) ( new_z80.pc |> toInt, new_z80.flags.a )
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
                                    , main = { z80main | iy = 0x6545 |> fromInt, hl = 0x6545 |> fromInt, b = 0x76 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x7645, 0x6545 ) ( new_z80.pc |> toInt, new_z80.main.iy |> toInt, new_z80.main.hl |> toInt )
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
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | hl = 0x6545 |> fromInt }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x7845 ) ( new_z80.pc |> toInt, new_z80.main.hl |> toInt )
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
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | ix = 0x6543 |> fromInt, hl = 0x6545 |> fromInt }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x7845, 0x6543 ) ( new_z80.pc |> toInt, new_z80.main.hl |> toInt, new_z80.main.ix |> toInt )
            , test "0x70 LD (HL),B" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x70

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0xA5 ) ( new_z80.pc |> toInt, mem_value.value )
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
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | hl = 0x2545 |> fromInt, iy = 0x6543 |> fromInt, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0xA5 ) ( new_z80.pc |> toInt, mem_value.value )
            , test "0x74 LD (HL),H" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x74

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | hl = 0x6545 |> fromInt, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x65 ) ( new_z80.pc |> toInt, mem_value.value )
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
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | iy = 0x6543 |> fromInt, hl = 0x2545 |> fromInt, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x25 ) ( new_z80.pc |> toInt, mem_value.value )
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
                                    | env = { new_env | sp = 0x8765 |> fromInt }
                                    , main = { z80main | ix = 0x6543 |> fromInt, hl = 0x2545 |> fromInt, b = 0xA5 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x25 ) ( new_z80.pc |> toInt, mem_value.value )
            ]
        , describe "0xB8 - -xBF CP"
            [ test "0xBC CP H greater" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0245 |> fromInt }
                                    , flags = { flags | a = 0x06 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -3, ff = 4, fr = 4 }
                        { pc = new_z80.pc |> toInt, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H less" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0645 |> fromInt }
                                    , flags = { flags | a = 0x02 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 2, fb = -7, ff = -44, fr = 252 }
                        { pc = new_z80.pc |> toInt, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            , test "0xBC CP H equal" <|
                \_ ->
                    let
                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xBC
                                    , main = { z80main | hl = 0x0645 |> fromInt }
                                    , flags = { flags | a = 0x06 }
                                }
                    in
                    Expect.equal { pc = addr + 1, fa = 6, fb = -7, ff = 0, fr = 0 }
                        { pc = new_z80.pc |> toInt, fa = new_z80.flags.fa, fb = new_z80.flags.fb, ff = new_z80.flags.ff, fr = new_z80.flags.fr }
            ]
        ]
