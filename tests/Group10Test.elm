module Group10Test exposing (..)

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
        [ describe "16 bit load immediate"
            [ test "0x11 LD DE,nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x11
                                |> setMem (addr + 1) 0x34
                                |> setMem (addr + 2) 0x45

                        new_z80 =
                            execute_instruction z80rom { z80 | env = new_env }
                    in
                    Expect.equal ( addr + 3, 0x45, 0x34 ) ( new_z80.pc |> toInt, new_z80.main.d, new_z80.main.e )
            ]
        , describe "ADD HL, 16-bit"
            [ test "0x19 ADD HL, DE" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x19
                                    , main = { z80main | d = 0x12, e = 0x23, hl = 0x3445 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 1, 0x4668 ) ( z80_after_01.pc |> toInt, z80_after_01.main.hl |> toInt )
            , test "0xDD 0x19 ADD IX, DE" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xDD |> setMem (addr + 1) 0x19
                                    , main = { z80main | ix = 0x05 |> fromInt, d = 0x01, e = 0x02, hl = 0x3445 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( z80_after_01.pc |> toInt, z80_after_01.main.hl |> toInt, z80_after_01.main.ix |> toInt )
            , test "0xFD 0x19 ADD IY, DE" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xFD |> setMem (addr + 1) 0x19
                                    , main = { z80main | iy = 0x05 |> fromInt, d = 0x01, e = 0x02, hl = 0x3445 |> fromInt }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( z80_after_01.pc |> toInt, z80_after_01.main.hl |> toInt, z80_after_01.main.iy |> toInt )
            ]
        , describe "LD A, (16 bit)"
            [ test "0x1A - LD A,(DE)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x1A
                                |> setMem 0x4546 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x45, e = 0x46 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.flags.a )
            ]
        , describe "DEC 16 bit"
            [ test "0x1B DEC DE" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x1B
                                    , main = { z80main | d = 0x45, e = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x44, 0xFF ) ( z80_after_01.pc |> toInt, z80_after_01.main.d, z80_after_01.main.e )
            ]
        , describe "INC 8 bit"
            [ test "INC D 0x14" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x14

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x38 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x66, 0xFF ) ( new_z80.pc |> toInt, new_z80.main.d, new_z80.main.e )
            , test "INC E - 0x1C" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x1C
                                    , main = { z80main | d = 0x45, e = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45, 0x01 ) ( z80_after_01.pc |> toInt, z80_after_01.main.d, z80_after_01.main.e )
            ]
        , describe "DEC 8 bit"
            [ test "DEC E - 0x1D" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x1D
                                    , main = { z80main | d = 0x45, e = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45, 0xFF ) ( z80_after_01.pc |> toInt, z80_after_01.main.d, z80_after_01.main.e )
            ]
        , describe "LD 8-bit,n"
            [ test "LD D,n - 0x16" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x16
                                |> setMem (addr + 1) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x38 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x34, 0xFF ) ( new_z80.pc |> toInt, new_z80.main.d, new_z80.main.e )
            , test "LD E,n - 0x1E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x1E
                                |> setMem (addr + 1) 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x45, e = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x78 ) ( z80_after_01.pc |> toInt, z80_after_01.main.e )
            ]
        , describe "DJNZ - 0x10"
            [ test "Jump" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x10 |> setMem (addr + 1) 0x02
                                    , main = { z80main | b = 0x45 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x44 ) ( z80_after_01.pc |> toInt, z80_after_01.main.b )
            , test "Dont jump" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x10 |> setMem (addr + 1) 0x02
                                    , main = { z80main | b = 0x01 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x00 ) ( z80_after_01.pc |> toInt, z80_after_01.main.b )
            ]
        , describe "LD (DE), A"
            [ test "Do it 0x12" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x12

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0x45 }
                                    , flags = { flags | a = 0x38 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x38 ) ( new_z80.pc |> toInt, mem_value.value )
            ]
        , describe "INC DE"
            [ test "Do it 0x13" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x13

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x38 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x66, 0x00 ) ( new_z80.pc |> toInt, new_z80.main.d, new_z80.main.e )
            ]
        , describe "DEC D"
            [ test "Do it 0x15" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x15

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x38 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x64, 0xFF ) ( new_z80.pc |> toInt, new_z80.main.d, new_z80.main.e )
            ]
        , describe "RLA"
            [ test "Do it 0x17" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x17
                                |> setMem (addr + 1) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x72 ) ( new_z80.pc |> toInt, new_z80.flags.a )
            ]
        , describe "JR n"
            [ test "Do it 0x18" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x18
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal (addr + 7) (new_z80.pc |> toInt)
            ]
        , describe "RRA"
            [ test "Do it 0x1F" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x1F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x65, e = 0xFF }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x1C ) ( new_z80.pc |> toInt, new_z80.flags.a )
            ]
        ]
