module Group30Test exposing (..)

import Bitwise exposing (shiftRightBy)
import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, mem16, setMem, setMem16)
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
        [ describe "0x30 JR NC, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x30
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | ff = 0x0100 }
                                }
                    in
                    Expect.equal (addr + 2) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x30
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0xFF }
                                }
                    in
                    Expect.equal (addr + 7) new_z80.pc
            ]
        , test "0x31 - LD SP, nn" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x31
                            |> setMem (addr + 1) 0x05
                            |> setMem (addr + 2) 0x07

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 3, 0x0705 ) ( new_z80.pc, new_z80.env.sp )
        , test "0x32 - LD (nn), A" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x32
                            |> setMem (addr + 1) 0x77
                            |> setMem (addr + 2) 0x55

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }

                    mem_value =
                        mem 0x5577 new_z80.env.time z80rom new_z80.env.ram
                in
                Expect.equal ( addr + 3, 0x39 ) ( new_z80.pc, mem_value.value )
        , test "0x33 INC SP" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x33

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 1, 0x8766 ) ( new_z80.pc, new_z80.env.sp )
        , describe "ADD HL, 16-bit"
            [ test "0x39 ADD HL,SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x39

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x4321 }
                                    , main = { z80main | hl = 0x1234 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x5555 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xFD 0x39 ADD IY,SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x39

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x4321 }
                                    , main = { z80main | iy = 0x1234, hl = 0x4234 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x5555 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "DEC 16 bit"
            [ test "0x3B DEC SP" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8756 }
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x8755 ) ( new_z80.pc, new_z80.env.sp )
            ]
        , describe "INC 8 bit"
            [ test "0x3C INC A" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x3A ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "DEC 8 bit"
            [ test "DEC A - 0x3D" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x38 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "LD 8-bit,n"
            [ test "LD A,n - 0x3E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3E
                                |> setMem (addr + 1) 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | d = 0x45, e = 0x00 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
            ]
        , describe "load reg indirect"
            [ test "0x3A LD A,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3A
                                |> setMem (addr + 1) 0x20
                                |> setMem (addr + 2) 0x70
                                |> setMem 0x7020 0x87

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x4334 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x87 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "16 bit indirect"
            [ test "0x34 INC (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x34
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x79 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x34 INC (IX)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x34
                                |> setMem (addr + 2) 0xFF
                                |> setMem 0x6544 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6545, hl = 0x2545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6544 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x79 ) ( new_z80.pc, mem_value.value )
            , test "0x35 DEC (HL)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x35
                                |> setMem 0x6545 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x77, 119 ) ( new_z80.pc, mem_value.value, new_z80.flags.fr )
            , test "0x35 DEC (HL) going to zero" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x35
                                |> setMem 0x6545 0x01

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 1, 0x00, 0 ) ( new_z80.pc, mem_value.value, new_z80.flags.fr )
            , test "0xFD 0x35 DEC (IY + n)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x35
                                |> setMem (addr + 2) 0x01
                                |> setMem 0x6546 0x78

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | iy = 0x6545, hl = 0x2545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6546 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 3, 0x77, 0x2545 ) ( new_z80.pc, mem_value.value, new_z80.main.hl )
            ]
        , describe "Indirect indexed load"
            [ test "0x36 LD (HL),n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x36
                                |> setMem (addr + 1) 0xA5

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 2, 0xA5 ) ( new_z80.pc, mem_value.value )
            , test "0xDD 0x36 LD (IX + m),n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x36
                                |> setMem (addr + 2) 0x00
                                |> setMem (addr + 3) 0xA5

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | ix = 0x6545, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            mem 0x6545 new_z80.env.time z80rom new_z80.env.ram
                    in
                    Expect.equal ( addr + 4, 0xA5 ) ( new_z80.pc, mem_value.value )
            ]
        , describe "Flags"
            [ test "0x37 SCF" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x37

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | ff = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x0100 ) ( new_z80.pc, Bitwise.and new_z80.flags.ff 0x0100 )
            , test "0x3F CCF" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x3F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = { new_env | sp = 0x8765 }
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | ff = 0x0100 }
                                }
                    in
                    Expect.equal ( addr + 1, 0 ) ( new_z80.pc, Bitwise.and new_z80.flags.ff 0x0100 )
            ]
        , describe "0x38 JR C, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x38
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | ff = 0xFF }
                                }
                    in
                    Expect.equal (addr + 2) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x38
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, ff = 0x0100 }
                                }
                    in
                    Expect.equal (addr + 7) new_z80.pc
            ]
        ]
