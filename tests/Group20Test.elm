module Group20Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem16, setMem, setMem16)
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
        [ describe "0x20 - JR NZ,n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x20
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal (addr + 2) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x20
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal (addr + 7) new_z80.pc
            , test "Jump backwards" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x20
                                |> setMem (addr + 1) 0xFB

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 1 }
                                }
                    in
                    Expect.equal (addr - 3) new_z80.pc
            ]
        , describe "16 bit load immediate"
            [ test "0x21 - LD HL, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x21
                                |> setMem (addr + 1) 0xC6
                                |> setMem (addr + 2) 0x15

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x15C6 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xFD 0x21 - LD IX, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x21
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x07

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x0705 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x21 - LD IY, nn" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x21
                                |> setMem (addr + 2) 0x05
                                |> setMem (addr + 3) 0x07

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x0705 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "16 bit store"
            [
            test "0x22 LD (nn), HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x22
                                |> setMem (addr + 1) 0x77
                                |> setMem (addr + 2) 0x55

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x5D9F }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem16 0x5577 z80rom
                    in
                    ( new_z80.pc, mem_value.value ) |> Expect.equal ( addr + 3, 0x5D9F )
            ,test "0xDD 22 LD (nn), IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x22
                                |> setMem (addr + 2) 0x77
                                |> setMem (addr + 3) 0x55

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x5D9F }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem16 0x5577 z80rom
                    in
                    Expect.equal ( addr + 4, 0x5D9F ) ( new_z80.pc, mem_value.value )
            ,test "0xFD 22 LD (nn), IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x22
                                |> setMem (addr + 2) 0x77
                                |> setMem (addr + 3) 0x55

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x5D9F }
                                    , flags = { flags | a = 0x39 }
                                }

                        mem_value =
                            new_z80.env |> mem16 0x5577 z80rom
                    in
                    Expect.equal ( addr + 4, 0x5D9F ) ( new_z80.pc, mem_value.value )
            ]
        , describe "16 bit Increment"
            [ test "0x23 INC HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x23

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6546 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x23 INC IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x23

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0xFFFF, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6545, 0 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x23 INC IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x23

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0xFFFE, hl = 0x6545 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6545, 0xFFFF ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , test "0x24 INC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x24

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0x6545, hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 1, 0x6645, 0x6545 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
        , test "0xDD 0x24 INC IXH" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0x24

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0x6545, hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x6545, 0x6645 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
        , test "0xFD 0x24 INC IYH" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0x24

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0x6545, hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x6545, 0x6645 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
        , test "0x25 DEC H" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x25

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 1, 0x6445 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xDD 0x25 DEC IXH" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0x25

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0x45, hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x6545, 0xFF45 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
        , test "0xFD 0x25 DEC IYH" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0x25

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0x45, hl = 0x6545 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x6545, 0xFF45 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
        , test "0x26 - LD H,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x26
                            |> setMem (addr + 1) 0x05

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | hl = 0x6545 }
                            }
                in
                Expect.equal ( addr + 2, 0x0545 ) ( new_z80.pc, new_z80.main.hl )
        , test "0xDD 0x26 - LD IXH,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xDD
                            |> setMem (addr + 1) 0x26
                            |> setMem (addr + 2) 0x05

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | ix = 0x6545, hl = 0x6545 }
                            }
                in
                Expect.equal ( addr + 3, 0x6545, 0x0545 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
        , test "0xFD 0x26 - LD IYH,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xFD
                            |> setMem (addr + 1) 0x26
                            |> setMem (addr + 2) 0x05

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = new_env
                                , main = { z80main | iy = 0x6545, hl = 0x6545 }
                            }
                in
                Expect.equal ( addr + 3, 0x6545, 0x0545 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
        , describe "0x27 - DAA"
            [ test "No idea how to do this..." <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x27
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x39 ) ( new_z80.pc, new_z80.flags.a )
            ]
        , describe "0x28 JR Z, n"
            [ test "Dont jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x28
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | fr = 1 }
                                }
                    in
                    Expect.equal (addr + 2) new_z80.pc
            , test "Jump" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x28
                                |> setMem (addr + 1) 0x05

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x39, fr = 0 }
                                }
                    in
                    Expect.equal (addr + 7) new_z80.pc
            ]
        , describe "ADD HL, 16-bit"
            [ test "0x29 ADD HL,HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x29

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x4334 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x8668 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x29 ADD IX, IX" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xDD |> setMem (addr + 1) 0x29
                                    , main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0A ) ( z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.ix )
            , test "0xFD 0x29 ADD IY, IY" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xFD |> setMem (addr + 1) 0x29
                                    , main = { z80main | iy = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0A ) ( z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.iy )
            ]
        , describe "load reg indirect"
            [ test "0x2A LD HL,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2A
                                |> setMem (addr + 1) 0x34
                                |> setMem (addr + 2) 0x54
                                |> setMem16 0x5434 0x8723

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x4334 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x8723 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x2A LD IX,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x2A
                                |> setMem (addr + 2) 0x34
                                |> setMem (addr + 3) 0x54
                                |> setMem16 0x5434 0x8723

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x4334, hl = 0x4334 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x8723 ) ( new_z80.pc, new_z80.main.ix )
            , test "0xFD 0x2A LD IY,(nn)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x2A
                                |> setMem (addr + 2) 0x34
                                |> setMem (addr + 3) 0x54
                                |> setMem16 0x5434 0x8723

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x4334, hl = 0x4334 }
                                }
                    in
                    Expect.equal ( addr + 4, 0x8723 ) ( new_z80.pc, new_z80.main.iy )
            ]
        , describe "DEC 16 bit"
            [ test "0x2B DEC HL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x64FF ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x2B DEC IX" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x2B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6500, 0xFFFF ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x2B DEC IY" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x2B

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6500, 0xFFFF ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
            ]
        , describe "INC 8 bit"
            [ test "0x2C INC L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x6501 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x2C INC IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x2C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6500, 0x6501 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.ix )
            , test "0xFD 0x2C INC IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x2C

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6500, 0x6501 ) ( new_z80.pc, new_z80.main.hl, new_z80.main.iy )
            , test "0x2D DEC L" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x65FF ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x2D DEC IXL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x2D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x65FF, 0x6500 ) ( new_z80.pc, new_z80.main.ix, new_z80.main.hl )
            , test "0xFD 0x2D DEC IYL" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x2D

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x65FF, 0x6500 ) ( new_z80.pc, new_z80.main.iy, new_z80.main.hl )
            ]
        , describe "LD 8-bit,n"
            [ test "0x2E LD L,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2E
                                |> setMem (addr + 1) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x6534 ) ( new_z80.pc, new_z80.main.hl )
            , test "0xDD 0x2E LD IXL,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xDD
                                |> setMem (addr + 1) 0x2E
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | ix = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x6534, 0x6500 ) ( new_z80.pc, new_z80.main.ix, new_z80.main.hl )
            , test "0xFD 0x2E LD IYL,n" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0xFD
                                |> setMem (addr + 1) 0x2E
                                |> setMem (addr + 2) 0x34

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | iy = 0x6500, hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 3, 0x6534, 0x6500 ) ( new_z80.pc, new_z80.main.iy, new_z80.main.hl )
            ]
        , describe "0x2F CPL"
            [ test "Do it" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x2F

                        new_z80 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | hl = 0x6500 }
                                    , flags = { flags | a = 0x39 }
                                }
                    in
                    Expect.equal ( addr + 1, 0xC6 ) ( new_z80.pc, new_z80.flags.a )
            ]
        ]
