module Group00Test exposing (..)

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
        [ test "0x00" <|
            \_ ->
                let
                    z80inc =
                        { z80 | env = z80env |> setMem addr 0x00 } |> Z80.execute_instruction z80rom
                in
                Expect.equal ( addr + 1, 4 ) ( z80inc.pc, z80inc.env.time.cpu_time - z80.env.time.cpu_time )
        , test "0x01 LD BC,nn" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x01
                            |> setMem (addr + 1) 0x34
                            |> setMem (addr + 2) 0x45

                    z80_after_01 =
                        { z80 | env = new_env } |> Z80.execute_instruction z80rom
                in
                Expect.equal ( addr + 3, 0x45, 0x34 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "0x02 LD (BC), A" <|
            \_ ->
                let
                    z80inc =
                        { z80
                            | env = z80env |> setMem addr 0x02
                            , main = { z80main | b = 0x45, c = 0x34 }
                            , flags = { flags | a = 0x27 }
                        }

                    z80_after_01 =
                        z80inc |> Z80.execute_instruction z80rom

                    mem_value =
                        mem 0x4534 z80_after_01.env.time z80rom z80_after_01.env.ram
                in
                Expect.equal ( addr + 1, 0x27 ) ( z80_after_01.pc, mem_value.value )
        , test "0x03 INC BC" <|
            \_ ->
                let
                    z80_after_01 =
                        execute_instruction z80rom
                            { z80
                                | env = z80env |> setMem addr 0x03
                                , main = { z80main | b = 0x45, c = 0xFF }
                            }
                in
                Expect.equal ( addr + 1, 0x46, 0x00 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
        , test "0x04 INC B" <|
            \_ ->
                let
                    z80_after_01 =
                        execute_instruction z80rom
                            { z80
                                | env = z80env |> setMem addr 0x04
                                , main = { z80main | b = 0x45 }
                            }
                in
                Expect.equal ( addr + 1, 0x46 ) ( z80_after_01.pc, z80_after_01.main.b )
        , test "0x05 DEC B" <|
            \_ ->
                let
                    z80_after_01 =
                        execute_instruction z80rom
                            { z80
                                | env = z80env |> setMem addr 0x05
                                , main = { z80main | b = 0x45 }
                            }
                in
                Expect.equal ( addr + 1, 0x44 ) ( z80_after_01.pc, z80_after_01.main.b )
        , test "0x06 LD B,n" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0x06
                            |> setMem (addr + 1) 0x78

                    z80_after_01 =
                        execute_instruction z80rom { z80 | env = new_env }
                in
                Expect.equal ( addr + 2, 0x78 ) ( z80_after_01.pc, z80_after_01.main.b )
        , describe "RLCA"
            [ test "0x07" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x07
                                |> setMem (addr + 1) 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , flags = { flags | a = 0x87 }
                                }
                    in
                    -- This is RLCA - bit 7 goes into bit 0 and carry flag
                    Expect.equal ( addr + 1, 0x0F ) ( z80_after_01.pc, z80_after_01.flags.a )
            ]
        , describe "EX AF,AF'"
            [ test "0x08" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x08
                                    , flags = { flags | a = 0x87 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x87, z80.alt_flags ) ( z80_after_01.pc, z80_after_01.alt_flags.a, z80_after_01.flags )
            ]
        , describe "ADD HL, 16-bit"
            [ test "0x09 ADD HL, BC" <|
                \_ ->
                    let
                        z80_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x09
                                    , main = { z80main | ix = 0x27, b = 0x01, c = 0x02, hl = 0x0304 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x0406, 0x27 ) ( z80_01.pc, z80_01.main.hl, z80_01.main.ix )
            , test "0xDD 0x09 ADD IX, BC" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0xDD |> setMem (addr + 1) 0x09
                                    , main = { z80main | ix = 0x05, b = 0x01, c = 0x02, hl = 0x3445 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x3445, 0x0107 ) ( z80_after_01.pc, z80_after_01.main.hl, z80_after_01.main.ix )
            ]
        , describe "LD A, (16 bit)"
            [ test "0x0A - LD A,(BC)" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x0A
                                |> setMem 0x4546 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x46 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x78 ) ( z80_after_01.pc, z80_after_01.flags.a )
            ]
        , describe "DEC 16 bit"
            [ test "0x0B DEC BC" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x0B
                                    , main = { z80main | b = 0x45, c = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x44, 0xFF ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
            ]
        , describe "INC 8 bit"
            [ test "INC C - 0x0C" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x0C
                                    , main = { z80main | b = 0x45, c = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45, 0x01 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
            ]
        , describe "DEC 8 bit"
            [ test "DEC C - 0x0D" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x0D
                                    , main = { z80main | b = 0x45, c = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x45, 0xFF ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
            ]
        , describe "LD 8-bit,n"
            [ test "LD C,n - 0x0E" <|
                \_ ->
                    let
                        new_env =
                            z80env
                                |> setMem addr 0x0E
                                |> setMem (addr + 1) 0x78

                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = new_env
                                    , main = { z80main | b = 0x45, c = 0x00 }
                                }
                    in
                    Expect.equal ( addr + 2, 0x45, 0x78 ) ( z80_after_01.pc, z80_after_01.main.b, z80_after_01.main.c )
            ]
        , describe "RRCA"
            [ test "RRCA - 0x0F" <|
                \_ ->
                    let
                        z80_after_01 =
                            execute_instruction z80rom
                                { z80
                                    | env = z80env |> setMem addr 0x0F
                                    , flags = { flags | a = 0x80 }
                                }
                    in
                    Expect.equal ( addr + 1, 0x40 ) ( z80_after_01.pc, z80_after_01.flags.a )
            ]
        ]
