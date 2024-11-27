module CB40Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80 exposing (execute_instruction)
import Z80Env exposing (mem, setMem)
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
            { old_z80 | pc = addr, env = { old_z80env | sp = sp }, main = { z80main | hl = hl } }

        flags =
            z80.flags

        z80env =
            z80.env

        z80rom =
            Z80Rom.constructor
    in
    describe "Bit instructions (CB)"
        [ test "0xCB 0x40 BIT 0,B (unset)" <|
            \_ ->
                let
                    new_env =
                        z80env
                            |> setMem addr 0xCB
                            |> setMem (addr + 1) 0x40

                    new_z80 =
                        execute_instruction z80rom
                            { z80
                                | env = { new_env | sp = 0x8765 }
                                , main = { z80main | hl = 0x6545, b = 0x50 }
                                , flags = { flags | a = 0x39 }
                            }
                in
                Expect.equal ( addr + 2, 0x00 ) ( new_z80.pc, new_z80.flags.fr )
        ,test "0xCB 0x40 BIT 0,B (set)" <|
                    \_ ->
                        let
                            new_env =
                                z80env
                                    |> setMem addr 0xCB
                                    |> setMem (addr + 1) 0x40

                            new_z80 =
                                execute_instruction z80rom
                                    { z80
                                        | env = { new_env | sp = 0x8765 }
                                        , main = { z80main | hl = 0x6545, b = 0x51 }
                                        , flags = { flags | a = 0x39 }
                                    }
                        in
                        Expect.equal ( addr + 2, 0x01 ) ( new_z80.pc, new_z80.flags.fr )
        ]
