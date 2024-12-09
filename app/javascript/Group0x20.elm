module Group0x20 exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc)
import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_xy_ixiy, imm8, set_xy_ixiy)


miniDict20 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict20 =
    Dict.fromList
        [ ( 0x2E, ld_l_n )
        ]


ld_l_n : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_l_n ixiyhl rom48k z80 =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        h =
            Bitwise.and xy 0xFF00

        l =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        --new_z80 = { z80 | env = l.env, pc = l.pc }
        new_xy =
            Bitwise.or h l.value

        main =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ new_z80 | main = main }
    MainRegsWithPcAndCpuTime main l.pc l.time
