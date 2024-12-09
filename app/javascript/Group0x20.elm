module Group0x20 exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, addCpuTimeTime, increment0)
import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (dec, inc)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_xy_ixiy, imm8, set_xy_ixiy)


miniDict20 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict20 =
    Dict.fromList
        [ ( 0x2C, inc_l )
        , ( 0x2D, dec_l )
        , ( 0x2E, ld_l_n )
        ]


inc_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
inc_l ixiyhl rom48k z80 =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    -- The HL version of this is now in SimpleSingleByte
    let
        z80_flags =
            z80.flags

        xy =
            get_xy_ixiy ixiyhl z80.main

        h =
            Bitwise.and xy 0xFF00

        l =
            inc (Bitwise.and xy 0xFF) z80_flags

        --z80_1 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value

        main =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ z80_1 | main = main }
    FlagsWithPCMainAndTime l.flags z80.pc main increment0


dec_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
dec_l ixiyhl rom48k z80 =
    -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
    -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
    -- The HL version of this is now in SimpleSingleByte
    let
        z80_flags =
            z80.flags

        xy =
            get_xy_ixiy ixiyhl z80.main

        h =
            Bitwise.and xy 0xFF00

        l =
            dec (Bitwise.and xy 0xFF) z80_flags

        --new_z80 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value

        main =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ new_z80 | main = main }
    FlagsWithPCMainAndTime l.flags z80.pc main increment0


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
