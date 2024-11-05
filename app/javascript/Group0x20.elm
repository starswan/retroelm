module Group0x20 exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, addCpuTimeTime, increment0)
import Dict exposing (Dict)
import Utils exposing (char, shiftLeftBy8, shiftRightBy8)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (mem16)
import Z80Flags exposing (add16, dec, inc)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_xy_ixiy, imm16, imm8, set_xy, set_xy_ixiy)


miniDict20 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict20 =
    Dict.fromList
        [ ( 0x21, ld_hl_nn )
        , ( 0x23, execute_0x23 )
        , ( 0x24, execute_0x24 )
        , ( 0x25, execute_0x25 )
        , ( 0x26, ld_h_n )
        , ( 0x29, add_hl_hl )
        , ( 0x2B, execute_0x2B )
        , ( 0x2C, execute_0x2C )
        , ( 0x2D, execute_0x2D )
        , ( 0x2E, ld_l_n )
        ]


delta_dict_20 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_20 =
    Dict.fromList
        [ ( 0x2A, execute_0x2A )
        ]


delta_dict_lite_20 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_20 =
    Dict.fromList
        [ ( 0x22, execute_0x22 )
        ]


ld_hl_nn : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_hl_nn ixiyhl rom48k z80 =
    -- case 0x21: HL=imm16(); break;
    -- case 0x21: xy=imm16(); break;
    let
        new_xy =
            z80 |> imm16 rom48k

        --z80_1 = { z80 | env = new_xy.env, pc = new_xy.pc }
        --x = debug_log ("LD " ++ (ixiyhl |> toString) ++ "," ++ (new_xy.value |> toHexString)) ("pc = " ++ (z80.pc |> toHexString)) Nothing
        main =
            z80.main |> set_xy_ixiy new_xy.value ixiyhl
    in
    --{ z80_1 | main = main }
    MainRegsWithPcAndCpuTime main new_xy.pc new_xy.time


-- THis would be TripleWithMain which we don't have (yet)
execute_0x22 : Z80ROM -> Z80 -> Z80Delta
execute_0x22 rom48k z80 =
    -- oops we seem to have forgotten the IX IY version of this
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
    -- case 0x22: MP=(v=imm16())+1; env.mem16(v,xy); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        --new_z80 = { z80 | pc = v.pc }
        --env =
        --    z80.env |> set_mem16 v.value z80.main.hl |> add_cpu_time_env 6
        --x = debug_log "LD nn, HL" ((z80.pc |> toHexString) ++ " addr " ++ (v.value |> toHexString) ++ " " ++ (new_z80.main.hl |> toHexString)) env
    in
    SetMem16WithTimeAndPc v.value z80.main.hl 6 v.pc


execute_0x23 : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x23 ixiyhl rom48k z80 =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    let
        xy =
            z80.main |> get_xy_ixiy ixiyhl

        --x = if z80.pc /= 0x11E7 then
        --        debug_log "INC HL" (z80.pc |> toHexString) Nothing
        --    else
        --        Nothing
        main =
            z80.main |> set_xy_ixiy (char (xy + 1)) ixiyhl
    in
    --{ z80 | main = main } |> add_cpu_time 2
    MainRegsWithPcAndCpuTime main z80.pc (z80.env.time |> addCpuTimeTime 2)


execute_0x24 : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x24 ixiyhl rom48k z80 =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        value =
            inc (shiftRightBy8 xy) z80.flags

        --z80_1 = { z80 | flags = value.flags }
        new_xy =
            Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value.value)

        main =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ z80_1 | main = main }
    FlagsWithPCMainAndTime value.flags z80.pc main increment0


execute_0x25 : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x25 ixiyhl rom48k z80 =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        value =
            dec (shiftRightBy8 xy) z80.flags

        z80_1 =
            { z80 | flags = value.flags }

        new_xy =
            Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value.value)

        main =
            set_xy_ixiy new_xy ixiyhl z80_1.main
    in
    --{ z80_1 | main = main }
    FlagsWithPCMainAndTime value.flags z80.pc main increment0


ld_h_n : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_h_n ixiyhl rom48k z80 =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    let
        value =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        --new_z80 = { z80 | env = value.env, pc = value.pc }
        xy =
            get_xy_ixiy ixiyhl z80.main

        new_xy =
            Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value.value)

        main =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ new_z80 | main = main }
    MainRegsWithPcAndCpuTime main value.pc value.time


add_hl_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
add_hl_hl ixiyhl rom48k z80 =
    -- case 0x29: HL=add16(HL,HL); break;
    -- case 0x29: xy=add16(xy,xy); break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        new_xy =
            add16 xy xy z80.flags

        new_z80 =
            set_xy_ixiy new_xy.value ixiyhl z80.main
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time


execute_0x2A : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x2A ixiyhl rom48k z80 =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    let
        v =
            z80 |> imm16 rom48k

        --z80_1 = { z80 | pc = v.pc }
        new_xy =
            z80.env |> mem16 v.value rom48k

        --z80_2 = { z80_1 | env = new_xy.env }
        main =
            z80.main |> set_xy new_xy.value ixiyhl
    in
    --{ z80_2 | main = main } |> add_cpu_time 6
    MainRegsWithPcAndCpuTime main v.pc (new_xy.time |> addCpuTimeTime 6)


execute_0x2B : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x2B ixiyhl rom48k z80 =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    -- The HL version of this is now in SimpleSingleByte
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        new_xy =
            Bitwise.and (xy - 1) 0xFFFF

        new_z80 =
            set_xy_ixiy new_xy ixiyhl z80.main
    in
    --{ z80 | main = new_z80 } |> add_cpu_time 2
    MainRegsWithPcAndCpuTime new_z80 z80.pc (z80.env.time |> addCpuTimeTime 2)


execute_0x2C : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x2C ixiyhl rom48k z80 =
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


execute_0x2D : IXIY -> Z80ROM -> Z80 -> Z80Delta
execute_0x2D ixiyhl rom48k z80 =
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
