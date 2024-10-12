module Group0x00 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (shiftLeftBy8)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (mem)
import Z80Flags exposing (add16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_bc, get_xy_ixiy, imm16, set_bc_main, set_xy_ixiy)


miniDict00 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict00 =
    Dict.fromList
        [ ( 0x09, add_hl_bc )
        ]


delta_dict_lite_00 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_00 =
    Dict.fromList
        [ ( 0x0A, ld_a_indirect_bc )
        ]


add_hl_bc : IXIY -> Z80ROM -> Z80 -> Z80Delta
add_hl_bc ixiyhl _ z80 =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        new_xy =
            add16 xy (get_bc z80.main) z80.flags

        new_z80 =
            set_xy_ixiy new_xy.value ixiyhl z80.main
    in
    --Whole ({ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time)
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time


ld_a_indirect_bc : Z80ROM -> Z80 -> Z80Delta
ld_a_indirect_bc rom48k z80 =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        z80_main =
            z80.main

        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        new_a =
            mem v z80.env.time rom48k z80.env.ram

        new_flags =
            { z80_flags | a = new_a.value }
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags (new_a.time |> addCpuTimeTime 3) new_flags
