module Group0x00 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (add16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_bc, get_xy_ixiy, set_xy_ixiy)


miniDict00 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict00 =
    Dict.fromList
        [ ( 0x09, add_hl_bc )
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
