module Group0x10 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (add16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_de, get_xy_ixiy, set_xy_ixiy)


miniDict10 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict10 =
    Dict.fromList
        [ ( 0x19, add_hl_de )
        ]


add_hl_de : IXIY -> Z80ROM -> Z80 -> Z80Delta
add_hl_de ixiyhl _ z80 =
    -- case 0x19: HL=add16(HL,D<<8|E); break;
    -- case 0x19: xy=add16(xy,D<<8|E); break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        new_xy =
            add16 xy (get_de z80.main) z80.flags

        new_z80 =
            set_xy_ixiy new_xy.value ixiyhl z80.main
    in
    --{ z80 | main = new_z80, flags = new_xy.flags} |> add_cpu_time new_xy.time
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time
