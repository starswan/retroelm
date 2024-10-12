module Group0x10 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (shiftLeftBy8)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (mem)
import Z80Flags exposing (add16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_de, get_xy_ixiy, set_xy_ixiy)


miniDict10 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict10 =
    Dict.fromList
        [ ( 0x19, add_hl_de )
        ]


delta_dict_lite_10 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_10 =
    Dict.fromList
        [ ( 0x1A, execute_0x1A )
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


execute_0x1A : Z80ROM -> Z80 -> Z80Delta
execute_0x1A rom48k z80 =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        z80_main =
            z80.main

        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e

        new_a =
            mem addr z80.env.time rom48k z80.env.ram

        main_flags =
            z80.flags

        new_flags =
            { main_flags | a = new_a.value }

        env_1 =
            new_a.time |> addCpuTimeTime 3
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    CpuTimeWithFlags env_1 new_flags
