module Group0xC0 exposing (..)

import Dict exposing (Dict)
import GroupCB exposing (group_cb, group_xy_cb)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), Z80)


delta_dict_C0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_C0 =
    Dict.fromList
        [ ( 0xCB, execute_0xCB )
        ]



--miniDictC0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
--miniDictC0 =
--    Dict.fromList
--        [
--        --( 0xCB, execute_0xCB )
--        ]


execute_0xCB : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xCB ixiyhl rom48k z80 =
    case ixiyhl of
        IX ->
            z80 |> group_xy_cb IXIY_IX rom48k

        IY ->
            z80 |> group_xy_cb IXIY_IY rom48k

        HL ->
            z80 |> group_cb rom48k
