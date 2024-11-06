module Group0xC0 exposing (..)

import Dict exposing (Dict)
import GroupCB exposing (group_cb, group_xy_cb)
import Z80Delta exposing (Z80Delta(..), rst_delta)
import Z80Env exposing (z80_pop)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), Z80, call_if, imm16)


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


delta_dict_lite_C0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_C0 =
    Dict.fromList
        [ ( 0xC9, ret )
        , ( 0xCD, call_0xCD )
        ]


ret : Z80ROM -> Z80 -> Z80Delta
ret rom48k z80 =
    -- case 0xC9: MP=PC=pop(); break;
    let
        a =
            z80.env |> z80_pop rom48k

        --b = debug_log "ret" (a.value |> subName) Nothing
        --env = z80.env
    in
    --{ z80 | env = { env | time = a.time, sp = a.sp }, pc = a.value }
    CpuTimeWithSpAndPc a.time a.sp a.value


execute_0xCB : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xCB ixiyhl rom48k z80 =
    case ixiyhl of
        IX ->
            z80 |> group_xy_cb IXIY_IX rom48k

        IY ->
            z80 |> group_xy_cb IXIY_IY rom48k

        HL ->
            z80 |> group_cb rom48k


call_0xCD : Z80ROM -> Z80 -> Z80Delta
call_0xCD rom48k z80 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    let
        v =
            z80 |> imm16 rom48k

        --env = z80.env
        --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
        --pushed = { env | time = v.time } |> z80_push v.pc
    in
    --{ z80_1 | env = pushed, pc = v.value }
    PushWithCpuTimeAndPc v.pc v.time v.value
