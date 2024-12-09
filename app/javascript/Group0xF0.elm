module Group0xF0 exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Group0x30 exposing (delta_dict_30, miniDict30)
import Group0x40 exposing (miniDict40)
import Group0x50 exposing (miniDict50)
import Group0x60 exposing (miniDict60)
import Group0x70 exposing (miniDict70)
import Group0x80 exposing (delta_dict_80, miniDict80)
import Group0x90 exposing (delta_dict_90, miniDict90)
import Group0xA0 exposing (delta_dict_A0, miniDictA0)
import Group0xB0 exposing (delta_dict_B0, miniDictB0)
import Group0xC0 exposing (delta_dict_C0)
import Group0xE0 exposing (delta_dict_E0, miniDictE0)
import Z80Delta exposing (Z80Delta(..))
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL, Z80)


miniDictF0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDictF0 =
    Dict.fromList
        [ ( 0xF9, ld_sp_hl )
        ]


ld_sp_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_sp_hl ixiyhl rom48k z80 =
    -- case 0xF9: SP=xy; time+=2; break;
    let
        v =
            case ixiyhl of
                IXIY_IX ->
                    z80.main.ix

                IXIY_IY ->
                    z80.main.iy
    in
    --{ z80 | env = { env | sp = v } |> addCpuTimeEnv 2 }
    SpAndCpuTime v 2


lt40_delta_dict : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict =
    delta_dict_80
        |> Dict.union delta_dict_90
        |> Dict.union delta_dict_A0
        |> Dict.union delta_dict_30
        |> Dict.union delta_dict_B0
        |> Dict.union delta_dict_C0
        |> Dict.union delta_dict_E0


list0255 =
    List.range 0 255


lt40_array : Array (Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta))
lt40_array =
    let
        delta_funcs =
            list0255 |> List.map (\index -> lt40_delta_dict |> Dict.get index)
    in
    delta_funcs |> Array.fromList


xYDict : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
xYDict =
    miniDict40
        |> Dict.union miniDict50
        |> Dict.union miniDict60
        |> Dict.union miniDict70
        |> Dict.union miniDict80
        |> Dict.union miniDict90
        |> Dict.union miniDictA0
        |> Dict.union miniDictB0
        |> Dict.union miniDictE0
        |> Dict.union miniDictF0
        |> Dict.union miniDict30
