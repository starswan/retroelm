module Group0x90 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (sbc, z80_sub)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_h, get_h_ixiy, get_l, get_l_ixiy, hl_deref_with_z80)


delta_dict_90 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_90 =
    Dict.fromList
        [ ( 0x96, sub_indirect_hl ) -- need single with main flags and env
        , ( 0x9E, sbc_indirect_hl ) -- need single with main flags and env
        ]


miniDict90 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict90 =
    Dict.fromList
        [ ( 0x94, sub_h )
        , ( 0x95, sub_l )
        , ( 0x9C, sbc_h )
        , ( 0x9D, sbc_l )
        ]


sub_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
sub_h ixiyhl _ z80 =
    -- case 0x94: sub(HL>>>8); break;
    -- case 0x94: sub(xy>>>8); break;
    --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_sub (get_h_ixiy ixiyhl z80.main) |> FlagRegs


sub_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
sub_l ixiyhl _ z80 =
    -- case 0x95: sub(HL&0xFF); break;
    -- case 0x95: sub(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_sub (get_l_ixiy ixiyhl z80.main) |> FlagRegs


sub_indirect_hl : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
sub_indirect_hl ixiyhl rom48k z80 =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    -- case 0x96: sub(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_sub value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_sub value.value) value.pc value.time


sbc_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
sbc_h ixiyhl _ z80 =
    -- case 0x9C: sbc(HL>>>8); break;
    -- case 0x9C: sbc(xy>>>8); break;
    --z80 |> set_flag_regs (sbc (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> sbc (get_h_ixiy ixiyhl z80.main) |> FlagRegs


sbc_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
sbc_l ixiyhl _ z80 =
    -- case 0x9D: sbc(HL&0xFF); break;
    -- case 0x9D: sbc(xy&0xFF); break;
    --z80 |> set_flag_regs (sbc (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> sbc (get_l_ixiy ixiyhl z80.main) |> FlagRegs


sbc_indirect_hl : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
sbc_indirect_hl ixiyhl rom48k z80 =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    -- case 0x9E: sbc(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (sbc value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> sbc value.value) value.pc value.time


