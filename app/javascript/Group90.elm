module Group90 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (sbc, z80_sub)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


delta_dict_90 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_90 =
    Dict.fromList
        [ ( 0x94, execute_0x94 )
        , ( 0x95, execute_0x95 )
        , ( 0x96, execute_0x96 )
        , ( 0x9C, execute_0x9C )
        , ( 0x9D, execute_0x9D )
        , ( 0x9E, execute_0x9E )
        ]


delta_dict_lite_90 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_90 =
    Dict.fromList
        [ ( 0x90, execute_0x90 )
        , ( 0x91, execute_0x91 )
        , ( 0x92, execute_0x92 )
        , ( 0x93, execute_0x93 )
        , ( 0x97, execute_0x97 )
        , ( 0x98, execute_0x98 )
        , ( 0x99, execute_0x99 )
        , ( 0x9A, execute_0x9A )
        , ( 0x9B, execute_0x9B )
        , ( 0x9F, execute_0x9F )
        ]


execute_0x90 : Z80ROM -> Z80 -> Z80Delta
execute_0x90 _ z80 =
    -- case 0x90: sub(B); break;
    --z80 |> set_flag_regs (z80_sub z80.main.b z80.flags)
    z80.flags |> z80_sub z80.main.b |> FlagRegs


execute_0x91 : Z80ROM -> Z80 -> Z80Delta
execute_0x91 _ z80 =
    -- case 0x91: sub(C); break;
    --z80 |> set_flag_regs (z80_sub z80.main.c z80.flags)
    z80.flags |> z80_sub z80.main.c |> FlagRegs


execute_0x92 : Z80ROM -> Z80 -> Z80Delta
execute_0x92 _ z80 =
    -- case 0x92: sub(D); break;
    --z80 |> set_flag_regs (z80_sub z80.main.d z80.flags)
    z80.flags |> z80_sub z80.main.d |> FlagRegs


execute_0x93 : Z80ROM -> Z80 -> Z80Delta
execute_0x93 _ z80 =
    -- case 0x93: sub(E); break;
    --z80 |> set_flag_regs (z80_sub z80.main.e z80.flags)
    z80.flags |> z80_sub z80.main.e |> FlagRegs


execute_0x94 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x94 ixiyhl _ z80 =
    -- case 0x94: sub(HL>>>8); break;
    -- case 0x94: sub(xy>>>8); break;
    --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_sub (get_h ixiyhl z80.main) |> FlagRegs


execute_0x95 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x95 ixiyhl _ z80 =
    -- case 0x95: sub(HL&0xFF); break;
    -- case 0x95: sub(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_sub (get_l ixiyhl z80.main) |> FlagRegs


execute_0x96 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x96 ixiyhl rom48k z80 =
    -- case 0x96: sub(env.mem(HL)); time+=3; break;
    -- case 0x96: sub(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_sub value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_sub value.value) value.pc value.time


execute_0x97 : Z80ROM -> Z80 -> Z80Delta
execute_0x97 _ z80 =
    -- case 0x97: sub(A); break;
    --z80 |> set_flag_regs (z80_sub z80.flags.a z80.flags)
    z80.flags |> z80_sub z80.flags.a |> FlagRegs


execute_0x98 : Z80ROM -> Z80 -> Z80Delta
execute_0x98 _ z80 =
    -- case 0x98: sbc(B); break;
    --z80 |> set_flag_regs (sbc z80.main.b z80.flags)
    z80.flags |> sbc z80.main.b |> FlagRegs


execute_0x99 : Z80ROM -> Z80 -> Z80Delta
execute_0x99 _ z80 =
    -- case 0x99: sbc(C); break;
    --z80 |> set_flag_regs (sbc z80.main.c z80.flags)
    z80.flags |> sbc z80.main.c |> FlagRegs


execute_0x9A : Z80ROM -> Z80 -> Z80Delta
execute_0x9A _ z80 =
    -- case 0x9A: sbc(D); break;
    --z80 |> set_flag_regs (sbc z80.main.d z80.flags)
    z80.flags |> sbc z80.main.d |> FlagRegs


execute_0x9B : Z80ROM -> Z80 -> Z80Delta
execute_0x9B _ z80 =
    -- case 0x9B: sbc(E); break;
    --z80 |> set_flag_regs (sbc z80.main.e z80.flags)
    z80.flags |> sbc z80.main.e |> FlagRegs


execute_0x9C : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x9C ixiyhl _ z80 =
    -- case 0x9C: sbc(HL>>>8); break;
    -- case 0x9C: sbc(xy>>>8); break;
    --z80 |> set_flag_regs (sbc (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> sbc (get_h ixiyhl z80.main) |> FlagRegs


execute_0x9D : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x9D ixiyhl _ z80 =
    -- case 0x9D: sbc(HL&0xFF); break;
    -- case 0x9D: sbc(xy&0xFF); break;
    --z80 |> set_flag_regs (sbc (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> sbc (get_l ixiyhl z80.main) |> FlagRegs


execute_0x9E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x9E ixiyhl rom48k z80 =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    -- case 0x9E: sbc(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (sbc value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> sbc value.value) value.pc value.time


execute_0x9F : Z80ROM -> Z80 -> Z80Delta
execute_0x9F _ z80 =
    -- case 0x9F: sbc(A); break;
    --z80 |> set_flag_regs (sbc z80.flags.a z80.flags)
    z80.flags |> sbc z80.flags.a |> FlagRegs
