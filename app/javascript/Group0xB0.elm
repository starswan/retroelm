module Group0xB0 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (z80_cp, z80_or)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL, Z80, get_h_ixiy, get_l_ixiy, hl_deref_with_z80)


delta_dict_B0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_B0 =
    Dict.fromList
        [ ( 0xB6, execute_0xB6 )
        , ( 0xBE, execute_0xBE )
        ]


miniDictB0 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDictB0 =
    Dict.fromList
        [ ( 0xB4, or_h )
        , ( 0xB5, or_l )
        , ( 0xBC, cp_h )
        , ( 0xBD, cp_l )
        ]


or_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
or_h ixiyhl _ z80 =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_or (get_h_ixiy ixiyhl z80.main) |> FlagRegs


or_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
or_l ixiyhl _ z80 =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_or (get_l_ixiy ixiyhl z80.main) |> FlagRegs


execute_0xB6 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB6 ixiyhl rom48k z80 =
    -- case 0xB6: or(env.mem(HL)); time+=3; break;
    -- case 0xB6: or(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_or value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_or value.value) value.pc value.time


cp_h : IXIY -> Z80ROM -> Z80 -> Z80Delta
cp_h ixiyhl _ z80 =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_cp (get_h_ixiy ixiyhl z80.main) |> FlagRegs


cp_l : IXIY -> Z80ROM -> Z80 -> Z80Delta
cp_l ixiyhl _ z80 =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_cp (get_l_ixiy ixiyhl z80.main) |> FlagRegs


execute_0xBE : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xBE ixiyhl rom48k z80 =
    -- case 0xBE: cp(env.mem(HL)); time+=3; break;
    -- case 0xBE: cp(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 =            z80.env
        flags =
            z80_cp value.value z80.flags
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (cp value.value z80.flags) |> Whole
    FlagsWithPcAndTime flags value.pc value.time
