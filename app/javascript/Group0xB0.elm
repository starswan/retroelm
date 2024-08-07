module Group0xB0 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (cp, z80_or)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80, set_flag_regs)


delta_dict_B0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_B0 =
    Dict.fromList
        [ ( 0xB4, execute_0xB4 )
        , ( 0xB5, execute_0xB5 )
        , ( 0xB6, execute_0xB6 )
        , ( 0xBC, execute_0xBC )
        , ( 0xBD, execute_0xBD )
        ]


delta_dict_lite_B0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_B0 =
    Dict.fromList
        [ ( 0xB0, execute_0xB0 )
        , ( 0xB1, execute_0xB1 )
        , ( 0xB2, execute_0xB2 )
        , ( 0xB3, execute_0xB3 )
        , ( 0xB7, execute_0xB7 )
        , ( 0xB8, execute_0xB8 )
        , ( 0xB9, execute_0xB9 )
        , ( 0xBA, execute_0xBA )
        , ( 0xBB, execute_0xBB )
        , ( 0xBF, execute_0xBF )
        ]


execute_0xB0 : Z80ROM -> Z80 -> Z80Delta
execute_0xB0 _ z80 =
    -- case 0xB0: or(B); break;
    --z80 |> set_flag_regs (z80_or z80.main.b z80.flags)
    z80.flags |> z80_or z80.main.b |> FlagRegs


execute_0xB1 : Z80ROM -> Z80 -> Z80Delta
execute_0xB1 _ z80 =
    -- case 0xB1: or(C); break;
    z80.flags |> z80_or z80.main.c |> FlagRegs


execute_0xB2 : Z80ROM -> Z80 -> Z80Delta
execute_0xB2 _ z80 =
    -- case 0xB2: or(D); break;
    z80.flags |> z80_or z80.main.d |> FlagRegs


execute_0xB3 : Z80ROM -> Z80 -> Z80Delta
execute_0xB3 _ z80 =
    -- case 0xB3: or(E); break;
    --z80 |> set_flag_regs (z80_or z80.main.e z80.flags)
    z80.flags |> z80_or z80.main.e |> FlagRegs


execute_0xB4 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB4 ixiyhl _ z80 =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_or (get_h ixiyhl z80.main) |> FlagRegs


execute_0xB5 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB5 ixiyhl _ z80 =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_or (get_l ixiyhl z80.main) |> FlagRegs


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


execute_0xB7 : Z80ROM -> Z80 -> Z80Delta
execute_0xB7 _ z80 =
    -- case 0xB7: or(A); break;
    --z80 |> set_flag_regs (z80_or z80.flags.a z80.flags)
    z80.flags |> z80_or z80.flags.a |> FlagRegs


execute_0xB8 : Z80ROM -> Z80 -> Z80Delta
execute_0xB8 _ z80 =
    -- case 0xB8: cp(B); break;
    --z80 |> set_flag_regs (cp z80.main.b z80.flags)
    z80.flags |> cp z80.main.b |> FlagRegs


execute_0xB9 : Z80ROM -> Z80 -> Z80Delta
execute_0xB9 _ z80 =
    -- case 0xB9: cp(C); break;
    --z80 |> set_flag_regs (cp z80.main.c z80.flags)
    z80.flags |> cp z80.main.c |> FlagRegs


execute_0xBA : Z80ROM -> Z80 -> Z80Delta
execute_0xBA _ z80 =
    -- case 0xBA: cp(D); break;
    --z80 |> set_flag_regs (cp z80.main.d z80.flags)
    z80.flags |> cp z80.main.d |> FlagRegs


execute_0xBB : Z80ROM -> Z80 -> Z80Delta
execute_0xBB _ z80 =
    -- case 0xBB: cp(E); break;
    --z80 |> set_flag_regs (cp z80.main.e z80.flags)
    z80.flags |> cp z80.main.e |> FlagRegs


execute_0xBC : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xBC ixiyhl _ z80 =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> cp (get_h ixiyhl z80.main) |> FlagRegs


execute_0xBD : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xBD ixiyhl _ z80 =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> cp (get_l ixiyhl z80.main) |> FlagRegs


execute_0xBE : IXIYHL -> Z80ROM -> Z80 -> Z80
execute_0xBE ixiyhl rom48k z80 =
    -- case 0xBE: cp(env.mem(HL)); time+=3; break;
    -- case 0xBE: cp(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        env_1 =
            z80.env
    in
    { z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (cp value.value z80.flags)


execute_0xBF : Z80ROM -> Z80 -> Z80Delta
execute_0xBF _ z80 =
    -- case 0xBF: cp(A); break;
    --z80 |> set_flag_regs (cp z80.flags.a z80.flags)
    z80.flags |> cp z80.flags.a |> FlagRegs
