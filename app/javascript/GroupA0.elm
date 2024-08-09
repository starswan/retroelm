module GroupA0 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (z80_and, z80_xor)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


delta_dict_A0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_A0 =
    Dict.fromList
        [ ( 0xA4, execute_0xA4 )
        , ( 0xA5, execute_0xA5 )
        , ( 0xA6, execute_0xA6 )
        , ( 0xAC, execute_0xAC )
        , ( 0xAD, execute_0xAD )
        , ( 0xAE, execute_0xAE )
        ]


delta_dict_lite_A0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_A0 =
    Dict.fromList
        [ ( 0xA0, execute_0xA0 )
        , ( 0xA1, execute_0xA1 )
        , ( 0xA2, execute_0xA2 )
        , ( 0xA3, execute_0xA3 )
        , ( 0xA7, execute_0xA7 )
        , ( 0xA8, execute_0xA8 )
        , ( 0xA9, execute_0xA9 )
        , ( 0xAA, execute_0xAA )
        , ( 0xAB, execute_0xAB )
        , ( 0xAF, execute_0xAF )
        ]


execute_0xA0 : Z80ROM -> Z80 -> Z80Delta
execute_0xA0 _ z80 =
    -- case 0xA0: and(B); break;
    --z80 |> set_flag_regs (z80_and z80.main.b z80.flags)
    z80.flags |> z80_and z80.main.b |> FlagRegs


execute_0xA1 : Z80ROM -> Z80 -> Z80Delta
execute_0xA1 _ z80 =
    -- case 0xA1: and(C); break;
    --z80 |> set_flag_regs (z80_and z80.main.c z80.flags)
    z80.flags |> z80_and z80.main.c |> FlagRegs


execute_0xA2 : Z80ROM -> Z80 -> Z80Delta
execute_0xA2 _ z80 =
    -- case 0xA2: and(D); break;
    --z80 |> set_flag_regs (z80_and z80.main.d z80.flags)
    z80.flags |> z80_and z80.main.d |> FlagRegs


execute_0xA3 : Z80ROM -> Z80 -> Z80Delta
execute_0xA3 _ z80 =
    -- case 0xA3: and(E); break;
    --z80 |> set_flag_regs (z80_and z80.main.e z80.flags)
    z80.flags |> z80_and z80.main.e |> FlagRegs


execute_0xA4 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xA4 ixiyhl _ z80 =
    -- case 0xA4: and(HL>>>8); break;
    -- case 0xA4: and(xy>>>8); break;
    --z80 |> set_flag_regs (z80_and (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> z80_and (get_h ixiyhl z80.main) |> FlagRegs


execute_0xA5 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xA5 ixiyhl _ z80 =
    -- case 0xA5: and(HL&0xFF); break;
    -- case 0xA5: and(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_and (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> z80_and (get_l ixiyhl z80.main) |> FlagRegs


execute_0xA6 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xA6 ixiyhl rom48k z80 =
    -- case 0xA6: and(env.mem(HL)); time+=3; break;
    -- case 0xA6: and(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_and value.value) value.pc value.time


execute_0xA7 : Z80ROM -> Z80 -> Z80Delta
execute_0xA7 _ z80 =
    -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
    -- and a is correct - I guess the above is a faster implementation
    z80.flags |> z80_and z80.flags.a |> FlagRegs


execute_0xA8 : Z80ROM -> Z80 -> Z80Delta
execute_0xA8 _ z80 =
    -- case 0xA8: xor(B); break;
    z80.flags |> z80_xor z80.main.b |> FlagRegs


execute_0xA9 : Z80ROM -> Z80 -> Z80Delta
execute_0xA9 _ z80 =
    -- case 0xA9: xor(C); break;
    z80.flags |> z80_xor z80.main.c |> FlagRegs


execute_0xAA : Z80ROM -> Z80 -> Z80Delta
execute_0xAA _ z80 =
    -- case 0xAA: xor(D); break;
    z80.flags |> z80_xor z80.main.d |> FlagRegs


execute_0xAB : Z80ROM -> Z80 -> Z80Delta
execute_0xAB _ z80 =
    -- case 0xAB: xor(E); break;
    z80.flags |> z80_xor z80.main.e |> FlagRegs


execute_0xAC : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xAC ixiyhl _ z80 =
    -- case 0xAC: xor(HL>>>8); break;
    -- case 0xAC: xor(xy>>>8); break;
    z80.flags |> z80_xor (get_h ixiyhl z80.main) |> FlagRegs


execute_0xAD : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xAD ixiyhl _ z80 =
    -- case 0xAD: xor(HL&0xFF); break;
    -- case 0xAD: xor(xy&0xFF); break;
    z80.flags |> z80_xor (get_l ixiyhl z80.main) |> FlagRegs


execute_0xAE : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xAE ixiyhl rom48k z80 =
    -- case 0xAE: xor(env.mem(HL)); time+=3; break;
    -- case 0xAE: xor(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k
    in
    FlagsWithPcAndTime (z80.flags |> z80_xor value.value) value.pc value.time


execute_0xAF : Z80ROM -> Z80 -> Z80Delta
execute_0xAF _ z80 =
    -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
    z80.flags |> z80_xor z80.flags.a |> FlagRegs
