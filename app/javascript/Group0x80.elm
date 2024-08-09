module Group0x80 exposing (..)

import Dict exposing (Dict)
import Z80Delta exposing (Z80Delta(..))
import Z80Flags exposing (adc, z80_add)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL, Z80, get_h, get_l, hl_deref_with_z80)


delta_dict_80 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_80 =
    Dict.fromList
        [
          (0x84, execute_0x84),
          (0x85, execute_0x85),
          (0x86, execute_0x86),
          (0x8C, execute_0x8C),
          (0x8D, execute_0x8D),
          (0x8E, execute_0x8E)
        ]


delta_dict_lite_80 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_80 =
    Dict.fromList
        [
          (0x80, execute_0x80),
          (0x81, execute_0x81),
          (0x82, execute_0x82),
          (0x83, execute_0x83),
          (0x87, execute_0x87),
          (0x88, execute_0x88),
          (0x89, execute_0x89),
          (0x8A, execute_0x8A),
          (0x8B, execute_0x8B),
          (0x8F, execute_0x8F)
        ]

execute_0x80 : Z80ROM -> Z80 -> Z80Delta
execute_0x80 rom z80 =
    -- case 0x80: add(B); break;
    --z80 |> set_flag_regs (z80_add z80.main.b z80.flags)
    FlagRegs (z80_add z80.main.b z80.flags)


execute_0x81 : Z80ROM -> Z80 -> Z80Delta
execute_0x81 rom z80 =
    -- case 0x81: add(C); break;
    --z80 |> set_flag_regs (z80_add z80.main.c z80.flags)
    FlagRegs (z80_add z80.main.c z80.flags)


execute_0x82 : Z80ROM -> Z80 -> Z80Delta
execute_0x82 rom z80 =
    -- case 0x82: add(D); break;
    --z80 |> set_flag_regs (z80_add z80.main.d z80.flags)
    FlagRegs (z80_add z80.main.d z80.flags)


execute_0x83 : Z80ROM -> Z80 -> Z80Delta
execute_0x83 rom z80 =
    -- case 0x83: add(E); break;
    --z80 |> set_flag_regs (z80_add z80.main.e z80.flags)
    FlagRegs (z80_add z80.main.e z80.flags)


execute_0x84 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x84 ixiyhl rom z80 =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    FlagRegs (z80_add (get_h ixiyhl z80.main) z80.flags)


execute_0x85 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x85 ixiyhl rom z80 =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    FlagRegs (z80_add (get_l ixiyhl z80.main) z80.flags)


execute_0x86 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x86 ixiyhl rom48k z80 =
    -- case 0x86: add(env.mem(HL)); time+=3; break;
    -- case 0x86: add(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
        --z80_1 = { z80 | pc = value.pc, env = { env_1 | time = value.time } }
        flags_one =
            z80_add value.value z80.flags
    in
    FlagsWithPcAndTime flags_one value.pc value.time



--{ z80_1 | flags = flags_one }


execute_0x87 : Z80ROM -> Z80 -> Z80Delta
execute_0x87 rom z80 =
    -- case 0x87: add(A); break;
    z80.flags |> z80_add z80.flags.a |> FlagRegs


execute_0x88 : Z80ROM -> Z80 -> Z80Delta
execute_0x88 rom z80 =
    -- case 0x88: adc(B); break;
    z80.flags |> adc z80.main.b |> FlagRegs


execute_0x89 : Z80ROM -> Z80 -> Z80Delta
execute_0x89 rom z80 =
    -- case 0x89: adc(C); break;
    --z80 |> set_flag_regs (adc z80.main.c z80.flags)
    z80.flags |> adc z80.main.c |> FlagRegs


execute_0x8A : Z80ROM -> Z80 -> Z80Delta
execute_0x8A rom z80 =
    -- case 0x8A: adc(D); break;
    --z80 |> set_flag_regs (adc z80.main.d z80.flags)
    z80.flags |> adc z80.main.d |> FlagRegs


execute_0x8B : Z80ROM -> Z80 -> Z80Delta
execute_0x8B rom z80 =
    -- case 0x8B: adc(E); break;
    --z80 |> set_flag_regs (adc z80.main.e z80.flags)
    z80.flags |> adc z80.main.e |> FlagRegs


execute_0x8C : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x8C ixiyhl rom z80 =
    -- case 0x8C: adc(HL>>>8); break;
    -- case 0x8C: adc(xy>>>8); break;
    --z80 |> set_flag_regs (adc (get_h ixiyhl z80.main) z80.flags)
    z80.flags |> adc (get_h ixiyhl z80.main) |> FlagRegs


execute_0x8D : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x8D ixiyhl rom z80 =
    -- case 0x8D: adc(HL&0xFF); break;
    -- case 0x8D: adc(xy&0xFF); break;
    --z80 |> set_flag_regs (adc (get_l ixiyhl z80.main) z80.flags)
    z80.flags |> adc (get_l ixiyhl z80.main) |> FlagRegs


execute_0x8E : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x8E ixiyhl rom48k z80 =
    -- case 0x8E: adc(env.mem(HL)); time+=3; break;
    -- case 0x8E: adc(env.mem(getd(xy))); time+=3; break;
    let
        value =
            z80 |> hl_deref_with_z80 ixiyhl rom48k

        --env_1 = z80.env
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (adc value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> adc value.value) value.pc value.time


execute_0x8F : Z80ROM -> Z80 -> Z80Delta
execute_0x8F rom z80 =
    -- case 0x8F: adc(A); break;
    z80.flags |> adc z80.flags.a |> FlagRegs
