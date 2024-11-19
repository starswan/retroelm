module SingleMainWithFlags exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..), increment0, increment3)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80Flags exposing (FlagRegisters, adc, add16, dec, inc, sbc, shifter0, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Types exposing (MainWithIndexRegisters, get_bc, get_de)


singleByteMainAndFlagRegisters : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, PCIncrement )
singleByteMainAndFlagRegisters =
    Dict.fromList
        [ ( 0x02, ( ld_indirect_bc_a, IncrementByOne ) )
        , ( 0x04, ( inc_b, IncrementByOne ) )
        , ( 0x05, ( dec_b, IncrementByOne ) )
        , ( 0x09, ( add_hl_bc, IncrementByOne ) )
        , ( 0x0C, ( inc_c, IncrementByOne ) )
        , ( 0x0D, ( dec_c, IncrementByOne ) )
        , ( 0x12, ( ld_indirect_de_a, IncrementByOne ) )
        , ( 0x14, ( inc_d, IncrementByOne ) )
        , ( 0x15, ( dec_d, IncrementByOne ) )
        , ( 0x19, ( add_hl_de, IncrementByOne ) )
        , ( 0x1C, ( inc_e, IncrementByOne ) )
        , ( 0x1D, ( dec_e, IncrementByOne ) )
        , ( 0x24, ( inc_h, IncrementByOne ) )
        , ( 0x25, ( dec_h, IncrementByOne ) )
        , ( 0x29, ( add_hl_hl, IncrementByOne ) )
        , ( 0x2C, ( inc_l, IncrementByOne ) )
        , ( 0x2D, ( dec_l, IncrementByOne ) )
        , ( 0x77, ( ld_indirect_hl_a, IncrementByOne ) )
        , ( 0x80, ( add_a_b, IncrementByOne ) )
        , ( 0x81, ( add_a_c, IncrementByOne ) )
        , ( 0x82, ( add_a_d, IncrementByOne ) )
        , ( 0x83, ( add_a_e, IncrementByOne ) )
        , ( 0x84, ( add_a_h, IncrementByOne ) )
        , ( 0x85, ( add_a_l, IncrementByOne ) )
        , ( 0x88, ( adc_a_b, IncrementByOne ) )
        , ( 0x89, ( adc_a_c, IncrementByOne ) )
        , ( 0x8A, ( adc_a_d, IncrementByOne ) )
        , ( 0x8B, ( adc_a_e, IncrementByOne ) )
        , ( 0x8C, ( adc_a_h, IncrementByOne ) )
        , ( 0x8D, ( adc_a_l, IncrementByOne ) )
        , ( 0x90, ( sub_b, IncrementByOne ) )
        , ( 0x91, ( sub_c, IncrementByOne ) )
        , ( 0x92, ( sub_d, IncrementByOne ) )
        , ( 0x93, ( sub_e, IncrementByOne ) )
        , ( 0x94, ( sub_h, IncrementByOne ) )
        , ( 0x95, ( sub_l, IncrementByOne ) )
        , ( 0x98, ( sbc_b, IncrementByOne ) )
        , ( 0x99, ( sbc_c, IncrementByOne ) )
        , ( 0x9A, ( sbc_d, IncrementByOne ) )
        , ( 0x9B, ( sbc_e, IncrementByOne ) )
        , ( 0x9C, ( sbc_h, IncrementByOne ) )
        , ( 0x9D, ( sbc_l, IncrementByOne ) )
        , ( 0xA0, ( and_b, IncrementByOne ) )
        , ( 0xA1, ( and_c, IncrementByOne ) )
        , ( 0xA2, ( and_d, IncrementByOne ) )
        , ( 0xA3, ( and_e, IncrementByOne ) )
        , ( 0xA4, ( and_h, IncrementByOne ) )
        , ( 0xA5, ( and_l, IncrementByOne ) )
        , ( 0xA8, ( xor_b, IncrementByOne ) )
        , ( 0xA9, ( xor_c, IncrementByOne ) )
        , ( 0xAA, ( xor_d, IncrementByOne ) )
        , ( 0xAB, ( xor_e, IncrementByOne ) )
        , ( 0xAC, ( xor_h, IncrementByOne ) )
        , ( 0xAD, ( xor_l, IncrementByOne ) )
        , ( 0xB0, ( or_b, IncrementByOne ) )
        , ( 0xB1, ( or_c, IncrementByOne ) )
        , ( 0xB2, ( or_d, IncrementByOne ) )
        , ( 0xB3, ( or_e, IncrementByOne ) )
        , ( 0xB4, ( or_h, IncrementByOne ) )
        , ( 0xB5, ( or_l, IncrementByOne ) )
        , ( 0xB8, ( cp_b, IncrementByOne ) )
        , ( 0xB9, ( cp_c, IncrementByOne ) )
        , ( 0xBA, ( cp_d, IncrementByOne ) )
        , ( 0xBB, ( cp_e, IncrementByOne ) )
        , ( 0xBC, ( cp_h, IncrementByOne ) )
        , ( 0xBD, ( cp_l, IncrementByOne ) )
        , ( 0xCB00, ( rlc_b, IncrementByTwo ) )
        , ( 0xCB01, ( rlc_c, IncrementByTwo ) )
        , ( 0xCB02, ( rlc_d, IncrementByTwo ) )
        , ( 0xCB03, ( rlc_e, IncrementByTwo ) )
        , ( 0xCB04, ( rlc_h, IncrementByTwo ) )
        , ( 0xCB05, ( rlc_l, IncrementByTwo ) )
        ]


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithBRegister new_b.flags new_b.value


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    FlagsWithBRegister new_b.flags new_b.value


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithCRegister new_c.flags new_c.value


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    FlagsWithCRegister new_c.flags new_c.value


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80_main.d z80_flags
    in
    FlagsWithDRegister new_d.flags new_d.value


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80_main.d z80_flags
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    FlagsWithDRegister new_d.flags new_d.value


inc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_e z80_main z80_flags =
    -- case 0x1C: E=inc(E); break;
    let
        new_e =
            inc z80_main.e z80_flags
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    FlagsWithERegister new_e.flags new_e.value


dec_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_e z80_main z80_flags =
    -- case 0x1D: E=dec(E); break;
    let
        new_e =
            dec z80_main.e z80_flags
    in
    FlagsWithERegister new_e.flags new_e.value


inc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_h z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithHLRegister value.flags new_xy increment0


dec_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_h z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (shiftRightBy8 z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.hl 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithHLRegister value.flags new_xy increment0


inc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            inc (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    HLRegister new_xy


dec_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_l z80_main z80_flags =
    -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
    -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.hl 0xFF00

        l =
            dec (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    HLRegister new_xy


add_hl_hl : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_hl_hl z80_main z80_flags =
    -- case 0x29: HL=add16(HL,HL); break;
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 z80_main.hl z80_main.hl z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


add_hl_bc : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_hl_bc z80_main z80_flags =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            z80_main.hl

        new_xy =
            add16 xy (get_bc z80_main) z80_flags
    in
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


add_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_b z80_main z80_flags =
    -- case 0x80: add(B); break;
    --z80 |> set_flag_regs (z80_add z80.main.b z80.flags)
    Z80ChangeFlags (z80_add z80_main.b z80_flags)


add_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_c z80_main z80_flags =
    -- case 0x81: add(C); break;
    --z80 |> set_flag_regs (z80_add z80.main.c z80.flags)
    Z80ChangeFlags (z80_add z80_main.c z80_flags)


add_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_d z80_main z80_flags =
    -- case 0x82: add(D); break;
    --z80 |> set_flag_regs (z80_add z80.main.d z80.flags)
    Z80ChangeFlags (z80_add z80_main.d z80_flags)


add_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_e z80_main z80_flags =
    -- case 0x83: add(E); break;
    --z80 |> set_flag_regs (z80_add z80.main.e z80.flags)
    Z80ChangeFlags (z80_add z80_main.e z80_flags)


add_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_add (shiftRightBy8 z80_main.hl) z80_flags)


add_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_add (Bitwise.and z80_main.hl 0xFF) z80_flags)


adc_a_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_b z80_main z80_flags =
    -- case 0x88: adc(B); break;
    Z80ChangeFlags (z80_flags |> adc z80_main.b)


adc_a_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_c z80_main z80_flags =
    -- case 0x89: adc(C); break;
    --z80 |> set_flag_regs (adc z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.c)


adc_a_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_d z80_main z80_flags =
    -- case 0x8A: adc(D); break;
    --z80 |> set_flag_regs (adc z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.d)


adc_a_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_e z80_main z80_flags =
    -- case 0x8B: adc(E); break;
    --z80 |> set_flag_regs (adc z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> adc z80_main.e)


adc_a_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_h z80_main z80_flags =
    -- case 0x84: add(HL>>>8); break;
    -- case 0x84: add(xy>>>8); break;
    --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> adc (shiftRightBy8 z80_main.hl))


adc_a_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
adc_a_l z80_main z80_flags =
    -- case 0x85: add(HL&0xFF); break;
    -- case 0x85: add(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> adc (Bitwise.and z80_main.hl 0xFF))


sub_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_b z80_main z80_flags =
    -- case 0x90: sub(B); break;
    --z80 |> set_flag_regs (z80_sub z80.main.b z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.b)


sub_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_c z80_main z80_flags =
    -- case 0x91: sub(C); break;
    --z80 |> set_flag_regs (z80_sub z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.c)


sub_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_d z80_main z80_flags =
    -- case 0x92: sub(D); break;
    --z80 |> set_flag_regs (z80_sub z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.d)


sub_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_e z80_main z80_flags =
    -- case 0x93: sub(E); break;
    --z80 |> set_flag_regs (z80_sub z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub z80_main.e)


sub_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_h z80_main z80_flags =
    -- case 0x94: sub(HL>>>8); break;
    -- case 0x94: sub(xy>>>8); break;
    --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub (shiftRightBy8 z80_main.hl))


sub_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sub_l z80_main z80_flags =
    -- case 0x95: sub(HL&0xFF); break;
    -- case 0x95: sub(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> z80_sub (Bitwise.and z80_main.hl 0xFF))


sbc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_b z80_main z80_flags =
    -- case 0x98: sbc(B); break;
    --z80 |> set_flag_regs (sbc z80.main.b z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.b)


sbc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_c z80_main z80_flags =
    -- case 0x99: sbc(C); break;
    --z80 |> set_flag_regs (sbc z80.main.c z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.c)


sbc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_d z80_main z80_flags =
    -- case 0x9A: sbc(D); break;
    --z80 |> set_flag_regs (sbc z80.main.d z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.d)


sbc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_e z80_main z80_flags =
    -- case 0x9B: sbc(E); break;
    --z80 |> set_flag_regs (sbc z80.main.e z80.flags)
    Z80ChangeFlags (z80_flags |> sbc z80_main.e)


sbc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_h z80_main z80_flags =
    -- case 0x9C: sbc(HL>>>8); break;
    -- case 0x9C: sbc(xy>>>8); break;
    --z80 |> set_flag_regs (sbc (get_h ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> sbc (shiftRightBy8 z80_main.hl))


sbc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sbc_l z80_main z80_flags =
    -- case 0x9D: sbc(HL&0xFF); break;
    -- case 0x9D: sbc(xy&0xFF); break;
    --z80 |> set_flag_regs (sbc (get_l ixiyhl z80.main) z80.flags)
    Z80ChangeFlags (z80_flags |> sbc (Bitwise.and z80_main.hl 0xFF))


and_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_b z80_main z80_flags =
    -- case 0xA0: and(B); break;
    --z80 |> set_flag_regs (z80_and z80.main.b z80.flags)
    z80_flags |> z80_and z80_main.b |> Z80ChangeFlags


and_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_c z80_main z80_flags =
    -- case 0xA1: and(C); break;
    --z80 |> set_flag_regs (z80_and z80.main.c z80.flags)
    z80_flags |> z80_and z80_main.c |> Z80ChangeFlags


and_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_d z80_main z80_flags =
    -- case 0xA2: and(D); break;
    --z80 |> set_flag_regs (z80_and z80.main.d z80.flags)
    z80_flags |> z80_and z80_main.d |> Z80ChangeFlags


and_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_e z80_main z80_flags =
    -- case 0xA3: and(E); break;
    --z80 |> set_flag_regs (z80_and z80.main.e z80.flags)
    z80_flags |> z80_and z80_main.e |> Z80ChangeFlags


and_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_h z80_main z80_flags =
    -- case 0xA4: and(HL>>>8); break;
    -- case 0xA4: and(xy>>>8); break;
    --z80 |> set_flag_regs (z80_and (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_and (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


and_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
and_l z80_main z80_flags =
    -- case 0xA5: and(HL&0xFF); break;
    -- case 0xA5: and(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_and (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_and (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


xor_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_b z80_main z80_flags =
    -- case 0xA8: xor(B); break;
    z80_flags |> z80_xor z80_main.b |> Z80ChangeFlags


xor_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_c z80_main z80_flags =
    -- case 0xA9: xor(C); break;
    z80_flags |> z80_xor z80_main.c |> Z80ChangeFlags


xor_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_d z80_main z80_flags =
    -- case 0xAA: xor(D); break;
    z80_flags |> z80_xor z80_main.d |> Z80ChangeFlags


xor_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_e z80_main z80_flags =
    -- case 0xAB: xor(E); break;
    z80_flags |> z80_xor z80_main.e |> Z80ChangeFlags


xor_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_h z80_main z80_flags =
    -- case 0xAC: xor(HL>>>8); break;
    -- case 0xAC: xor(xy>>>8); break;
    z80_flags |> z80_xor (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


xor_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
xor_l z80_main z80_flags =
    -- case 0xAD: xor(HL&0xFF); break;
    -- case 0xAD: xor(xy&0xFF); break;
    z80_flags |> z80_xor (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


or_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_b z80_main z80_flags =
    -- case 0xB0: or(B); break;
    --z80 |> set_flag_regs (z80_or z80.main.b z80.flags)
    z80_flags |> z80_or z80_main.b |> Z80ChangeFlags


or_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_c z80_main z80_flags =
    -- case 0xB1: or(C); break;
    z80_flags |> z80_or z80_main.c |> Z80ChangeFlags


or_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_d z80_main z80_flags =
    -- case 0xB2: or(D); break;
    z80_flags |> z80_or z80_main.d |> Z80ChangeFlags


or_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_e z80_main z80_flags =
    -- case 0xB3: or(E); break;
    --z80 |> set_flag_regs (z80_or z80.main.e z80.flags)
    z80_flags |> z80_or z80_main.e |> Z80ChangeFlags


or_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_h z80_main z80_flags =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_or (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


or_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
or_l z80_main z80_flags =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_or (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


cp_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_b z80_main z80_flags =
    -- case 0xB8: cp(B); break;
    --z80 |> set_flag_regs (cp z80.main.b z80.flags)
    z80_flags |> z80_cp z80_main.b |> Z80ChangeFlags


cp_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_c z80_main z80_flags =
    -- case 0xB9: cp(C); break;
    --z80 |> set_flag_regs (cp z80.main.c z80.flags)
    z80_flags |> z80_cp z80_main.c |> Z80ChangeFlags


cp_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_d z80_main z80_flags =
    -- case 0xBA: cp(D); break;
    --z80 |> set_flag_regs (cp z80.main.d z80.flags)
    z80_flags |> z80_cp z80_main.d |> Z80ChangeFlags


cp_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_e z80_main z80_flags =
    -- case 0xBB: cp(E); break;
    --z80 |> set_flag_regs (cp z80.main.e z80.flags)
    z80_flags |> z80_cp z80_main.e |> Z80ChangeFlags


cp_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_h z80_main z80_flags =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
    z80_flags |> z80_cp (shiftRightBy8 z80_main.hl) |> Z80ChangeFlags


cp_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
cp_l z80_main z80_flags =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
    z80_flags |> z80_cp (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


add_hl_de : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_hl_de z80_main z80_flags =
    -- case 0x19: HL=add16(HL,D<<8|E); break;
    -- case 0x19: xy=add16(xy,D<<8|E); break;
    let
        new_xy =
            add16 z80_main.hl (get_de z80_main) z80_flags
    in
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


ld_indirect_bc_a : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ld_indirect_bc_a z80_main z80_flags =
    -- case 0x02: MP=(v=B<<8|C)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        addr =
            shiftLeftBy8 z80_main.b + z80_main.c
    in
    --{ z80 | env = z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 }
    --SetMem8WithTime addr z80.flags.a 3
    Z80ChangeSetIndirect addr z80_flags.a increment3


ld_indirect_de_a : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ld_indirect_de_a z80_main z80_flags =
    -- case 0x12: MP=(v=D<<8|E)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        addr =
            shiftLeftBy8 z80_main.d + z80_main.e
    in
    --z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 |> OnlyEnv
    --SetMem8WithTime addr z80.flags.a 3
    Z80ChangeSetIndirect addr z80_flags.a increment3


rlc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    let
        value =
            shifter0 z80_main.b z80_flags
    in
    FlagsWithBRegister value.flags value.value


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    let
        value =
            shifter0 z80_main.c z80_flags
    in
    FlagsWithCRegister value.flags value.value


rlc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    let
        value =
            shifter0 z80_main.d z80_flags
    in
    FlagsWithDRegister value.flags value.value


rlc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter0 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rlc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter0 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rlc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter0 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


ld_indirect_hl_a : MainWithIndexRegisters -> FlagRegisters -> Z80Change
ld_indirect_hl_a z80_main z80_flags =
    -- case 0x77: env.mem(HL,A); time+=3; break;
    -- case 0x77: env.mem(getd(xy),A); time+=3; break;
    Z80ChangeSetIndirect z80_main.hl z80_flags.a increment3
