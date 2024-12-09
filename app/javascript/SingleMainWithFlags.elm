module SingleMainWithFlags exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..), increment0, increment3)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80Flags exposing (BitTest(..), FlagRegisters, IntWithFlags, adc, add16, dec, inc, sbc, shifter0, shifter1, shifter2, shifter3, shifter4, shifter5, shifter6, shifter7, testBit, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Types exposing (MainWithIndexRegisters, get_bc, get_de)


singleByteMainAndFlagRegisters : Dict Int ( MainWithIndexRegisters -> FlagRegisters -> Z80Change, PCIncrement )
singleByteMainAndFlagRegisters =
    Dict.fromList
        [ ( 0x02, ( ld_indirect_bc_a, IncrementByOne ) )
        , ( 0x04, ( inc_b, IncrementByOne ) )
        , ( 0x05, ( dec_b, IncrementByOne ) )
        , ( 0x09, ( add_hl_bc, IncrementByOne ) )
        , ( 0xDD09, ( add_ix_bc, IncrementByTwo ) )
        , ( 0xFD09, ( add_iy_bc, IncrementByTwo ) )
        , ( 0x0C, ( inc_c, IncrementByOne ) )
        , ( 0x0D, ( dec_c, IncrementByOne ) )
        , ( 0x12, ( ld_indirect_de_a, IncrementByOne ) )
        , ( 0x14, ( inc_d, IncrementByOne ) )
        , ( 0x15, ( dec_d, IncrementByOne ) )
        , ( 0x19, ( add_hl_de, IncrementByOne ) )
        , ( 0xDD19, ( add_ix_de, IncrementByTwo ) )
        , ( 0xFD19, ( add_iy_de, IncrementByTwo ) )
        , ( 0x1C, ( inc_e, IncrementByOne ) )
        , ( 0x1D, ( dec_e, IncrementByOne ) )
        , ( 0x24, ( inc_h, IncrementByOne ) )
        , ( 0xDD24, ( inc_h_ix, IncrementByTwo ) )
        , ( 0xFD24, ( inc_h_iy, IncrementByTwo ) )
        , ( 0x25, ( dec_h, IncrementByOne ) )
        , ( 0xDD25, ( dec_h_ix, IncrementByTwo ) )
        , ( 0xFD25, ( dec_h_iy, IncrementByTwo ) )
        , ( 0x29, ( add_hl_hl, IncrementByOne ) )
        , ( 0xDD29, ( add_ix_ix, IncrementByTwo ) )
        , ( 0xFD29, ( add_iy_iy, IncrementByTwo ) )
        , ( 0x2C, ( inc_l, IncrementByOne ) )
        , ( 0xDD2C, ( inc_ix_l, IncrementByTwo ) )
        , ( 0xFD2C, ( inc_iy_l, IncrementByTwo ) )
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
        , ( 0xCB08, ( rrc_b, IncrementByTwo ) )
        , ( 0xCB09, ( rrc_c, IncrementByTwo ) )
        , ( 0xCB0A, ( rrc_d, IncrementByTwo ) )
        , ( 0xCB0B, ( rrc_e, IncrementByTwo ) )
        , ( 0xCB0C, ( rrc_h, IncrementByTwo ) )
        , ( 0xCB0D, ( rrc_l, IncrementByTwo ) )
        , ( 0xCB10, ( rl_b, IncrementByTwo ) )
        , ( 0xCB11, ( rl_c, IncrementByTwo ) )
        , ( 0xCB12, ( rl_d, IncrementByTwo ) )
        , ( 0xCB13, ( rl_e, IncrementByTwo ) )
        , ( 0xCB14, ( rl_h, IncrementByTwo ) )
        , ( 0xCB15, ( rl_l, IncrementByTwo ) )
        , ( 0xCB18, ( rr_b, IncrementByTwo ) )
        , ( 0xCB19, ( rr_c, IncrementByTwo ) )
        , ( 0xCB1A, ( rr_d, IncrementByTwo ) )
        , ( 0xCB1B, ( rr_e, IncrementByTwo ) )
        , ( 0xCB1C, ( rr_h, IncrementByTwo ) )
        , ( 0xCB1D, ( rr_l, IncrementByTwo ) )
        , ( 0xCB20, ( sla_b, IncrementByTwo ) )
        , ( 0xCB21, ( sla_c, IncrementByTwo ) )
        , ( 0xCB22, ( sla_d, IncrementByTwo ) )
        , ( 0xCB23, ( sla_e, IncrementByTwo ) )
        , ( 0xCB24, ( sla_h, IncrementByTwo ) )
        , ( 0xCB25, ( sla_l, IncrementByTwo ) )
        , ( 0xCB28, ( sra_b, IncrementByTwo ) )
        , ( 0xCB29, ( sra_c, IncrementByTwo ) )
        , ( 0xCB2A, ( sra_d, IncrementByTwo ) )
        , ( 0xCB2B, ( sra_e, IncrementByTwo ) )
        , ( 0xCB2C, ( sra_h, IncrementByTwo ) )
        , ( 0xCB2D, ( sra_l, IncrementByTwo ) )
        , ( 0xCB30, ( sll_b, IncrementByTwo ) )
        , ( 0xCB31, ( sll_c, IncrementByTwo ) )
        , ( 0xCB32, ( sll_d, IncrementByTwo ) )
        , ( 0xCB33, ( sll_e, IncrementByTwo ) )
        , ( 0xCB34, ( sll_h, IncrementByTwo ) )
        , ( 0xCB35, ( sll_l, IncrementByTwo ) )
        , ( 0xCB38, ( srl_b, IncrementByTwo ) )
        , ( 0xCB39, ( srl_c, IncrementByTwo ) )
        , ( 0xCB3A, ( srl_d, IncrementByTwo ) )
        , ( 0xCB3B, ( srl_e, IncrementByTwo ) )
        , ( 0xCB3C, ( srl_h, IncrementByTwo ) )
        , ( 0xCB3D, ( srl_l, IncrementByTwo ) )
        , ( 0xCB40, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB41, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB42, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB43, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB44, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB45, ( \z80_main z80_flags -> z80_flags |> testBit Bit_0 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB48, ( bit_1_b, IncrementByTwo ) )
        , ( 0xCB49, ( bit_1_c, IncrementByTwo ) )
        , ( 0xCB4A, ( bit_1_d, IncrementByTwo ) )
        , ( 0xCB4B, ( bit_1_e, IncrementByTwo ) )
        , ( 0xCB4C, ( bit_1_h, IncrementByTwo ) )
        , ( 0xCB4D, ( bit_1_l, IncrementByTwo ) )
        , ( 0xCB50, ( bit_2_b, IncrementByTwo ) )
        , ( 0xCB51, ( bit_2_c, IncrementByTwo ) )
        , ( 0xCB52, ( bit_2_d, IncrementByTwo ) )
        , ( 0xCB53, ( bit_2_e, IncrementByTwo ) )
        , ( 0xCB54, ( bit_2_h, IncrementByTwo ) )
        , ( 0xCB55, ( bit_2_l, IncrementByTwo ) )
        , ( 0xCB58, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB59, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB5A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB5B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB5C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB5D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_3 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB60, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB61, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB62, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB63, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB64, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB65, ( \z80_main z80_flags -> z80_flags |> testBit Bit_4 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB68, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB69, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB6A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB6B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB6C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB6D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_5 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB70, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB71, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB72, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB73, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB74, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB75, ( \z80_main z80_flags -> z80_flags |> testBit Bit_6 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB78, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.b |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB79, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.c |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB7A, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.d |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB7B, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 z80_main.e |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB7C, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags, IncrementByTwo ) )
        , ( 0xCB7D, ( \z80_main z80_flags -> z80_flags |> testBit Bit_7 (z80_main.hl |> Bitwise.and 0xFF) |> Z80ChangeFlags, IncrementByTwo ) )
        ]


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    z80_flags |> inc z80_main.b |> FlagsWithBRegister


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    z80_flags |> dec z80_main.b |> FlagsWithBRegister


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    z80_flags |> inc z80_main.c |> FlagsWithCRegister


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    z80_flags |> dec z80_main.c |> FlagsWithCRegister


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    z80_flags |> inc z80_main.d |> FlagsWithDRegister


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    z80_flags |> dec z80_main.d |> FlagsWithDRegister


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


inc_h_ix : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_h_ix z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (shiftRightBy8 z80_main.ix) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.ix 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithIXRegister value.flags new_xy increment0


inc_h_iy : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_h_iy z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (shiftRightBy8 z80_main.iy) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.iy 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithIYRegister value.flags new_xy increment0


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


dec_h_ix : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_h_ix z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (shiftRightBy8 z80_main.ix) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.ix 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithIXRegister value.flags new_xy increment0


dec_h_iy : MainWithIndexRegisters -> FlagRegisters -> Z80Change
dec_h_iy z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (shiftRightBy8 z80_main.iy) z80_flags

        new_xy =
            Bitwise.or (Bitwise.and z80_main.iy 0xFF) (shiftLeftBy8 value.value)
    in
    FlagsWithIYRegister value.flags new_xy increment0


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

inc_ix_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_ix_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.ix 0xFF00

        l =
            inc (Bitwise.and z80_main.ix 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    IXRegister new_xy

inc_iy_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
inc_iy_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            Bitwise.and z80_main.iy 0xFF00

        l =
            inc (Bitwise.and z80_main.iy 0xFF) z80_flags

        new_xy =
            Bitwise.or h l.value
    in
    IYRegister new_xy


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
    let
        new_xy =
            add16 z80_main.hl z80_main.hl z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithHLRegister new_xy.flags new_xy.value new_xy.time


add_ix_ix : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_ix_ix z80_main z80_flags =
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 z80_main.ix z80_main.ix z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithIXRegister new_xy.flags new_xy.value new_xy.time


add_iy_iy : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_iy_iy z80_main z80_flags =
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 z80_main.iy z80_main.iy z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    FlagsWithIYRegister new_xy.flags new_xy.value new_xy.time


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


add_ix_bc : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_ix_bc z80_main z80_flags =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            z80_main.ix

        new_xy =
            add16 xy (get_bc z80_main) z80_flags
    in
    FlagsWithIXRegister new_xy.flags new_xy.value new_xy.time


add_iy_bc : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_iy_bc z80_main z80_flags =
    --case 0x09: HL=add16(HL,B<<8|C); break;
    --case 0x09: xy=add16(xy,B<<8|C); break;
    let
        xy =
            z80_main.iy

        new_xy =
            add16 xy (get_bc z80_main) z80_flags
    in
    FlagsWithIYRegister new_xy.flags new_xy.value new_xy.time


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


add_ix_de : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_ix_de z80_main z80_flags =
    -- case 0x19: HL=add16(HL,D<<8|E); break;
    -- case 0x19: xy=add16(xy,D<<8|E); break;
    let
        xy =
            z80_main.ix

        new_xy =
            add16 xy (get_de z80_main) z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags} |> add_cpu_time new_xy.time
    FlagsWithIXRegister new_xy.flags new_xy.value new_xy.time


add_iy_de : MainWithIndexRegisters -> FlagRegisters -> Z80Change
add_iy_de z80_main z80_flags =
    -- case 0x19: HL=add16(HL,D<<8|E); break;
    -- case 0x19: xy=add16(xy,D<<8|E); break;
    let
        xy =
            z80_main.iy

        new_xy =
            add16 xy (get_de z80_main) z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags} |> add_cpu_time new_xy.time
    FlagsWithIYRegister new_xy.flags new_xy.value new_xy.time


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
    z80_flags |> shifter0 z80_main.b |> FlagsWithBRegister


rlc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    --z80_flags |> shifter_c shifter0 z80_main.c
    z80_flags |> shifter0 z80_main.c |> FlagsWithCRegister


rlc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rlc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter0 z80_main.d |> FlagsWithDRegister


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


rrc_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_b z80_main z80_flags =
    z80_flags |> shifter1 z80_main.b |> FlagsWithBRegister


rrc_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter1 z80_main.c |> FlagsWithCRegister


rrc_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter1 z80_main.d |> FlagsWithDRegister


rrc_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter1 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rrc_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter1 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rrc_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rrc_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter1 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter2 z80_main.b |> FlagsWithBRegister


rl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter2 z80_main.c |> FlagsWithCRegister


rl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter2 z80_main.d |> FlagsWithDRegister


rl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter2 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter2 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter2 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rr_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter3 z80_main.b |> FlagsWithBRegister


rr_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter3 z80_main.c |> FlagsWithCRegister


rr_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter3 z80_main.d |> FlagsWithDRegister


rr_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter3 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


rr_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter3 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


rr_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
rr_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter3 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sla_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter4 z80_main.b |> FlagsWithBRegister


sla_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter4 z80_main.c |> FlagsWithCRegister


sla_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter4 z80_main.d |> FlagsWithDRegister


sla_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter4 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sla_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter4 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sla_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sla_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter4 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sra_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter5 z80_main.b |> FlagsWithBRegister


sra_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter5 z80_main.c |> FlagsWithCRegister


sra_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter5 z80_main.d |> FlagsWithDRegister


sra_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter5 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sra_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter5 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sra_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sra_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter5 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sll_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter6 z80_main.b |> FlagsWithBRegister


sll_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter6 z80_main.c |> FlagsWithCRegister


sll_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter6 z80_main.d |> FlagsWithDRegister


sll_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter6 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


sll_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter6 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


sll_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
sll_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter6 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


srl_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_b z80_main z80_flags =
    -- case 0x00: B=shifter(o,B); break;
    z80_flags |> shifter7 z80_main.b |> FlagsWithBRegister


srl_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_c z80_main z80_flags =
    -- case 0x01: C=shifter(o,C); break;
    z80_flags |> shifter7 z80_main.c |> FlagsWithCRegister


srl_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_d z80_main z80_flags =
    -- case 0x02: D=shifter(o,D); break;
    z80_flags |> shifter7 z80_main.d |> FlagsWithDRegister


srl_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_e z80_main z80_flags =
    -- case 0x03: E=shifter(o,E); break;
    let
        value =
            shifter7 z80_main.e z80_flags
    in
    FlagsWithERegister value.flags value.value


srl_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_h z80_main z80_flags =
    --case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break
    let
        value =
            shifter7 (z80_main.hl |> shiftRightBy8) z80_flags

        new_hl =
            Bitwise.or (value.value |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF)
    in
    FlagsWithHLRegister value.flags new_hl increment0


srl_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
srl_l z80_main z80_flags =
    -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
    let
        value =
            shifter7 (Bitwise.and z80_main.hl 0xFF) z80_flags

        new_hl =
            Bitwise.or value.value (Bitwise.and z80_main.hl 0xFF00)
    in
    FlagsWithHLRegister value.flags new_hl increment0


bit_1_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_1 z80_main.b |> Z80ChangeFlags


bit_1_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_c z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.c |> Z80ChangeFlags


bit_1_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_d z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.d |> Z80ChangeFlags


bit_1_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_e z80_main z80_flags =
    z80_flags |> testBit Bit_1 z80_main.e |> Z80ChangeFlags


bit_1_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_h z80_main z80_flags =
    z80_flags |> testBit Bit_1 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_1_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_1_l z80_main z80_flags =
    z80_flags |> testBit Bit_1 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags


bit_2_b : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_b z80_main z80_flags =
    -- case 0x40: bit(o,B); break;
    z80_flags |> testBit Bit_2 z80_main.b |> Z80ChangeFlags


bit_2_c : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_c z80_main z80_flags =
    -- case 0x41: bit(o,C); break;
    z80_flags |> testBit Bit_2 z80_main.c |> Z80ChangeFlags


bit_2_d : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_d z80_main z80_flags =
    -- case 0x42: bit(o,D); break;
    z80_flags |> testBit Bit_2 z80_main.d |> Z80ChangeFlags


bit_2_e : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_e z80_main z80_flags =
    z80_flags |> testBit Bit_2 z80_main.e |> Z80ChangeFlags


bit_2_h : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_h z80_main z80_flags =
    z80_flags |> testBit Bit_2 (z80_main.hl |> shiftRightBy8) |> Z80ChangeFlags


bit_2_l : MainWithIndexRegisters -> FlagRegisters -> Z80Change
bit_2_l z80_main z80_flags =
    z80_flags |> testBit Bit_2 (Bitwise.and z80_main.hl 0xFF) |> Z80ChangeFlags
