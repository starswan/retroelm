module SimpleSingleByte exposing (..)

import Bitwise
import Dict exposing (Dict)
import RegisterChange exposing (RegisterChange(..))
import Utils exposing (shiftLeftBy8, shiftRightBy8)
import Z80Address exposing (decrement, fromInt, increment, lower8Bits, toInt, top8Bits, top8BitsWithoutShift)
import Z80Change exposing (Z80Change(..))
import Z80ChangeData exposing (RegisterChangeData, Z80ChangeData)
import Z80Flags exposing (FlagRegisters, add16, dec, inc)
import Z80Types exposing (IXIYHL(..), MainRegisters, MainWithIndexRegisters, Z80)


singleByteMainAndFlagRegisters : Dict Int (MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData)
singleByteMainAndFlagRegisters =
    Dict.fromList
        [ ( 0x04, inc_b )
        , ( 0x05, dec_b )
        , ( 0x0C, inc_c )
        , ( 0x0D, dec_c )
        , ( 0x14, inc_d )
        , ( 0x15, dec_d )
        , ( 0x1C, inc_e )
        , ( 0x1D, dec_e )
        , ( 0x24, inc_h )
        , ( 0x25, dec_h )
        , ( 0x29, add_hl_hl )
        , ( 0x2C, inc_l )
        , ( 0x2D, dec_l )
        ]


singleByteMainRegs : Dict Int (MainWithIndexRegisters -> RegisterChangeData)
singleByteMainRegs =
    Dict.fromList
        [ ( 0x03, inc_bc )
        , ( 0x0B, dec_bc )
        , ( 0x13, inc_de )
        , ( 0x1B, dec_de )
        , ( 0x23, inc_hl )
        , ( 0x2B, dec_hl )
        , ( 0x41, ld_b_c )
        , ( 0x42, ld_b_d )
        , ( 0x43, ld_b_e )
        , ( 0x44, ld_b_h )
        , ( 0x45, ld_b_l )
        , ( 0x48, ld_c_b )
        , ( 0x4A, ld_c_d )
        , ( 0x4B, ld_c_e )
        , ( 0x4C, ld_c_h )
        , ( 0x4D, ld_c_l )
        ]


inc_bc : MainWithIndexRegisters -> RegisterChangeData
inc_bc z80_main =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    let
        changes =
            if z80_main.c == 0xFF then
                ChangeRegisterBC (Bitwise.and (z80_main.b + 1) 0xFF) 0

            else
                ChangeRegisterC (z80_main.c + 1)
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    { changes = changes, cpu_time = 2 }


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = FlagsWithBRegister new_b.flags new_b.value, cpu_time = 0 }


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80_main.b z80_flags
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = FlagsWithBRegister new_b.flags new_b.value, cpu_time = 0 }


dec_bc : MainWithIndexRegisters -> RegisterChangeData
dec_bc z80_main =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    let
        tmp_c =
            z80_main.c - 1

        changes =
            if tmp_c < 0 then
                ChangeRegisterBC (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF

            else
                ChangeRegisterC tmp_c
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    { changes = changes, cpu_time = 2 }


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = FlagsWithCRegister new_c.flags new_c.value, cpu_time = 0 }


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80_main.c z80_flags
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = FlagsWithCRegister new_c.flags new_c.value, cpu_time = 0 }


inc_de : MainWithIndexRegisters -> RegisterChangeData
inc_de z80_main =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        tmp_e =
            z80_main.e + 1

        changes =
            if tmp_e == 256 then
                ChangeRegisterDE (Bitwise.and (z80_main.d + 1) 0xFF) 0

            else
                ChangeRegisterE tmp_e

        --env_1 =
        --    z80.env |> addCpuTimeEnv 2
        --main_1 =
        --    { z80_main | d = reg_d, e = reg_e }
    in
    --{ z80 | env = env_1, main = main_1 }
    --MainRegsWithEnv main_1 env_1
    { changes = changes, cpu_time = 2 }


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80_main.d z80_flags

        changes =
            FlagsWithDRegister new_d.flags new_d.value
    in
    { changes = changes, cpu_time = 0 }


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80_main.d z80_flags
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    { changes = FlagsWithDRegister new_d.flags new_d.value, cpu_time = 0 }


dec_de : MainWithIndexRegisters -> RegisterChangeData
dec_de z80_main =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        tmp_e =
            z80_main.e - 1

        changes =
            if tmp_e < 0 then
                ChangeRegisterDE (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF

            else
                ChangeRegisterE tmp_e
    in
    { changes = changes, cpu_time = 2 }


inc_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_e z80_main z80_flags =
    -- case 0x1C: E=inc(E); break;
    let
        new_e =
            inc z80_main.e z80_flags
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = 0 }


dec_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_e z80_main z80_flags =
    -- case 0x1D: E=dec(E); break;
    let
        new_e =
            dec z80_main.e z80_flags
    in
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = 0 }


inc_hl : MainWithIndexRegisters -> RegisterChangeData
inc_hl z80_main =
    -- case 0x23: HL=(char)(HL+1); time+=2; break;
    -- case 0x23: xy=(char)(xy+1); time+=2; break;
    --{ changes = ChangeRegisterHL (char (z80_main.hl + 1)), cpu_time = 2, pc_change = 1 }
    { changes = ChangeRegisterHL (z80_main.hl |> increment), cpu_time = 2 }


inc_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_h z80_main z80_flags =
    -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
    -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
    let
        value =
            inc (top8Bits z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (lower8Bits z80_main.hl) (shiftLeftBy8 value.value)
    in
    --{ z80_1 | main = main }
    { changes = FlagsWithHLRegister value.flags (new_xy |> fromInt), cpu_time = 0 }


dec_h : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_h z80_main z80_flags =
    -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
    -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
    let
        value =
            dec (top8Bits z80_main.hl) z80_flags

        new_xy =
            Bitwise.or (lower8Bits z80_main.hl) (shiftLeftBy8 value.value)
    in
    { changes = FlagsWithHLRegister value.flags (new_xy |> fromInt), cpu_time = 0 }


dec_hl : MainWithIndexRegisters -> RegisterChangeData
dec_hl z80_main =
    -- case 0x2B: HL=(char)(HL-1); time+=2; break;
    -- case 0x2B: xy=(char)(xy-1); time+=2; break;
    let
        new_xy =
            --Bitwise.and (z80_main.hl - 1) 0xFFFF
            z80_main.hl |> decrement
    in
    { changes = ChangeRegisterHL new_xy, cpu_time = 2 }


inc_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_l z80_main z80_flags =
    -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
    -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
    let
        h =
            top8BitsWithoutShift z80_main.hl

        l =
            inc (lower8Bits z80_main.hl) z80_flags

        --z80_1 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value |> fromInt
    in
    --{ z80_1 | main = main }
    { changes = HLRegister new_xy, cpu_time = 0 }


dec_l : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_l z80_main z80_flags =
    -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
    -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
    let
        h =
            top8BitsWithoutShift z80_main.hl

        l =
            dec (lower8Bits z80_main.hl) z80_flags

        --new_z80 = { z80 | flags = l.flags }
        new_xy =
            Bitwise.or h l.value |> fromInt
    in
    --{ new_z80 | main = main }
    { changes = HLRegister new_xy, cpu_time = 0 }


ld_b_c : MainWithIndexRegisters -> RegisterChangeData
ld_b_c z80_main =
    -- case 0x41: B=C; break;
    --z80 |> set_b z80.main.c
    { changes = ChangeRegisterB z80_main.c, cpu_time = 0 }


ld_b_d : MainWithIndexRegisters -> RegisterChangeData
ld_b_d z80_main =
    -- case 0x42: B=D; break;
    --z80 |> set_b z80.main.d
    { changes = ChangeRegisterB z80_main.d, cpu_time = 0 }


ld_b_e : MainWithIndexRegisters -> RegisterChangeData
ld_b_e z80_main =
    -- case 0x43: B=E; break;
    --z80 |> set_b z80.main.e
    { changes = ChangeRegisterB z80_main.e, cpu_time = 0 }


ld_b_h : MainWithIndexRegisters -> RegisterChangeData
ld_b_h z80_main =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
    { changes = ChangeRegisterB (top8Bits z80_main.hl), cpu_time = 0 }


ld_b_l : MainWithIndexRegisters -> RegisterChangeData
ld_b_l z80_main =
    -- case 0x45: B=HL&0xFF; break;
    -- case 0x45: B=xy&0xFF; break;
    --  z80 |> set_b (get_l ixiyhl z80.main)
    { changes = ChangeRegisterB (lower8Bits z80_main.hl), cpu_time = 0 }


ld_c_b : MainWithIndexRegisters -> RegisterChangeData
ld_c_b z80_main =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
    { changes = ChangeRegisterC z80_main.b, cpu_time = 0 }


ld_c_d : MainWithIndexRegisters -> RegisterChangeData
ld_c_d z80_main =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
    { changes = ChangeRegisterC z80_main.d, cpu_time = 0 }


add_hl_hl : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
add_hl_hl z80_main z80_flags =
    -- case 0x29: HL=add16(HL,HL); break;
    -- case 0x29: xy=add16(xy,xy); break;
    let
        new_xy =
            add16 (z80_main.hl |> toInt) (z80_main.hl |> toInt) z80_flags
    in
    --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
    { changes = FlagsWithHLRegister new_xy.flags (new_xy.value |> fromInt), cpu_time = new_xy.time }


ld_c_e : MainWithIndexRegisters -> RegisterChangeData
ld_c_e z80_main =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
    { changes = ChangeRegisterC z80_main.e, cpu_time = 0 }


ld_c_h : MainWithIndexRegisters -> RegisterChangeData
ld_c_h z80_main =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
    { changes = ChangeRegisterC (shiftRightBy8 (z80_main.hl |> toInt)), cpu_time = 0 }


ld_c_l : MainWithIndexRegisters -> RegisterChangeData
ld_c_l z80_main =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
    { changes = ChangeRegisterC (Bitwise.and (z80_main.hl |> toInt) 0xFF), cpu_time = 0 }
