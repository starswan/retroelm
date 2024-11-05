module SimpleFlagOps exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..))
import Dict exposing (Dict)
import Utils exposing (shiftRightBy8)
import Z80Change exposing (FlagChange(..))
import Z80Flags exposing (FlagRegisters, adc, c_FP, c_FS, cpl, daa, dec, get_af, get_flags, inc, rot, sbc, scf_ccf, shifter0, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)


singleByteFlags : Dict Int ( FlagRegisters -> FlagChange, Int )
singleByteFlags =
    Dict.fromList
        [ ( 0x07, ( rlca, 1 ) )
        , ( 0x0F, ( rrca, 1 ) )
        , ( 0x17, ( rla, 1 ) )
        , ( 0x1F, ( rra, 1 ) )
        , ( 0x27, ( z80_daa, 1 ) )
        , ( 0x2F, ( z80_cpl, 1 ) )
        , ( 0x37, ( scf, 1 ) )
        , ( 0x3C, ( inc_a, 1 ) )
        , ( 0x3D, ( dec_a, 1 ) )
        , ( 0x3F, ( ccf, 1 ) )
        , ( 0x47, ( ld_b_a, 1 ) )
        , ( 0x4F, ( ld_c_a, 1 ) )
        , ( 0x57, ( ld_d_a, 1 ) )
        , ( 0x5F, ( ld_e_a, 1 ) )
        , ( 0x67, ( ld_h_a, 1 ) )
        , ( 0x6F, ( ld_l_a, 1 ) )
        , ( 0x87, ( add_a_a, 1 ) )
        , ( 0x8F, ( adc_a_a, 1 ) )
        , ( 0x97, ( sub_a, 1 ) )
        , ( 0x9F, ( sbc_a, 1 ) )
        , ( 0xA7, ( and_a, 1 ) )
        , ( 0xAF, ( xor_a, 1 ) )
        , ( 0xB7, ( or_a, 1 ) )
        , ( 0xBF, ( cp_a, 1 ) )
        , ( 0xC0, ( ret_nz, 1 ) )
        , ( 0xC8, ( ret_z, 1 ) )
        , ( 0xD0, ( ret_nc, 1 ) )
        , ( 0xD8, ( ret_c, 1 ) )
        , ( 0xE0, ( ret_po, 1 ) )
        , ( 0xE8, ( ret_pe, 1 ) )
        , ( 0xF0, ( ret_p, 1 ) )
        , ( 0xF5, ( push_af, 1 ) )
        , ( 0xF8, ( ret_m, 1 ) )
        , ( 0xCB07, ( rlc_a, 2 ) )
        ]


rlca : FlagRegisters -> FlagChange
rlca z80_flags =
    -- case 0x07: rot(A*0x101>>>7); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
    z80_flags |> rot (Bitwise.shiftRightBy 7 (z80_flags.a * 0x0101)) |> OnlyFlags


rrca : FlagRegisters -> FlagChange
rrca z80_flags =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
    z80_flags |> rot (Bitwise.shiftRightBy 24 (z80_flags.a * 0x80800000)) |> OnlyFlags


rla : FlagRegisters -> FlagChange
rla z80_flags =
    -- case 0x17: rot(A<<1|Ff>>>8&1); break;
    -- { z80 | flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a)
    --                                                                           (Bitwise.and (shiftRightBy8 z80.flags.ff) 1)) }
    z80_flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80_flags.a) (Bitwise.and (shiftRightBy8 z80_flags.ff) 1)) |> OnlyFlags


rra : FlagRegisters -> FlagChange
rra z80_flags =
    -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201)
    --                                                                           (Bitwise.and z80.flags.ff 0x100))) }
    z80_flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80_flags.a * 0x0201) (Bitwise.and z80_flags.ff 0x0100))) |> OnlyFlags


scf : FlagRegisters -> FlagChange
scf z80_flags =
    -- case 0x37: scf_ccf(0); break;
    --{ z80 | flags = z80.flags |> scf_ccf 0 }
    z80_flags |> scf_ccf 0 |> OnlyFlags


inc_a : FlagRegisters -> FlagChange
inc_a z80_flags =
    -- case 0x3C: A=inc(A); break;
    let
        v =
            inc z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value } |> OnlyFlags


dec_a : FlagRegisters -> FlagChange
dec_a z80_flags =
    -- case 0x3D: A=dec(A); break;
    let
        v =
            dec z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value } |> OnlyFlags


ccf : FlagRegisters -> FlagChange
ccf z80_flags =
    -- case 0x3F: scf_ccf(Ff&0x100); break;
    z80_flags |> scf_ccf (Bitwise.and z80_flags.ff 0x0100) |> OnlyFlags


ld_b_a : FlagRegisters -> FlagChange
ld_b_a z80_flags =
    -- case 0x47: B=A; break;
    --z80 |> set_b z80.flags.a
    FlagChangeB z80_flags.a


ld_c_a : FlagRegisters -> FlagChange
ld_c_a z80_flags =
    -- case 0x4F: C=A; break;
    --z80 |> set_c z80.flags.a
    FlagChangeC z80_flags.a


z80_daa : FlagRegisters -> FlagChange
z80_daa z80_flags =
    -- case 0x27: daa(); break;
    --{ z80 | flags = daa z80.flags }
    z80_flags |> daa |> OnlyFlags


z80_cpl : FlagRegisters -> FlagChange
z80_cpl z80_flags =
    z80_flags |> cpl |> OnlyFlags


ld_d_a : FlagRegisters -> FlagChange
ld_d_a z80_flags =
    -- case 0x57: D=A; break;
    --z80 |> set_d z80.flags.a
    FlagChangeD z80_flags.a


ld_e_a : FlagRegisters -> FlagChange
ld_e_a z80_flags =
    -- case 0x5F: E=A; break;
    --z80 |> set_e z80.flags.a
    FlagChangeE z80_flags.a


ld_h_a : FlagRegisters -> FlagChange
ld_h_a z80_flags =
    -- case 0x67: HL=HL&0xFF|A<<8; break;
    -- case 0x67: xy=xy&0xFF|A<<8; break;
    --z80 |> set_h_z80 z80.flags.a ixiyhl
    FlagChangeH z80_flags.a


ld_l_a : FlagRegisters -> FlagChange
ld_l_a z80_flags =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    FlagChangeL z80_flags.a


add_a_a : FlagRegisters -> FlagChange
add_a_a z80_flags =
    -- case 0x87: add(A); break;
    z80_flags |> z80_add z80_flags.a |> OnlyFlags


adc_a_a : FlagRegisters -> FlagChange
adc_a_a z80_flags =
    -- case 0x8F: adc(A); break;
    z80_flags |> adc z80_flags.a |> OnlyFlags


sub_a : FlagRegisters -> FlagChange
sub_a z80_flags =
    -- case 0x97: sub(A); break;
    --z80 |> set_flag_regs (z80_sub z80.flags.a z80.flags)
    z80_flags |> z80_sub z80_flags.a |> OnlyFlags


sbc_a : FlagRegisters -> FlagChange
sbc_a z80_flags =
    -- case 0x9F: sbc(A); break;
    --z80 |> set_flag_regs (sbc z80.flags.a z80.flags)
    z80_flags |> sbc z80_flags.a |> OnlyFlags


and_a : FlagRegisters -> FlagChange
and_a z80_flags =
    -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
    -- and a is correct - I guess the above is a faster implementation
    z80_flags |> z80_and z80_flags.a |> OnlyFlags


xor_a : FlagRegisters -> FlagChange
xor_a z80_flags =
    -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
    z80_flags |> z80_xor z80_flags.a |> OnlyFlags


or_a : FlagRegisters -> FlagChange
or_a z80_flags =
    -- case 0xB7: or(A); break;
    --z80 |> set_flag_regs (z80_or z80.flags.a z80.flags)
    z80_flags |> z80_or z80_flags.a |> OnlyFlags


cp_a : FlagRegisters -> FlagChange
cp_a z80_flags =
    -- case 0xBF: cp(A); break;
    --z80 |> set_flag_regs (cp z80.flags.a z80.flags)
    z80_flags |> z80_cp z80_flags.a |> OnlyFlags


increment1 =
    CpuTimeIncrement 1


ret_nz : FlagRegisters -> FlagChange
ret_nz z80_flags =
    -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
    if z80_flags.fr /= 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_z : FlagRegisters -> FlagChange
ret_z z80_flags =
    -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
    if z80_flags.fr == 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_nc : FlagRegisters -> FlagChange
ret_nc z80_flags =
    -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_c : FlagRegisters -> FlagChange
ret_c z80_flags =
    -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_po : FlagRegisters -> FlagChange
ret_po z80_flags =
    -- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
    if Bitwise.and (z80_flags |> get_flags) c_FP == 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_pe : FlagRegisters -> FlagChange
ret_pe z80_flags =
    -- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
    if Bitwise.and (z80_flags |> get_flags) c_FP /= 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_p : FlagRegisters -> FlagChange
ret_p z80_flags =
    -- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff c_FS == 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


ret_m : FlagRegisters -> FlagChange
ret_m z80_flags =
    -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
    if Bitwise.and z80_flags.ff c_FS /= 0 then
        ReturnWithPop increment1

    else
        EmptyFlagChange increment1


push_af : FlagRegisters -> FlagChange
push_af z80_flags =
    -- case 0xF5: push(A<<8|flags()); break;
    --let
    --    a =
    --        z80 |> get_af
    --
    --    --pushed = z80.env |> z80_push a
    --in
    ----{ z80 | env = pushed }
    FlagChangePush (z80_flags |> get_af)


rlc_a : FlagRegisters -> FlagChange
rlc_a z80_flags =
    --case 0x07: A=shifter(o,A); break;
    let
        value =
            shifter0 z80_flags.a z80_flags

        new_flags =
            value.flags
    in
    OnlyFlags { new_flags | a = value.value }
