module SimpleFlagOps exposing (..)

import Bitwise
import Dict exposing (Dict)
import Utils exposing (shiftRightBy8)
import Z80Change exposing (FlagChange(..))
import Z80Flags exposing (FlagRegisters, cpl, daa, dec, inc, rot, scf_ccf)


singleByteFlags : Dict Int (FlagRegisters -> FlagChange)
singleByteFlags =
    Dict.fromList
        [ ( 0x07, rlca )
        , ( 0x0F, rrca )
        , ( 0x17, rla )
        , ( 0x1F, rra )
        , ( 0x27, z80_daa )
        , ( 0x2F, z80_cpl )
        , ( 0x37, scf )
        , ( 0x3C, inc_a )
        , ( 0x3D, dec_a )
        , ( 0x3F, ccf )
        , ( 0x47, ld_b_a )
        , ( 0x4F, ld_c_a )
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
