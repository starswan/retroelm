module SimpleSingleByte exposing (..)

import Bitwise
import Dict exposing (Dict)
import Utils exposing (shiftRightBy8)
import Z80Change exposing (Z80Change(..))
import Z80ChangeData exposing (Z80ChangeData)
import Z80Flags exposing (FlagRegisters, cpl, daa, dec, inc, rot, scf_ccf)
import Z80Types exposing (MainRegisters, MainWithIndexRegisters, Z80)


singleByteMainAndFlagRegisters : Dict Int (MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData)
singleByteMainAndFlagRegisters =
    Dict.fromList
        [ ( 0x03, inc_bc )
        , ( 0x04, inc_b )
        , ( 0x05, dec_b )
        , ( 0x0B, dec_bc )
        , ( 0x0C, inc_c )
        , ( 0x0D, dec_c )
        , ( 0x13, inc_de )
        , ( 0x14, inc_d )
        , ( 0x15, dec_d )
        , ( 0x1B, dec_de )
        , ( 0x1C, inc_e )
        , ( 0x1D, dec_e )
        ]


singleByteFlags : Dict Int (FlagRegisters -> FlagRegisters)
singleByteFlags =
    Dict.fromList
        [ ( 0x07, rlca )
        , ( 0x0F, rrca )
        , ( 0x17, rla )
        , ( 0x1F, rra )
        , ( 0x27, daa )
        , ( 0x2F, cpl )
        , ( 0x37, scf )
        , ( 0x3C, inc_a )
        , ( 0x3D, dec_a )
        , ( 0x3F, ccf )
        ]


singleByteMainRegs : Dict Int (MainWithIndexRegisters -> Z80ChangeData)
singleByteMainRegs =
    Dict.empty


inc_bc : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_bc z80_main _ =
    -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
    let
        changes =
            if z80_main.c == 0xFF then
                BCRegister (Bitwise.and (z80_main.b + 1) 0xFF) 0

            else
                CRegister (z80_main.c + 1)
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
    { changes = changes, cpu_time = 2, pc_change = 1 }


inc_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_b z80_main z80_flags =
    -- case 0x04: B=inc(B); break;
    let
        new_b =
            inc z80_main.b z80_flags

        changes =
            FlagsWithBRegister new_b.flags new_b.value
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = changes, cpu_time = 0, pc_change = 1 }


dec_b : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_b z80_main z80_flags =
    -- case 0x05: B=dec(B); break;
    let
        new_b =
            dec z80_main.b z80_flags

        changes =
            FlagsWithBRegister new_b.flags new_b.value
    in
    --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
    --FlagsWithMain new_b.flags { z80_main | b = new_b.value }
    { changes = changes, cpu_time = 0, pc_change = 1 }


rlca : FlagRegisters -> FlagRegisters
rlca z80_flags =
    -- case 0x07: rot(A*0x101>>>7); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
    z80_flags |> rot (Bitwise.shiftRightBy 7 (z80_flags.a * 0x0101))


dec_bc : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_bc z80_main _ =
    -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
    let
        tmp_c =
            z80_main.c - 1

        changes =
            if tmp_c < 0 then
                BCRegister (Bitwise.and (z80_main.b - 1) 0xFF) 0xFF

            else
                CRegister tmp_c
    in
    --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
    { changes = changes, cpu_time = 2, pc_change = 1 }


inc_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_c z80_main z80_flags =
    -- case 0x0C: C=inc(C); break;
    let
        new_c =
            inc z80_main.c z80_flags

        changes =
            FlagsWithCRegister new_c.flags new_c.value
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = changes, cpu_time = 0, pc_change = 1 }


dec_c : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_c z80_main z80_flags =
    -- case 0x0D: C=dec(C); break;
    let
        new_c =
            dec z80_main.c z80_flags

        changes =
            FlagsWithCRegister new_c.flags new_c.value
    in
    --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
    { changes = changes, cpu_time = 0, pc_change = 1 }


rrca : FlagRegisters -> FlagRegisters
rrca z80_flags =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
    z80_flags |> rot (Bitwise.shiftRightBy 24 (z80_flags.a * 0x80800000))


inc_de : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_de z80_main _ =
    -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
    let
        tmp_e =
            z80_main.e + 1

        changes =
            if tmp_e == 256 then
                DERegister (Bitwise.and (z80_main.d + 1) 0xFF) 0

            else
                ERegister tmp_e

        --env_1 =
        --    z80.env |> addCpuTimeEnv 2
        --main_1 =
        --    { z80_main | d = reg_d, e = reg_e }
    in
    --{ z80 | env = env_1, main = main_1 }
    --MainRegsWithEnv main_1 env_1
    { changes = changes, cpu_time = 2, pc_change = 1 }


inc_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_d z80_main z80_flags =
    -- case 0x14: D=inc(D); break;
    let
        new_d =
            inc z80_main.d z80_flags

        changes =
            FlagsWithDRegister new_d.flags new_d.value
    in
    { changes = changes, cpu_time = 0, pc_change = 1 }


dec_d : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_d z80_main z80_flags =
    -- case 0x15: D=dec(D); break;
    let
        new_d =
            dec z80_main.d z80_flags
    in
    --{ z80 | flags = new_d.flags, main = main_1 }
    { changes = FlagsWithDRegister new_d.flags new_d.value, cpu_time = 0, pc_change = 1 }


rla : FlagRegisters -> FlagRegisters
rla z80_flags =
    -- case 0x17: rot(A<<1|Ff>>>8&1); break;
    -- { z80 | flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a)
    --                                                                           (Bitwise.and (shiftRightBy8 z80.flags.ff) 1)) }
    z80_flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80_flags.a) (Bitwise.and (shiftRightBy8 z80_flags.ff) 1))


dec_de : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_de z80_main _ =
    -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
    let
        tmp_e =
            z80_main.e - 1

        changes =
            if tmp_e < 0 then
                DERegister (Bitwise.and (z80_main.d - 1) 0xFF) 0xFF

            else
                ERegister tmp_e
    in
    { changes = changes, cpu_time = 2, pc_change = 1 }



inc_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
inc_e z80_main z80_flags =
    -- case 0x1C: E=inc(E); break;
    let
        new_e =
            inc z80_main.e z80_flags
    in
    --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = 0, pc_change = 1 }


dec_e : MainWithIndexRegisters -> FlagRegisters -> Z80ChangeData
dec_e z80_main z80_flags =
    -- case 0x1D: E=dec(E); break;
    let
        new_e =
            dec z80_main.e z80_flags
    in
    { changes = FlagsWithERegister new_e.flags new_e.value, cpu_time = 0, pc_change = 1 }


rra : FlagRegisters -> FlagRegisters
rra z80_flags =
    -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201)
    --                                                                           (Bitwise.and z80.flags.ff 0x100))) }
    z80_flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80_flags.a * 0x0201) (Bitwise.and z80_flags.ff 0x0100)))



--z80_daa : FlagRegisters -> FlagRegisters
--z80_daa z80_flags =
--    -- case 0x27: daa(); break;
--    --{ z80 | flags = daa z80.flags }
--    z80_flags |> daa
--
--
--z80_cpl : FlagRegisters -> FlagRegisters
--z80_cpl z80_flags =
--            z80_flags |> cpl


scf : FlagRegisters -> FlagRegisters
scf z80_flags =
    -- case 0x37: scf_ccf(0); break;
    --{ z80 | flags = z80.flags |> scf_ccf 0 }
    z80_flags |> scf_ccf 0


inc_a : FlagRegisters -> FlagRegisters
inc_a z80_flags =
    -- case 0x3C: A=inc(A); break;
    let
        v =
            inc z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value }


dec_a : FlagRegisters -> FlagRegisters
dec_a z80_flags =
    -- case 0x3D: A=dec(A); break;
    let
        v =
            dec z80_flags.a z80_flags

        new_flags =
            v.flags
    in
    { new_flags | a = v.value }


ccf : FlagRegisters -> FlagRegisters
ccf z80_flags =
    -- case 0x3F: scf_ccf(Ff&0x100); break;
    z80_flags |> scf_ccf (Bitwise.and z80_flags.ff 0x0100)
