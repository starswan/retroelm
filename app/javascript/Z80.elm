--
-- $Id$
--
module Z80 exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimePcAndValue, add_cpu_time_time)
import Dict exposing (Dict)
import Group0x00 exposing (delta_dict_00, delta_dict_lite_00)
import Group0x10 exposing (delta_dict_10, delta_dict_lite_10)
import Group0x20 exposing (delta_dict_20, delta_dict_lite_20)
import Group0x30 exposing (delta_dict_30, delta_dict_lite_30)
import Group0x40 exposing (execute_0x41, execute_0x42, execute_0x43, execute_0x44, execute_0x45, execute_0x46, execute_0x47, execute_0x48, execute_0x4A, execute_0x4B, execute_0x4C, execute_0x4D, execute_0x4E, execute_0x4F)
import Group0x50 exposing (execute_0x50, execute_0x51, execute_0x53, execute_0x54, execute_0x55, execute_0x56, execute_0x57, execute_0x58, execute_0x59, execute_0x5A, execute_0x5C, execute_0x5D, execute_0x5E, execute_0x5F)
import Group0x60 exposing (execute_0x60, execute_0x61, execute_0x62, execute_0x63, execute_0x65, execute_0x66, execute_0x67, execute_0x68, execute_0x69, execute_0x6A, execute_0x6B, execute_0x6C, execute_0x6E, execute_0x6F)
import Group0x70 exposing (execute_0x70, execute_0x71, execute_0x72, execute_0x73, execute_0x74, execute_0x75, execute_0x76_halt, execute_0x77, execute_0x78, execute_0x79, execute_0x7A, execute_0x7B, execute_0x7C, execute_0x7D, execute_0x7E)
import Group0x80 exposing (delta_dict_80, delta_dict_lite_80)
import Group0x90 exposing (delta_dict_90, delta_dict_lite_90)
import Group0xA0 exposing (delta_dict_A0, delta_dict_lite_A0)
import GroupCB exposing (group_cb, group_xy_cb)
import GroupED exposing (group_ed)
import Loop
import Utils exposing (char, shiftLeftBy8, shiftRightBy8, toHexString)
import Z80Debug exposing (debug_todo)
import Z80Delta exposing (DeltaWithChanges, Z80Delta(..), apply_delta, delta_noop)
import Z80Env exposing (Z80Env, add_cpu_time_env, m1, mem16, out, pop, z80_in, z80_push, z80env_constructor)
import Z80Flags exposing (FlagRegisters, IntWithFlags, adc, c_FC, c_FS, cp, get_flags, set_af, z80_add, z80_and, z80_or, z80_sub, z80_xor)
import Z80Ram exposing (c_FRSTART)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), IntWithFlagsTimeAndPC, InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, call_if, get_bc, get_de, get_h, get_l, get_xy, hl_deref_with_z80, imm16, imm8, inc_pc, jp, jp_z80, rst, rst_z80, set_bc_main, set_de_main, set_flag_regs)

constructor: Z80
constructor =
    let
        main = Z80Types.MainWithIndexRegisters 0 0 0 0 0 0 0
        alternate = MainRegisters 0 0 0 0 0
        main_flags = FlagRegisters 0 0 0 0 0
        alt_flags = FlagRegisters 0 0 0 0 0
        interrupts = InterruptRegisters 0 0 0 0 False
    in
        Z80 z80env_constructor 0 main main_flags alternate alt_flags interrupts c_FRSTART
--	int a() {return A;}
--get_a: Z80 -> Int
--get_a z80 =
--    z80.flags.a
--	int f() {return flags();}
--get_f: Z80 -> Int
--get_f z80 =
--    get_flags z80

--	int af() {return A<<8 | flags();}
get_af : Z80 -> Int
get_af z80 =
    or (shiftLeftBy8 z80.flags.a) (get_flags z80.flags)
--
--get_i: Z80 -> Int
--get_i z80 =
--    shiftRightBy8 z80.interrupts.ir
--	int r() {return R&0x7F | IR&0x80;}
--get_r: Z80 -> Int
--get_r z80 =
--    let
--        a = and z80.interrupts.r 0x7F
--        b = and z80.interrupts.ir 0x80
--    in
--        or a b
--	int im() {int v=IM; return v==0?v:v-1;}
--get_im: Z80 -> Int
--get_im z80 =
--    let
--        v = z80.interrupts.iM
--    in
--        if v == 0 then v else v - 1
--	boolean ei() {return (IFF&1)!=0;}
--get_ei: Z80 -> Bool
--get_ei z80 =
--    (Bitwise.and z80.interrupts.iff 1) /= 0  -- slightly odd != operator in Elm
--	void f(int v) {flags(v);}
--set_f: Int -> Int -> FlagRegisters
--set_f v a =
--   set_flags v a
--	void hl(int v) {HL = v;}
--set_hl: Int -> Z80 -> Z80
--set_hl hl z80 =
--    let
--        z80_main = z80.main
--    in
--        { z80 | main = { z80_main | hl = hl } }
--	void ix(int v) {IX = v;}
--set_ix: Z80 -> Int -> Z80
--set_ix z80 ix =
--    { z80 | ix = ix }
--	void iy(int v) {IY = v;}
--set_iy: Z80 -> Int -> Z80
--set_iy z80 iy =
--    { z80 | iy = iy }

--	void pc(int v) {PC = v;}
set_pc: Int -> Z80 -> Z80
set_pc pc z80 =
   let
      -- ignore common routines and LDIR/LDDR and friends (jump back 2)
      --y = if Dict.member pc Z80Rom.c_COMMON_NAMES || pc == z80.pc - 2 then
      --      Nothing
      --    else
      --      let
      --        sub_name = pc |> subName
      --      in
      --        if sub_name|> String.startsWith "CHAN-OPEN" then
      --          debug_log sub_name (z80.flags.a |> toHexString2) Nothing
      --        else if sub_name |> String.startsWith "PRINT-OUT " then
      --           debug_log sub_name (z80.flags.a |> toHexString2) Nothing
      --        else if sub_name |> String.startsWith "PO-CHAR " then
      --           debug_log sub_name ("DE " ++ (z80 |> get_de |> toHexString) ++
      --                                        " HL " ++ (z80.main.hl |> toHexString) ++
      --                                        " BC " ++ (z80 |> get_bc |> toHexString) ++
      --                                        " A " ++ (z80.flags.a |> toHexString2)) Nothing
      --        else if sub_name |> String.startsWith "PR-ALL-3 " then
      --           debug_log sub_name ("DE " ++ (z80 |> get_de |> toHexString) ++
      --                                        " HL " ++ (z80.main.hl |> toHexString) ++
      --                                        " B " ++ (z80.main.b |> toHexString2) ++
      --                                        " C " ++ (z80.main.c |> toHexString2)) Nothing
      --      else
      --          debug_log "set_pc" ("from " ++ (z80.pc |> toHexString) ++
      --                           " to " ++ (pc |> subName) ++
      --                           " (sp " ++ (z80.sp |> toHexString) ++ ")") Nothing
      z80_1 =  { z80 | pc = Bitwise.and pc 0xFFFF }
   in
      z80_1

--	void sp(int v) {SP = v;}
--set_sp: Int -> Z80 -> Z80
--set_sp sp z80 =
--   { z80 | sp = Bitwise.and sp 0xFFFF }

--	void r(int v) {R=v; IR = IR&0xFF00 | v&0x80;}
--	void im(int v) {IM = v+1 & 3;}
--	void iff(int v) {IFF = v;}
set_iff: Int -> Z80 -> InterruptRegisters
set_iff value z80 =
   let
      --y = debug_log "set_iff" value Nothing
      interrupts = z80.interrupts
   in
      { interrupts | iff = value }
--	void ei(boolean v) {IFF = v ? 3 : 0;}
--
--
--	private void adc_hl(int b)
--	{
--		int a,r;
--		r = (a=HL) + b + (Ff>>>8 & FC);
--		Ff = r>>>8;
--		Fa = a>>>8; Fb = b>>>8;
--		HL = r = (char)r;
--		Fr = r>>>8 | r<<8;
--		MP = a+1;
--		time += 7;
--	}
adc_hl: Int -> Z80 -> Z80
adc_hl b z80 =
   let
      a = z80.main.hl
      r1 = a + b + (and (shiftRightBy8 z80.flags.ff) c_FC)
      ff = shiftRightBy8 r1
      fa = shiftRightBy8 a
      fb = shiftRightBy8 b
      r = char r1
      fr = or (shiftRightBy8 r) (shiftLeftBy8 r)
      main = z80.main
      flags = z80.flags
   in
      { z80 | main = { main | hl = r }, flags = { flags | ff = ff, fa = fa, fb = fb, fr = fr} } |> add_cpu_time 7
--
--
--	private void rrd()
--	{
--		int v = env.mem(HL) | A<<8;
--		time += 7;
--		f_szh0n0p(A = A&0xF0 | v&0x0F);
--		env.mem(HL, v>>>4 & 0xFF);
--		MP = HL+1;
--		time += 3;
--	}
--
--
--	private void ld_a_ir(int v)
--	{
--		Ff = Ff&~0xFF | (A = v);
--		Fr = v==0 ? 0 : 1;
--		Fa = Fb = IFF<<6 & 0x80;
--		time++;
--	}
--
--
--	private void cpir(int i, boolean r)
--	{
--		int a,b,v;
--
--		v = A-(b = env.mem(a=HL)) & 0xFF;
--		MP += i;
--		HL = (char)(a+i);
--		time += 8;
--
--		Fr = v & 0x7F | v>>>7;
--		Fb = ~(b | 0x80);
--		Fa = A & 0x7F;
--
--		bc(a = (char)(bc() - 1));
--		if(a!=0) {
--			Fa |= 0x80;
--			Fb |= 0x80;
--			if(r && v!=0) {
--				MP = (PC = (char)(PC-2)) + 1;
--				time += 5;
--			}
--		}
--
--		Ff = Ff&~0xFF | v&~F53;
--		if(((v ^ b ^ A)&FH) != 0) v--;
--		Ff |= v<<4&0x20 | v&8;
--	}
--
--	private void inir_otir(int op) // op: 101rd01o
--	{
--		int bc, hl, d, v;
--
--		hl = (char)(HL + (d = (op&8)==0 ? 1 : -1));
--		bc = B<<8|C;
--		time++;
--		if((op&1)==0) {
--			v = env.in(bc); time += 4;
--			MP = bc+d;
--			bc = (char)(bc-256);
--			env.mem(HL, v); time += 3;
--			d += bc;
--		} else {
--			v = env.mem(HL); time += 3;
--			bc = (char)(bc-256);
--			MP = bc+d;
--			env.out(bc, v); time += 4;
--			d = hl;
--		}
--		d = (d&0xFF) + v;
--		HL = hl;
--		B = (bc >>= 8);
--		if(op>0xB0 && bc>0) {
--			time += 5;
--			PC = (char)(PC-2);
--		}
--		int x = d&7 ^ bc;
--		Ff = bc | (d &= 0x100);
--		Fa = (Fr = bc) ^ 0x80;
--		x = 0x4B3480 >> ((x^x>>>4)&15);
--		Fb = (x^bc) & 0x80 | d>>>4 | (v & 0x80)<<2;
--	}
--
--	/* Note: EI isn't prefix here - interrupt will be acknowledged */
--

--toString: IXIYHL -> String
--toString ixiyhl =
--   case ixiyhl of
--      IX -> "IX"
--      IY -> "IY"
--      HL -> "HL"

execute_ltC0: Int -> IXIYHL -> Z80ROM -> Z80 -> Maybe Z80Delta
execute_ltC0 c ixiyhl rom48k z80 =
   case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
      Just f -> Just (z80 |> f ixiyhl rom48k)
      Nothing ->
         case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
            Just f_without_ixiyhl -> Just (z80 |> f_without_ixiyhl rom48k)
            Nothing -> Nothing



z80_halt: Z80 -> Z80
z80_halt z80 =
   let
      interrupts = z80.interrupts
      n = shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
      z80_1 = if n > 0 then
                 -- turns out env.halt(n, r) just returns n...?
                 { z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
              else
                 z80
    in
      { z80_1 | interrupts = { interrupts | halted = True } }



execute_0xB0: Z80ROM -> Z80 -> Z80Delta
execute_0xB0 _ z80 =
    -- case 0xB0: or(B); break;
    --z80 |> set_flag_regs (z80_or z80.main.b z80.flags)
   z80.flags |> z80_or z80.main.b |> FlagRegs

execute_0xB1: Z80ROM -> Z80 -> Z80Delta
execute_0xB1 _ z80 =
    -- case 0xB1: or(C); break;
   z80.flags |> z80_or z80.main.c |> FlagRegs

execute_0xB2: Z80ROM -> Z80 -> Z80Delta
execute_0xB2 _ z80 =
    -- case 0xB2: or(D); break;
   z80.flags |> z80_or z80.main.d |> FlagRegs

execute_0xB3: Z80ROM -> Z80 -> Z80Delta
execute_0xB3 _ z80 =
    -- case 0xB3: or(E); break;
    --z80 |> set_flag_regs (z80_or z80.main.e z80.flags)
   z80.flags |> z80_or z80.main.e |> FlagRegs

execute_0xB4: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB4 ixiyhl _ z80 =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> z80_or (get_h ixiyhl z80.main) |> FlagRegs

execute_0xB5: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB5 ixiyhl _ z80 =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> z80_or (get_l ixiyhl z80.main) |> FlagRegs

execute_0xB6: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xB6 ixiyhl rom48k z80 =
  -- case 0xB6: or(env.mem(HL)); time+=3; break;
  -- case 0xB6: or(env.mem(getd(xy))); time+=3; break;
  let
    value = z80 |> hl_deref_with_z80 ixiyhl rom48k
    --env_1 = z80.env
  in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_or value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_or value.value) value.pc value.time

execute_0xB7: Z80ROM -> Z80 -> Z80Delta
execute_0xB7 _ z80 =
    -- case 0xB7: or(A); break;
    --z80 |> set_flag_regs (z80_or z80.flags.a z80.flags)
   z80.flags |> z80_or z80.flags.a |> FlagRegs

execute_0xB8: Z80ROM -> Z80 -> Z80Delta
execute_0xB8 _ z80 =
   -- case 0xB8: cp(B); break;
   --z80 |> set_flag_regs (cp z80.main.b z80.flags)
   z80.flags |> cp z80.main.b |> FlagRegs

execute_0xB9: Z80ROM -> Z80 -> Z80Delta
execute_0xB9 _ z80 =
   -- case 0xB9: cp(C); break;
   --z80 |> set_flag_regs (cp z80.main.c z80.flags)
   z80.flags |> cp z80.main.c |> FlagRegs

execute_0xBA: Z80ROM -> Z80 -> Z80Delta
execute_0xBA _ z80 =
   -- case 0xBA: cp(D); break;
   --z80 |> set_flag_regs (cp z80.main.d z80.flags)
   z80.flags |> cp z80.main.d |> FlagRegs

execute_0xBB: Z80ROM -> Z80 -> Z80Delta
execute_0xBB _ z80 =
   -- case 0xBB: cp(E); break;
   --z80 |> set_flag_regs (cp z80.main.e z80.flags)
   z80.flags |> cp z80.main.e |> FlagRegs

execute_0xBC: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xBC ixiyhl _ z80 =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> cp (get_h ixiyhl z80.main) |> FlagRegs

execute_0xBD: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xBD ixiyhl _ z80 =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> cp (get_l ixiyhl z80.main) |> FlagRegs

execute_0xBE: IXIYHL -> Z80ROM -> Z80 -> Z80
execute_0xBE ixiyhl rom48k z80 =
    -- case 0xBE: cp(env.mem(HL)); time+=3; break;
    -- case 0xBE: cp(env.mem(getd(xy))); time+=3; break;
    let
       value = z80 |> hl_deref_with_z80 ixiyhl rom48k
       env_1 = z80.env
    in
       { z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (cp value.value z80.flags)

execute_0xBF: Z80ROM -> Z80 -> Z80Delta
execute_0xBF _ z80 =
    -- case 0xBF: cp(A); break;
    --z80 |> set_flag_regs (cp z80.flags.a z80.flags)
   z80.flags |> cp z80.flags.a |> FlagRegs

lt40_array_lite: Array (Maybe (Z80ROM -> Z80 -> Z80Delta))
lt40_array_lite = makeLiteArray

list0255 = List.range 0 255

z80_to_delta: Maybe (Z80ROM -> Z80 -> Z80) -> Maybe (Z80ROM -> Z80 -> Z80Delta)
z80_to_delta z80func =
    case z80func of
        Just f ->  Just (\rom48k z80  -> Whole (z80 |> f rom48k))
        Nothing -> Nothing

ixiyhl_z80_to_delta: Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80) -> Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
ixiyhl_z80_to_delta z80func =
    case z80func of
        Just f ->  Just (\ixiyhl rom48k z80  -> Whole (z80 |> f ixiyhl rom48k))
        Nothing -> Nothing

mergeFuncList:  Maybe (Z80ROM -> Z80 -> Z80Delta) -> Maybe (Z80ROM -> Z80 -> Z80Delta) -> Maybe (Z80ROM -> Z80 -> Z80Delta)
mergeFuncList afunc bfunc =
    case afunc of
        Just a -> Just a
        Nothing -> case bfunc of
                        Just b -> Just b
                        Nothing -> Nothing

makeLiteArray: Array (Maybe (Z80ROM -> Z80 -> Z80Delta))
makeLiteArray =
    let
       z80_funcs = list0255 |> List.map (\index -> lt40_dict_lite |> Dict.get index |> z80_to_delta)
       delta_funcs = list0255 |> List.map (\index -> lt40_delta_dict_lite |> Dict.get index)
    in
       List.map2 mergeFuncList z80_funcs delta_funcs |> Array.fromList

lt40_array: Array (Maybe ((IXIYHL -> Z80ROM -> Z80 -> Z80Delta)))
lt40_array = makeLt40Array

mergeIxiyFuncList:  Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta) -> Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta) -> Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
mergeIxiyFuncList afunc bfunc =
    case afunc of
        Just a -> Just a
        Nothing -> case bfunc of
                        Just b -> Just b
                        Nothing -> Nothing

lt40_dict: Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80)
lt40_dict = Dict.fromList
    [
          (0xBE, execute_0xBE)
    ]

makeLt40Array: Array (Maybe ((IXIYHL -> Z80ROM -> Z80 -> Z80Delta)))
makeLt40Array =
    let
       z80_funcs = list0255 |> List.map (\index -> lt40_dict |> Dict.get index |> ixiyhl_z80_to_delta)
       delta_funcs = list0255 |> List.map (\index -> lt40_delta_dict |> Dict.get index)
    in
       List.map2 mergeIxiyFuncList z80_funcs delta_funcs |> Array.fromList

lt40_delta_dict_lite: Dict Int (Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict_lite = Dict.fromList
    [
          -- case 0x40: break;
          (0x40, delta_noop),
          (0x41, execute_0x41),
          (0x42, execute_0x42),
          (0x43, execute_0x43),
          (0x47, execute_0x47),
          (0x48, execute_0x48),
          -- case 0x49: break;
          (0x49, delta_noop),
          (0x4A, execute_0x4A),
          (0x4B, execute_0x4B),
          (0x4F, execute_0x4F),
          (0x50, execute_0x50),
          (0x51, execute_0x51),
          (0x52, delta_noop),
          (0x53, execute_0x53),
          (0x57, execute_0x57),
          (0x58, execute_0x58),
          (0x59, execute_0x59),
          (0x5A, execute_0x5A),
          -- case 0x5B: break;
          (0x5B, delta_noop),
          (0x5F, execute_0x5F),
          -- case 0x64: break;
          (0x64, delta_noop),
          -- case 0x6D: break;
          (0x6D, delta_noop),
          -- case 0x76: halt(); break;
          (0x76, execute_0x76_halt),
          (0x78, execute_0x78),
          (0x79, execute_0x79),
          (0x7A, execute_0x7A),
          (0x7B, execute_0x7B),
          -- case 0x7F: break;
          (0x7F, delta_noop),
          (0xB0, execute_0xB0),
          (0xB1, execute_0xB1),
          (0xB2, execute_0xB2),
          (0xB3, execute_0xB3),
          (0xB7, execute_0xB7),
          (0xB8, execute_0xB8),
          (0xB9, execute_0xB9),
          (0xBA, execute_0xBA),
          (0xBB, execute_0xBB),
          (0xBF, execute_0xBF),
          (0xC0, execute_0xC0),
          (0xC1, execute_0xC1),
          (0xC2, execute_0xC2),
          (0xC3, execute_0xC3),
          (0xC4, execute_0xC4),
          (0xC5, execute_0xC5),
          (0xC6, execute_0xC6),
          (0xC7, execute_0xC7),
          (0xC8, retz_0xC8),
          (0xC9, execute_0xC9),
          (0xCA, execute_0xCA),
          (0xCC, execute_0xCC),
          (0xCD, call_0xCD),
          (0xCE, execute_0xCE),
          (0xCF, execute_0xCF),
          (0xD0, execute_0xD0),
          (0xD1, execute_0xD1),
          (0xD2, execute_0xD2),
          (0xD3, execute_0xD3),
          (0xD5, execute_0xD5),
          (0xD6, execute_0xD6),
          (0xD7, execute_0xD7),
          (0xD8, execute_0xD8),
          -- case 0xD9: exx(); break;
          (0xD9, exx),
          (0xDA, execute_0xDA),
          (0xDB, execute_0xDB),
          (0xDD, (\z80 -> group_xy IXIY_IX z80)),
          (0xDF, execute_0xDF),
          (0xE6, execute_0xE6),
          (0xE7, execute_0xE7),
          (0xEB, execute_0xEB),
          (0xED, group_ed),
          (0xEE, execute_0xEE),
          (0xEF, execute_0xEF),
          (0xF1, execute_0xF1),
          (0xF2, execute_0xF2),
          (0xF3, execute_0xF3),
          (0xF5, execute_0xF5),
          (0xF6, execute_0xF6),
          (0xF7, execute_0xF7),
          (0xF8, execute_0xF8),
          (0xFB, execute_0xFB),
          (0xFD, (\z80 -> group_xy IXIY_IY z80))
    ] |> Dict.union delta_dict_lite_00
    |> Dict.union delta_dict_lite_80
    |> Dict.union delta_dict_lite_90
    |> Dict.union delta_dict_lite_A0
    |> Dict.union delta_dict_lite_10
    |> Dict.union delta_dict_lite_20
    |> Dict.union delta_dict_lite_30

lt40_dict_lite: Dict Int (Z80ROM -> Z80 -> Z80)
lt40_dict_lite = Dict.fromList
    [
          (0xF9, execute_0xF9),
          (0xFA, execute_0xFA),
          (0xFE, execute_0xFE),
          (0xFF, execute_0xFF)
    ]

-- case 0xC7:
-- case 0xCF:
-- case 0xD7:
-- case 0xDF:
-- case 0xE7:
-- case 0xEF:
-- case 0xF7:
-- case 0xFF: push(PC); PC=c-199; break;

execute_0xFF: Z80ROM -> Z80 -> Z80
execute_0xFF _ z80 =
    z80 |> rst_z80 0xFF

lt40_delta_dict: Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict = Dict.fromList
    [
          (0x44, execute_0x44),
          (0x45, execute_0x45),
          (0x46, execute_0x46),
          (0x4C, execute_0x4C),
          (0x4D, execute_0x4D),
          (0x4E, execute_0x4E),
          (0x54, execute_0x54),
          (0x55, execute_0x55),
          (0x56, execute_0x56),
          (0x5C, execute_0x5C),
          (0x5D, execute_0x5D),
          (0x5E, execute_0x5E),
          (0x60, execute_0x60),
          (0x61, execute_0x61),
          (0x62, execute_0x62),
          (0x63, execute_0x63),
          (0x65, execute_0x65),
          (0x66, execute_0x66),
          (0x67, execute_0x67),
          (0x68, execute_0x68),
          (0x69, execute_0x69),
          (0x6A, execute_0x6A),
          (0x6B, execute_0x6B),
          (0x6C, execute_0x6C),
          (0x6E, execute_0x6E),
          (0x6F, execute_0x6F),
          (0x70, execute_0x70),
          (0x71, execute_0x71),
          (0x72, execute_0x72),
          (0x73, execute_0x73),
          (0x74, execute_0x74),
          (0x75, execute_0x75),
          (0x77, execute_0x77),
          (0x7C, execute_0x7C),
          (0x7D, execute_0x7D),
          (0x7E, execute_0x7E),
          (0xB4, execute_0xB4),
          (0xB5, execute_0xB5),
          (0xB6, execute_0xB6),
          (0xBC, execute_0xBC),
          (0xBD, execute_0xBD),
          (0xCB, execute_0xCB),
          (0xE1, execute_0xE1),
          (0xE3, execute_0xE3),
          (0xE5, execute_0xE5),
          (0xE9, execute_0xE9)
    ] |> Dict.union delta_dict_00
    |> Dict.union delta_dict_80
    |> Dict.union delta_dict_90
    |> Dict.union delta_dict_A0
    |> Dict.union delta_dict_10
    |> Dict.union delta_dict_20
    |> Dict.union delta_dict_30

execute_0xC0: Z80ROM -> Z80 -> Z80Delta
execute_0xC0 rom48k z80 =
   -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
   let
      env =  z80.env |> add_cpu_time_env 1
   in
      if z80.flags.fr /= 0 then
         let
            result = env |> pop rom48k
         --   env = z80_1.env
         --   --x = debug_log "ret nz" (result.value |> subName) Nothing
         --   z80_2 = { z80_1 | env = { env | time = result.time, sp = result.sp } }
         in
         --   { z80_2 | pc = result.value }
         CpuTimeWithSpAndPc result.time result.sp result.value
      else
         --z80_1
         NoChange

execute_0xC1: Z80ROM -> Z80 -> Z80Delta
execute_0xC1 rom48k z80 =
   -- case 0xC1: v=pop(); B=v>>>8; C=v&0xFF; break;
   let
      v = z80.env |> pop rom48k
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
      --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
   in
      --z80_1 |> set_bc v.value
      MainRegsWithSpAndTime (z80.main |> set_bc_main v.value) v.sp v.time

execute_0xC2: Z80ROM -> Z80 -> Z80Delta
execute_0xC2 rom48k z80 =
  -- case 0xC2: jp(Fr!=0); break;
  --jp_z80 (z80.flags.fr /= 0) z80
  let
     v = z80 |> jp (z80.flags.fr /= 0) rom48k
  in
     CpuTimeWithPc v.time v.pc

execute_0xC3: Z80ROM -> Z80 -> Z80Delta
execute_0xC3 rom48k z80 =
   -- case 0xC3: MP=PC=imm16(); break;
   let
      v = z80 |> imm16 rom48k
      --env = z80.env
      --z80_1 = { z80 | pc = v.pc, env = { env | time = v.time } }
      --y = debug_log "jp" (v.value |> subName) Nothing
   in
      --z80_1 |> set_pc v.value
      CpuTimeWithPc v.time v.value

execute_0xC4: Z80ROM -> Z80 -> Z80Delta
execute_0xC4 rom48k z80 =
      -- case 0xC4: call(Fr!=0); break;
   --call_z80 (z80.flags.fr /= 0) z80
   let
     result = z80 |> call_if (z80.flags.fr /= 0) rom48k
   in
      EnvWithPc result.env result.pc

execute_0xC5: Z80ROM -> Z80 -> Z80Delta
execute_0xC5 _ z80 =
   -- case 0xC5: push(B<<8|C); break;
   --z80 |> push (z80 |> get_bc)
   let
     bc =  (z80 |> get_bc)
     --pushed = z80.env |> z80_push bc
   in
     --{ z80 | env = pushed }
     OnlyPush bc

execute_0xC6: Z80ROM -> Z80 -> Z80Delta
execute_0xC6 rom48k z80 =
   -- case 0xC6: add(imm8()); break;
   let
      v =  z80 |> imm8 rom48k
      --env_1 = z80.env
      --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
      flags = z80.flags |> z80_add v.value
   in
      --{ z80_1 | flags = flags }
      FlagsWithPcAndTime flags v.pc v.time

rst_delta: Int -> Z80 -> Z80Delta
rst_delta value z80  =
    --z80 |> rst_z80 0xC7
   let
      result = z80 |> rst value
   in
     EnvWithPc result.env result.pc

execute_0xC7: Z80ROM -> Z80 -> Z80Delta
execute_0xC7 _ z80 =
    --z80 |> rst_z80 0xC7
   --let
   --   result = z80 |> rst 0xC7
   --in
   --  EnvWithPc result.env result.pc
    z80 |> rst_delta 0xC7

retz_0xC8: Z80ROM -> Z80 -> Z80Delta
retz_0xC8 rom48k z80 =
    -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
   let
      z80_1_time = z80.env.time |> add_cpu_time_time 1
      env = z80.env
   in
      if z80.flags.fr == 0 then
           let
              popped = { env | time = z80_1_time } |> pop rom48k
           in
              --{ z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
              CpuTimeWithSpAndPc popped.time popped.sp popped.value
      else
           --z80_1
         OnlyTime z80_1_time

execute_0xC9: Z80ROM -> Z80 -> Z80Delta
execute_0xC9 rom48k z80 =
    -- case 0xC9: MP=PC=pop(); break;
   let
      a = z80.env |> pop rom48k
      --b = debug_log "ret" (a.value |> subName) Nothing
      --env = z80.env
   in
      --{ z80 | env = { env | time = a.time, sp = a.sp }, pc = a.value }
      CpuTimeWithSpAndPc a.time a.sp a.value

execute_0xCA: Z80ROM -> Z80 -> Z80Delta
execute_0xCA rom48k z80 =
    -- case 0xCA: jp(Fr==0); break;
    --jp_z80 (z80.flags.fr == 0) z80
  --let
  --  result = z80 |> jp (z80.flags.fr == 0)
  --in
  --  CpuTimeWithPc result.time result.pc
  z80 |> jp_delta (z80.flags.fr == 0) rom48k

execute_0xCB: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xCB ixiyhl rom48k z80 =
    case ixiyhl of
        IX -> z80 |> group_xy_cb IXIY_IX rom48k
        IY -> z80 |> group_xy_cb IXIY_IY rom48k
        HL -> z80 |> group_cb rom48k

execute_0xCC: Z80ROM -> Z80 -> Z80Delta
execute_0xCC rom48k z80 =
    -- case 0xCC: call(Fr==0); break;
   --call_z80 (z80.flags.fr == 0) z80
  let
    result = z80 |> call_if (z80.flags.fr == 0) rom48k
  in
    EnvWithPc result.env result.pc

call_0xCD: Z80ROM -> Z80 -> Z80Delta
call_0xCD rom48k z80 =
   -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
   let
      v = z80 |> imm16 rom48k
      --env = z80.env
      --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
      --pushed = { env | time = v.time } |> z80_push v.pc
   in
      --{ z80_1 | env = pushed, pc = v.value }
      --EnvWithPc pushed v.value
      PushWithCpuTimeAndPc v.pc v.time v.value

execute_0xCE: Z80ROM -> Z80 -> Z80Delta
execute_0xCE rom48k z80 =
   -- case 0xCE: adc(imm8()); break;
   let
      v = z80 |> imm8 rom48k
      flags = z80.flags |> adc v.value
      --env_1 = z80.env
   in
      --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
      FlagsWithPcAndTime flags v.pc v.time

execute_0xCF: Z80ROM -> Z80 -> Z80Delta
execute_0xCF _ z80 =
  --let
  --   result = z80 |> rst 0xCF
  --in
  --   EnvWithPc result.env result.pc
  z80 |> rst_delta 0xCF

execute_0xD0: Z80ROM -> Z80 -> Z80Delta
execute_0xD0 rom48k z80 =
  -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
  let
    z80_1_time = z80.env.time |> add_cpu_time_time 1
    --env = z80.env
  in
    if (and z80.flags.ff 0x100) == 0 then
      let
        popped = z80.env |> pop rom48k
        --x = debug_log "ret nc" (popped.value |> subName) Nothing
      in
        --{ z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
        CpuTimeWithSpAndPc popped.time popped.sp popped.value
    else
      --z80_1
      OnlyTime z80_1_time

execute_0xD1: Z80ROM -> Z80 -> Z80Delta
execute_0xD1 rom48k z80 =
   -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
   let
      v = z80.env |> pop rom48k
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
   in
      --z80_1 |> set_de v.value
      MainRegsWithSpAndTime (z80.main |> set_de_main v.value) v.sp v.time

jp_delta: Bool -> Z80ROM -> Z80 -> Z80Delta
jp_delta y rom48k z80 =
  let
    result = z80 |> jp y rom48k
  in
    CpuTimeWithPc result.time result.pc

execute_0xD2: Z80ROM -> Z80 -> Z80Delta
execute_0xD2 rom48k z80 =
   -- case 0xD2: jp((Ff&0x100)==0); break;
   --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) == 0)
  --let
  --  result = z80 |> jp ((Bitwise.and z80.flags.ff 0x100) == 0)
  --in
  --  CpuTimeWithPc result.time result.pc
  z80 |> jp_delta ((Bitwise.and z80.flags.ff 0x100) == 0) rom48k

execute_0xD3: Z80ROM -> Z80 -> Z80Delta
execute_0xD3 rom48k z80 =
  -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
  let
    value = z80 |> imm8 rom48k
    env_1 = z80.env
    env_2 = { env_1 | time = value.time }
    v = Bitwise.or value.value (shiftLeftBy8 z80.flags.a)
    env = out v z80.flags.a env_2 |> add_cpu_time_env 4
  in
    EnvWithPc env value.pc

execute_0xD5: Z80ROM -> Z80 -> Z80Delta
execute_0xD5 _ z80 =
  -- case 0xD5: push(D<<8|E); break;
  --z80 |> push (z80 |> get_de)
  let
    de = z80 |> get_de
    --pushed = z80.env |> z80_push de
  in
    --{ z80 | env = pushed }
    OnlyPush de

execute_0xD6: Z80ROM -> Z80 -> Z80Delta
execute_0xD6 rom48k z80 =
   -- case 0xD6: sub(imm8()); break;
  let
    v = z80 |> imm8 rom48k
    flags = z80.flags |> z80_sub v.value
    --env_1 = z80.env
  in
      --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagsWithPcAndTime flags v.pc v.time

execute_0xD7: Z80ROM -> Z80 -> Z80Delta
execute_0xD7 _ z80 =
    z80 |> rst_delta 0xD7

execute_0xD8: Z80ROM -> Z80 -> Z80Delta
execute_0xD8 rom48k z80 =
  -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
  let
    z80_1_time = z80.env.time |> add_cpu_time_time 1
  in
    if and z80.flags.ff 0x100 /= 0 then
      let
        env = z80.env
        v = { env | time = z80_1_time } |> pop rom48k
      in
        --debug_log "ret c" (v.value |> subName) ret
        --{ z80_1 | env = { env | time = v.time, sp = v.sp }, pc = v.value }
        CpuTimeWithSpAndPc v.time v.sp v.value
    else
        --z80_1
      OnlyTime z80_1_time

exx: Z80ROM -> Z80 -> Z80Delta
exx _ z80 =
    let
        main = z80.main
        alt = z80.alt_main
        new_main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl }
        alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl }
   in
   MainRegsWithAltRegs new_main alt_main
      --{ z80 | main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl },
      --        alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl } }

execute_0xDA: Z80ROM -> Z80 -> Z80Delta
execute_0xDA rom48k z80 =
    -- case 0xDA: jp((Ff&0x100)!=0); break;
    --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) /= 0)
    z80 |> jp_delta ((Bitwise.and z80.flags.ff 0x100) /= 0) rom48k

execute_0xDB: Z80ROM -> Z80 -> Z80Delta
execute_0xDB rom48k z80 =
   -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
   let
      imm8val = z80 |> imm8 rom48k
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = imm8val.time }, pc = imm8val.pc }
      v = or imm8val.value (shiftLeftBy8 z80_1.flags.a)
      a = z80_1.env |> z80_in v
      flags = z80_1.flags
      new_flags = { flags | a = a.value }
   in
      --{ z80_1 | env = a.env, flags = { flags | a = a.value } }
      CpuTimeWithFlagsAndPc imm8val.time new_flags imm8val.pc

execute_0xDF: Z80ROM -> Z80 -> Z80Delta
execute_0xDF _ z80 =
    z80 |> rst_delta 0xDF

execute_0xE1: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE1 ixiyhl rom48k z80 =
   -- case 0xE1: HL=pop(); break;
   -- case 0xE1: xy=pop(); break;
   let
      hl = z80.env |> pop rom48k
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
      main = z80.main
   in
   case ixiyhl of
       IX -> MainRegsWithSpPcAndTime { main | ix = hl.value } hl.sp z80.pc hl.time
       IY -> MainRegsWithSpPcAndTime { main | iy = hl.value } hl.sp z80.pc hl.time
       HL -> MainRegsWithSpAndTime { main | hl = hl.value } hl.sp hl.time

execute_0xE3: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE3 ixiyhl rom48k z80 =
  -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
  -- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
  let
    hl = z80.env |> pop rom48k
    env = z80.env
    z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
    pushed = z80_1.env |> z80_push (z80_1.main |> get_xy ixiyhl) |> add_cpu_time_env 2
    --z80_2 = { z80_1 | env = pushed }
    main = z80_1.main
  in
    case ixiyhl of
      --IX -> { z80_2 | main = { main | ix = v.value } }
      --IY -> { z80_2 | main = { main | iy = v.value } }
      --HL -> { z80_2 | main = { main | hl = v.value } }
       IX -> MainRegsWithEnvAndPc { main | ix = hl.value } pushed z80.pc
       IY -> MainRegsWithEnvAndPc { main | iy = hl.value } pushed z80.pc
       HL -> MainRegsWithEnv { main | hl = hl.value } pushed

execute_0xE5: IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xE5 ixiyhl _ z80 =
   -- case 0xE5: push(HL); break;
   -- case 0xE5: push(xy); break;
   let
       pushed = z80.env |> z80_push (z80.main |> get_xy ixiyhl)
   in
      --{ z80 | env = pushed }
      EnvWithPc pushed z80.pc

execute_0xE6: Z80ROM -> Z80 -> Z80Delta
execute_0xE6 rom48k z80 =
   -- case 0xE6: and(imm8()); break;
   let
      a = z80 |> imm8 rom48k
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc  }
      flags = z80_1.flags |> z80_and a.value
   in
      --{ z80_1 | flags = flags }
      FlagsWithPcAndTime flags a.pc a.time

execute_0xE7: Z80ROM -> Z80 -> Z80Delta
execute_0xE7 _ z80 =
    z80 |> rst_delta 0xE7

execute_0xE9: IXIYHL ->Z80ROM -> Z80 -> Z80Delta
execute_0xE9 ixiyhl _ z80 =
    -- case 0xE9: PC=HL; break;
    -- case 0xE9: PC=xy; break;
    let
       xy = z80.main |> get_xy ixiyhl
      --a = if Dict.member xy Z80Rom.c_COMMON_NAMES then
      --      Nothing
      --    else
      --      debug_log ("JP (" ++ (toString ixiyhl) ++ ")") (xy |> subName) Nothing
    in
       --{ z80 | pc = xy }
       OnlyPc xy

execute_0xEB: Z80ROM -> Z80 -> Z80Delta
execute_0xEB _ z80 =
   -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
   let
      v = z80.main.hl
      de = z80 |> get_de
      --x = debug_log "EX DE,HL" ("DE " ++ (v |> toHexString) ++ " HL " ++ (de |> toHexString)) Nothing
      main = z80.main |> set_de_main v
   in
      --z80 |> set_de v |> set_hl de
      MainRegs { main | hl = de }

execute_0xEE: Z80ROM -> Z80 -> Z80Delta
execute_0xEE rom48k z80 =
   -- case 0xEE: xor(imm8()); break;
   let
      v = z80 |> imm8 rom48k
      --env_1 = z80.env
      --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
      flags = z80.flags |> z80_xor v.value
   in
      --{ z80_1 | flags = flags }
      FlagsWithPcAndTime flags v.pc v.time

execute_0xEF: Z80ROM -> Z80 -> Z80Delta
execute_0xEF _ z80 =
    z80 |> rst_delta 0xEF

execute_0xF1: Z80ROM -> Z80 -> Z80Delta
execute_0xF1 rom48k z80 =
    -- case 0xF1: af(pop()); break;
   let
      v = z80.env |> pop rom48k
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
   in
      --z80_1 |> set_af v.value
      FlagsWithSpTimeAndPc (set_af v.value) v.sp v.time z80.pc

execute_0xF2: Z80ROM -> Z80 -> Z80Delta
execute_0xF2 rom48k z80 =
   -- case 0xF2: jp((Ff&FS)==0); break;
   --z80 |> jp_z80 (Bitwise.and z80.flags.ff c_FS == 0)
   z80 |> jp_delta (Bitwise.and z80.flags.ff c_FS == 0) rom48k

execute_0xF3: Z80ROM -> Z80 -> Z80Delta
execute_0xF3 _ z80 =
   -- case 0xF3: IFF=0; break;
   z80 |> set_iff 0 |> OnlyInterrupts

execute_0xF5: Z80ROM -> Z80 -> Z80Delta
execute_0xF5 _ z80 =
   -- case 0xF5: push(A<<8|flags()); break;
   let
      a = z80 |> get_af
      --pushed = z80.env |> z80_push a
   in
      --{ z80 | env = pushed }
      OnlyPush a

execute_0xF6: Z80ROM -> Z80 -> Z80Delta
execute_0xF6 rom48k z80 =
   -- case 0xF6: or(imm8()); break;
   let
      a = z80 |> imm8 rom48k
      --env_1 = z80.env
      --z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc }
      flags = z80.flags |> z80_or a.value
   in
      --{ z80_1 | flags = flags }
      FlagsWithPcAndTime flags a.pc a.time

execute_0xF7: Z80ROM -> Z80 -> Z80Delta
execute_0xF7 _ z80 =
    z80 |> rst_delta 0xF7

execute_0xF8: Z80ROM -> Z80 -> Z80Delta
execute_0xF8 rom48k z80 =
    -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
    let
       z80_1_time = z80.env.time |> add_cpu_time_time 1
       z80_2 = if (and z80.flags.ff c_FS) /= 0 then
                   let
                       env = z80.env
                       popped = { env | time = z80_1_time } |> pop rom48k
                   in
                       --{ z80 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
                       CpuTimeWithSpAndPc popped.time popped.sp popped.value
               else
                   --z80
                   OnlyTime z80_1_time
    in
       z80_2

execute_0xF9: Z80ROM -> Z80 -> Z80
execute_0xF9 _ z80 =
   -- case 0xF9: SP=HL; time+=2; break;
   let
       env = z80.env
   in
   { z80 | env = { env | sp = z80.main.hl } |> add_cpu_time_env 2 }

execute_0xFA: Z80ROM -> Z80 -> Z80
execute_0xFA rom48k z80 =
   -- case 0xFA: jp((Ff&FS)!=0); break;
   z80 |> jp_z80 (Bitwise.and z80.flags.ff c_FS /= 0) rom48k

execute_0xFB: Z80ROM -> Z80 -> Z80Delta
execute_0xFB _ z80 =
    -- case 0xFB: IFF=3; break;
   z80 |> set_iff 3 |> OnlyInterrupts

execute_0xFE: Z80ROM -> Z80 -> Z80
execute_0xFE rom48k z80 =
   -- case 0xFE: cp(imm8()); break;
   let
      v = z80 |> imm8 rom48k
      flags = z80.flags |> cp v.value
      env_1 = z80.env
   in
      { z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }

--execute_gtc0: Int -> IXIYHL -> Z80 -> Z80Delta
--execute_gtc0 c ixiyhl z80 =
--   case c of
--      -- case 0xC7:
--      -- case 0xCF:
--      -- case 0xD7:
--      -- case 0xDF:
--      -- case 0xE7:
--      -- case 0xEF:
--      -- case 0xF7:
--      -- case 0xFF: push(PC); PC=c-199; break;
--      0xC7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xC7 |> Whole
--      0xCF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xCF |> Whole
--      0xD7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xD7 |> Whole
--      0xDF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xDF |> Whole
--      0xE7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xE7 |> Whole
--      0xEF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xEF |> Whole
--      0xF7 -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xF7 |> Whole
--      0xFF -> z80 |> execute_0xC7CFD7DFE7EFF7FF 0xFF |> Whole
--      ---- case 0xDC: call((Ff&0x100)!=0); break;
--      --0xDC -> z80 |> call (Bitwise.and z80.flags.ff 0x100 /= 0)
--      ---- case 0xF2: jp((Ff&FS)==0); break;
--      --0xF2 -> z80 |> jp (Bitwise.and z80.flags.ff c_FS == 0)
--      ---- case 0xFA: jp((Ff&FS)!=0); break;
--      --0xFA -> z80 |> jp (Bitwise.and z80.flags.ff c_FS /= 0)
--      -- case 0xDA: jp((Ff&0x100)!=0); break;
--      --0xDA -> z80 |> jp ((Bitwise.and z80.flags.ff 0x100) /= 0)
--      -- case 0xCE: adc(imm8()); break;
--      --0xCE -> let
--      --           v = z80 |> imm8
--      --           flags = z80.flags |> adc v.value
--      --        in
--      --           {z80 | pc = v.pc, env = v.env, flags = flags }
--      _ -> debug_todo "execute" (c |> toHexString) z80  |> Whole

execute_delta: Z80ROM -> Z80 -> DeltaWithChanges
execute_delta rom48k tmp_z80 =
   --int v, c = env.m1(PC, IR|R++&0x7F);
   --PC = (char)(PC+1); time += 4;
   --switch(c) {
   let
     interrupts = tmp_z80.interrupts
     c = tmp_z80.env |> m1 tmp_z80.pc (or interrupts.ir (and interrupts.r 0x7F)) rom48k
     env = tmp_z80.env
     old_z80 = { tmp_z80 | env = { env | time = c.time }, interrupts = { interrupts | r = interrupts.r + 1 } }
     new_pc = Bitwise.and (old_z80.pc + 1) 0xFFFF
     new_time = old_z80.env.time |> add_cpu_time_time 4
     z80 = { old_z80 | pc = new_pc } |> add_cpu_time 4
   in
     case z80 |> execute_ltC0 c.value HL rom48k of
       Just a_z80 -> DeltaWithChanges a_z80 interrupts new_pc new_time
       Nothing ->
            --case c.value of
                --0xDD -> DeltaWithChanges (group_xy IXIY_IX z80) interrupts new_pc new_time
                --0xFD -> DeltaWithChanges (group_xy IXIY_IY z80) interrupts new_pc new_time
                --0xED -> DeltaWithChanges (Whole (group_ed z80)) interrupts new_pc new_time
                --0xCD -> DeltaWithChanges (execute_0xCD z80) interrupts new_pc new_time
                --_ ->
         let
           delta = debug_todo "execute" (c.value |> toHexString) z80  |> Whole
         in
           DeltaWithChanges delta interrupts new_pc new_time
-- case 0xD4: call((Ff&0x100)==0); break;
-- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
-- case 0xE2: jp((flags()&FP)==0); break;
-- case 0xE4: call((flags()&FP)==0); break;
-- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
-- case 0xEA: jp((flags()&FP)!=0); break;
-- case 0xEC: call((flags()&FP)!=0); break;
-- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
-- case 0xF4: call((Ff&FS)==0); break;
-- case 0xFC: call((Ff&FS)!=0); break;
-- case 0xDE: sbc(imm8()); break;
-- case 0xF3: IFF=0; break;

execute_instruction: Z80ROM -> Z80 -> Z80
execute_instruction rom48k z80 =
   z80 |> execute_delta rom48k |> apply_delta z80

execute: Z80ROM -> Z80 -> Z80
execute rom48k z80 =
    if z80.interrupts.halted then
        z80_halt z80
    else
        let
            execute_f = execute_instruction rom48k
        in
        Loop.while (\x -> x.time_limit > x.env.time.cpu_time) execute_f z80
--	void execute()
--	{
--		if(halted) {
--			halt();
--			return;
--		}
--		do {
--			int v, c = env.m1(PC, IR|R++&0x7F);
--			PC = (char)(PC+1); time += 4;
--			switch(c) {
--// -------------- >8 main
--// case 0x00: break;
-- case 0x08: ex_af(); break;
-- case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
--	if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
--	PC=(char)v;} break;
--// -------------- >8
--			}
--		} while(time_limit - time > 0);
--	}
--

--	private void group_xy(int c0)
--	{
--		for(;;) {
--			int xy = c0==0xDD ? IX : IY;
--			int v, c = env.m1(PC, IR|R++&0x7F);
--			PC = (char)(PC+1); time += 4;
--			switch(c) {
--// -------------- >8 xy
group_xy: IXIY -> Z80ROM -> Z80 -> Z80Delta
group_xy ixiy rom48k old_z80 =
  let
    c = old_z80.env |> m1 old_z80.pc (or old_z80.interrupts.ir (and old_z80.interrupts.r 0x7F)) rom48k
    intr = old_z80.interrupts
    env = old_z80.env
    z80_1 = { old_z80 | env = { env | time = c.time }, interrupts = { intr | r = intr.r + 1 } }
    new_pc = z80_1 |> inc_pc
    z80 = { z80_1 | pc = new_pc } |> add_cpu_time 4

    ltc0 = case ixiy of
      IXIY_IX -> z80 |> execute_ltC0 c.value IX rom48k
      IXIY_IY -> z80 |> execute_ltC0 c.value IY rom48k
   in
     case ltc0 of
       Just z_z80 -> z_z80
       Nothing ->
             --case c.value of
                --0xCB -> group_xy_cb ixiy z80 |> Whole
                --_ -> debug_todo "group_xy" (c.value |> toHexString) z80 |> Whole
         debug_todo "group_xy" (c.value |> toHexString) z80 |> Whole
                --_ -> case ixiy of
                --        IXIY_IX -> execute_gtc0 c.value IX z80
                --        IXIY_IY -> execute_gtc0 c.value IY z80
--      case c.value of
-- case 0xED: group_ed(); break;
-- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
-- case 0xC2: jp(Fr!=0); break;
-- case 0xC4: call(Fr!=0); break;
-- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
-- case 0xCA: jp(Fr==0); break;
-- case 0xCC: call(Fr==0); break;
-- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
-- case 0xD2: jp((Ff&0x100)==0); break;
-- case 0xD4: call((Ff&0x100)==0); break;
-- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
-- case 0xDA: jp((Ff&0x100)!=0); break;
-- case 0xDC: call((Ff&0x100)!=0); break;
-- case 0xE0: time++; if((flags()&FP)==0) MP=PC=pop(); break;
-- case 0xE2: jp((flags()&FP)==0); break;
-- case 0xE4: call((flags()&FP)==0); break;
-- case 0xE8: time++; if((flags()&FP)!=0) MP=PC=pop(); break;
-- case 0xEA: jp((flags()&FP)!=0); break;
-- case 0xEC: call((flags()&FP)!=0); break;
-- case 0xF0: time++; if((Ff&FS)==0) MP=PC=pop(); break;
-- case 0xF2: jp((Ff&FS)==0); break;
-- case 0xF4: call((Ff&FS)==0); break;
-- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
-- case 0xFA: jp((Ff&FS)!=0); break;
-- case 0xFC: call((Ff&FS)!=0); break;
-- case 0xC1: bc(pop()); break;
-- case 0xC5: push(bc()); break;
-- case 0xD1: de(pop()); break;
-- case 0xD5: push(de()); break;
-- case 0xF1: af(pop()); break;
-- case 0xF5: push(A<<8|flags()); break;
-- case 0xC3: MP=PC=imm16(); break;
-- case 0xC6: add(imm8()); break;
-- case 0xCE: adc(imm8()); break;
-- case 0xD6: sub(imm8()); break;
-- case 0xDE: sbc(imm8()); break;
-- case 0xE6: and(imm8()); break;
-- case 0xEE: xor(imm8()); break;
-- case 0xF6: or(imm8()); break;
-- case 0xFE: cp(imm8()); break;
-- case 0xC9: MP=PC=pop(); break;
-- case 0xCD: call(true); break;
-- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
-- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
-- case 0xEB: v=HL; HL=de(); de(v); break;
-- case 0xF3: IFF=0; break;
-- case 0xFB: IFF=3; break;
-- case 0xF9: SP=xy; time+=2; break;
-- case 0xC7:
-- case 0xCF:
-- case 0xD7:
-- case 0xDF:
-- case 0xE7:
-- case 0xEF:
-- case 0xF7:
-- case 0xFF: push(PC); PC=c-199; break;
--         _ -> z80
--// -------------- >8
--			}
--			if(c0==0xDD) IX = xy; else IY = xy;
--			break;
--		}
--	}
--
im0: Int -> Z80 -> Z80
im0 bus z80 =
    if and bus 0x38 == 0xFF then
        let
            new_pc = bus - 199
        in
            z80 |> set_pc new_pc
    else
        z80

interrupt: Int -> Z80ROM -> Z80 -> Z80
interrupt bus rom48k z80 =
   let
      ints = z80.interrupts
   in
      if (and ints.iff 1) == 0 then
         z80
      else
        let
            --z81 = debug_log "interrupt" "keyboard scan" z80
            new_ints = { ints | iff = 0, halted = False }
            z80_1 = { z80 | interrupts = new_ints }
            pushed = z80_1.env |> z80_push z80_1.pc
            new_z80 = { z80_1 | env = pushed |> add_cpu_time_env 6 }
        in
            case ints.iM of
                0 -> new_z80 |> im0 bus
                1 -> new_z80 |> im0 bus
                2 -> { new_z80 | pc = 0x38 }
                3 -> let
                        new_ir = Bitwise.and ints.ir 0xFF00
                        addr = Bitwise.or new_ir bus
                        env_and_pc = z80.env |> mem16 addr rom48k
                        env = z80.env
                      in
                        { new_z80 | env = { env | time = env_and_pc.time } |> add_cpu_time_env 6, pc = env_and_pc.value }
                _ -> new_z80

--set_env: Z80Env -> Z80 -> Z80
--set_env z80env z80 =
--   { z80 | env = z80env }

--
--	void nmi()
--	{
--		IFF &= 2;
--		halted = false;
--		push(PC);
--		time += 4;
--		PC = 0x66;
--	}
--
--	void reset() {
--		halted = false;
--		PC = IFF = IM = 0;
--		af(SP = 0xFFFF);
--	}
--}

