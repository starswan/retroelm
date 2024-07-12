--
-- $Id$
--
module Z80 exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimePcAndValue, add_cpu_time_time)
import Dict exposing (Dict)
import Loop
import Utils exposing (byte, char, shiftLeftBy8, shiftRightBy8, toHexString, toHexString2)
import Z80Debug exposing (debug_log, debug_todo)
import Z80Delta exposing (DeltaWithChanges, Z80Delta(..), apply_delta)
import Z80Env exposing (Z80Env, add_cpu_time_env, m1, mem, mem16, out, pop, set_mem, set_mem16, z80_in, z80_push, z80env_constructor)
import Z80Flags exposing (FlagRegisters, IntWithFlags, adc, add16, bit, c_F3, c_F5, c_F53, c_FC, c_FS, cp, cpl, daa, dec, get_flags, inc, rot, sbc, scf_ccf, set_flags, shifter, z80_add, z80_and, z80_or, z80_sub, z80_xor)
import Z80Ram exposing (c_FRSTART)
import Z80Rom exposing (subName)
import Z80Types exposing (InterruptRegisters, MainRegisters, MainWithIndexRegisters, ProgramCounter, Z80, call, call_z80, imm16, imm8, jp, jp_z80, rst, rst_z80)

--type alias RegisterSet =
--   {
--      main: MainRegisters,
--      flags: FlagRegisters
--   }
type alias IntWithZ80 =
    {
        value: Int,
        z80: Z80
    }
--type alias EnvWithRegisterAndValue =
--   {
--        register_value: Int,
--        value: Int,
--        env: Z80Env
--   }
type alias EnvWithRegister =
   {
        register_value: Int,
        env: Z80Env
   }

add_cpu_time: Int -> Z80 -> Z80
add_cpu_time value z80 =
   let
      env = z80.env |> add_cpu_time_env value
   in
      { z80 | env = env }

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

get_bc : Z80 -> Int
get_bc z80 =
    z80.main.b |> shiftLeftBy8 |> or z80.main.c

get_de : Z80 -> Int
get_de z80 =
    z80.main.d |> shiftLeftBy8 |> or z80.main.e

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
--	void bc(int v) {C=v&0xFF; B=v>>>8;}
set_bc: Int -> Z80 -> Z80
set_bc v z80 =
    let
        z80_main = z80.main
    in
        { z80 | main = { z80_main | b = shiftRightBy8 v, c = and v 0xFF }}

set_bc_main: Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_bc_main v z80_main =
     { z80_main | b = shiftRightBy8 v, c = and v 0xFF }
--	void de(int v) {E=v&0xFF; D=v>>>8;}
set_de: Int -> Z80 -> Z80
set_de v z80 =
    let
        z80_main = z80.main
    in
        { z80 | main = { z80_main | d = shiftRightBy8 v, e = and v 0xFF } }

set_de_main: Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_de_main v z80_main =
    { z80_main | d = shiftRightBy8 v, e = and v 0xFF }
--	void hl(int v) {HL = v;}
set_hl: Int -> Z80 -> Z80
set_hl hl z80 =
    let
        z80_main = z80.main
    in
        { z80 | main = { z80_main | hl = hl } }
--	void ix(int v) {IX = v;}
--set_ix: Z80 -> Int -> Z80
--set_ix z80 ix =
--    { z80 | ix = ix }
--	void iy(int v) {IY = v;}
--set_iy: Z80 -> Int -> Z80
--set_iy z80 iy =
--    { z80 | iy = iy }

inc_pc: Z80 -> Int
inc_pc z80 =
   Bitwise.and (z80.pc + 1) 0xFFFF

inc_pc2: Z80 -> Int
inc_pc2 z80 =
   Bitwise.and (z80.pc + 2) 0xFFFF

dec_pc2: Z80 -> Z80
dec_pc2 z80 =
   { z80 | pc = Bitwise.and (z80.pc - 2) 0xFFFF }

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

--	void af(int v) {A = v>>>8; flags(v&0xFF);}
set_af: Int -> Z80 -> Z80
set_af v z80 =
    let
        a = shiftRightBy8 v
        flags = Bitwise.and v 0xFF
    in
        { z80 | flags = set_flags flags a }
--	void i(int v) {IR = IR&0xFF | v<<8;}
set_i: Int -> Z80 -> Z80
set_i v z80 =
    let
        ir = Bitwise.or (Bitwise.and z80.interrupts.ir 0xFF) (shiftLeftBy8 v)
        interrupts = z80.interrupts
    in
        { z80 | interrupts = { interrupts | ir = ir } }
--	void r(int v) {R=v; IR = IR&0xFF00 | v&0x80;}
--	void im(int v) {IM = v+1 & 3;}
--	void iff(int v) {IFF = v;}
set_iff: Int -> Z80 -> Z80
set_iff value z80 =
   let
      --y = debug_log "set_iff" value Nothing
      interrupts = z80.interrupts
   in
      { z80 | interrupts = { interrupts | iff = value } }
--	void ei(boolean v) {IFF = v ? 3 : 0;}
--
f_szh0n0p: Int -> Z80 -> Z80
f_szh0n0p r z80 =
   let
      fr = r
      ff = or (and z80.flags.ff (complement 0xFF)) fr
      fa = or r 0x100
      flags = z80.flags
   in
      { z80 | flags = { flags | fr = fr, ff = ff, fa = fa, fb = 0 } }

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
--	private void sbc_hl(int b)
--	{
--		int a,r;
--		r = (a=HL) - b - (Ff>>>8 & FC);
--		Ff = r>>>8;
--		Fa = a>>>8; Fb = ~(b>>>8);
--		HL = r = (char)r;
--		Fr = r>>>8 | r<<8;
--		MP = a+1;
--		time += 7;
--	}
sbc_hl: Int -> Z80 -> Z80
sbc_hl b z80 =
   let
      a = z80.main.hl
      r1 = a - b - (and (shiftRightBy8 z80.flags.ff) c_FC)
      ff = shiftRightBy8 r1
      fa = shiftRightBy8 a
      fb = complement (shiftRightBy8 b)
      r = and r1 0xFFFF
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
--	private void rld()
--	{
--		int v = env.mem(HL)<<4 | A&0x0F;
--		time += 7;
--		f_szh0n0p(A = A&0xF0 | v>>>8);
--		env.mem(HL, v & 0xFF);
--		MP = HL+1;
--		time += 3;
--	}
rld: Z80 -> Z80
rld z80 =
    let
        v_lhs_1 = z80.env |> mem z80.main.hl
        v_rhs = and z80.flags.a 0x0F
        v_lhs = v_lhs_1.value |> (shiftLeftBy 4)
        v = or v_lhs v_rhs
        a1 = and z80.flags.a 0xF0
        new_a = or a1 (shiftRightBy8 v)
        flags = z80.flags
        new_flags = { flags | a = new_a }
        z80_1 = z80 |> f_szh0n0p new_a
        env_0 = z80.env
        env_1 = { env_0 | time = v_lhs_1.time } |> set_mem z80_1.main.hl (and v 0xFF)
    in
        { z80_1 | flags = new_flags, env = env_1 } |> add_cpu_time 10
--
--	private void ld_a_ir(int v)
--	{
--		Ff = Ff&~0xFF | (A = v);
--		Fr = v==0 ? 0 : 1;
--		Fa = Fb = IFF<<6 & 0x80;
--		time++;
--	}
--
--	private void jr()
--	{
--		int pc = PC;
--		byte d = (byte)env.mem(pc); time += 8;
--		MP = PC = (char)(pc+d+1);
--	}
jr: Z80 -> CpuTimeAndPc
jr z80 =
   let
      mempc = mem z80.pc z80.env
      d = byte mempc.value
      --x = Debug.log "jr" ((String.fromInt d.value) ++ " " ++ (String.fromInt (byte d.value)))
   in
      --z80 |> set_env mempc.env |> add_cpu_time 8 |> set_pc (z80.pc + d + 1)
      CpuTimeAndPc (mempc.time |> add_cpu_time_time 8) (Bitwise.and (z80.pc + d + 1) 0xFFFF)
--
--
--	private void ldir(int i, boolean r)
--	{
ldir: Int -> Bool -> Z80 -> Z80
ldir i r z80 =
   let
      --v = env.mem(a = HL); HL = (char)(a+i); time += 3;
      --env.mem(a = de(), v); de((char)(a+i)); time += 5;
      main = z80.main
      a1 = z80.main.hl
      v1 = mem z80.main.hl z80.env
      env_0 = z80.env
      z80_1 = { z80 | env = { env_0 | time = v1.time }, main = { main | hl = char (a1 + i) } } |> add_cpu_time 3
      a2 = z80_1 |> get_de
      env_1 = z80_1.env |> set_mem a2 v1.value
      z80_2 = { z80_1 | env = env_1 } |> set_de (char (a2 + i)) |> add_cpu_time 5

      --if(Fr!=0) Fr = 1; // keep Z
      --v += A;
      --Ff = Ff&~F53 | v&F3 | v<<4&F5;
      fr = if z80_2.flags.fr /= 0 then
              1
           else
              z80_2.flags.fr
      v2 = v1.value + z80_2.flags.a
      ff = or (or (and z80_2.flags.ff (complement c_F53)) (and v2 c_F3)) (and (shiftLeftBy 4 v2) c_F5)
--		bc(a = (char)(bc()-1));
--		v = 0;
--		if(a!=0) {
--			if(r) {
--				time += 5;
--				MP = (PC = (char)(PC-2)) + 1;
--			}
--			v = 0x80;
--		}
--		Fa = Fb = v;
      a = and ((z80_2 |> get_bc) - 1) 0xFFFF
      z80_3 = z80_2 |> set_bc a
      (v, z80_4) = if a /= 0 then
                     if r then
                        (0x80, z80_3 |> dec_pc2 |> add_cpu_time 5)
                     else
                        (0x80, z80_3)
                   else
                        (0, z80_3)
      flags = z80_4.flags
   in
      { z80_4 | flags = { flags | fr = fr, ff = ff, fa = v, fb = v } }
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
-- Think this could be a useful parameter to execute_lt40 to avoid the duplication
-- problem currently being experienced in function group_xy
type IXIYHL = IX | IY | HL
type IXIY = IXIY_IX | IXIY_IY

--toString: IXIYHL -> String
--toString ixiyhl =
--   case ixiyhl of
--      IX -> "IX"
--      IY -> "IY"
--      HL -> "HL"

get_xy: IXIYHL -> MainWithIndexRegisters -> Int
get_xy ixiyhl z80_main =
   case ixiyhl of
      IX -> z80_main.ix
      IY -> z80_main.iy
      HL -> z80_main.hl

get_ixiy_xy: IXIY -> MainWithIndexRegisters -> Int
get_ixiy_xy ixiy z80_main =
   case ixiy of
      IXIY_IX -> z80_main.ix
      IXIY_IY -> z80_main.iy

set_xy: Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy value ixiyhl z80 =
   case ixiyhl of
      IX -> { z80 | ix = value }
      IY -> { z80 | iy = value }
      HL -> { z80 | hl = value }

set_h: Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h value ixiyhl z80 =
   let
      xy = get_xy ixiyhl z80
   in
      set_xy (or (and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80

set_h_z80: Int -> IXIYHL -> Z80 -> Z80
set_h_z80 value ixiyhl z80 =
    let
        main = z80.main |> set_h value ixiyhl
    in
        { z80 | main = main }

set_l: Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l value ixiyhl z80 =
   let
     xy = get_xy ixiyhl z80
   in
     set_xy (or (and xy 0xFF00) value) ixiyhl z80

set_l_z80: Int -> IXIYHL -> Z80 -> Z80
set_l_z80 value ixiyhl z80 =
    let
        main = z80.main |> set_l value ixiyhl
    in
        { z80 | main = main }

execute_ltC0: Int -> IXIYHL -> Z80 -> Maybe Z80Delta
execute_ltC0 c ixiyhl z80 =
   case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
      Just f -> Just (z80 |> f ixiyhl)
      Nothing ->
         case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
            Just f_without_ixiyhl -> Just (z80 |> f_without_ixiyhl)
            Nothing -> Nothing

execute_0x01: Z80 -> Z80Delta
execute_0x01 z80 =
   -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
   let
      v = imm16 z80
      z80main = z80.main |> set_bc_main v.value
   in
      MainRegsWithPcAndCpuTime z80main v.pc v.time

execute_0x02: Z80 -> Z80Delta
execute_0x02 z80 =
  -- case 0x02: MP=(v=B<<8|C)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
  let
     addr = (shiftLeftBy8 z80.main.b) + z80.main.c
  in
     --{ z80 | env = z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 }
     OnlyEnv (z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3)

execute_0x03: Z80 -> Z80Delta
execute_0x03 z80 =
  -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
  let
      z80_main = z80.main
      tmp_c = z80_main.c + 1
      (reg_b, reg_c) = if tmp_c == 256 then
                          ((and (z80_main.b + 1) 0xFF), 0)
                       else
                          (z80_main.b, tmp_c)
  in
      --{ z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
      MainRegsAndCpuTime { z80_main | b = reg_b, c = reg_c } 2

execute_0x04: Z80 -> Z80Delta
execute_0x04 z80 =
   -- case 0x04: B=inc(B); break;
   let
      new_b = inc z80.main.b z80.flags
      z80main = z80.main
   in
      --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
      FlagsWithMain new_b.flags { z80main | b = new_b.value }

execute_0x05: Z80 -> Z80Delta
execute_0x05 z80 =
   -- case 0x05: B=dec(B); break;
   let
      new_b = dec z80.main.b z80.flags
      z80main = z80.main
   in
      --z80 |> set_flag_regs new_b.flags |> set_b new_b.value
      FlagsWithMain new_b.flags { z80main | b = new_b.value }

execute_0x06: Z80 -> Z80Delta
execute_0x06 z80 =
   -- case 0x06: B=imm8(); break;
   let
      new_b = imm8 z80
      z80main = z80.main
   in
      --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
      MainRegsWithPcAndCpuTime { z80main | b = new_b.value } new_b.pc new_b.time

execute_0x07: Z80 -> Z80Delta
execute_0x07 z80 =
   -- case 0x07: rot(A*0x101>>>7); break;
   --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
   FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)))

ex_af: Z80 -> Z80Delta
ex_af z80 =
    --{ z80 | flags = z80.alt_flags, alt_flags = z80.flags }
    FlagsAndAlt z80.alt_flags z80.flags

execute_0x09: IXIYHL -> Z80 -> Z80Delta
execute_0x09 ixiyhl z80 =
   --case 0x09: HL=add16(HL,B<<8|C); break;
   --case 0x09: xy=add16(xy,B<<8|C); break;
   let
      xy = get_xy ixiyhl z80.main
      new_xy = add16 xy (get_bc z80) z80.flags
      new_z80 = set_xy new_xy.value ixiyhl z80.main
   in
      Whole ({ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time)
     -- doing this doesn't allow for DD 09 which adds an extra 1 to PC
     --IXIYMainFlagsCpuTime new_z80.ix new_z80.iy new_z80.main new_xy.flags new_xy.time

execute_0x0A: Z80 -> Z80Delta
execute_0x0A z80 =
   -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
  let
      z80_flags = z80.flags
      z80_main = z80.main
      v = or (shiftLeftBy8 z80_main.b) z80_main.c
      new_a = mem v z80.env
      new_flags = { z80_flags | a = new_a.value }
  in
      --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
      CpuTimeWithFlags (new_a.time |> add_cpu_time_time 3) new_flags

execute_0x0B: Z80 -> Z80Delta
execute_0x0B z80 =
  -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
  let
      z80_main = z80.main
      tmp_c = z80_main.c - 1
      (reg_b, reg_c) = if tmp_c < 0 then
                          ((and (z80_main.b - 1) 0xFF), 0xFF)
                       else
                          (z80_main.b, tmp_c)
  in
      --{ z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
      MainRegsAndCpuTime { z80_main | b = reg_b, c = reg_c } 2

execute_0x0C: Z80 -> Z80Delta
execute_0x0C z80 =
   -- case 0x0C: C=inc(C); break;
   let
      new_c = inc z80.main.c z80.flags
      z80main = z80.main
   in
      --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
      FlagsWithMain new_c.flags { z80main | c = new_c.value }

execute_0x0D: Z80 -> Z80Delta
execute_0x0D z80 =
   -- case 0x0D: C=dec(C); break;
   let
      new_c = dec z80.main.c z80.flags
      z80main = z80.main
   in
      --z80 |> set_flag_regs new_c.flags |> set_c new_c.value
      FlagsWithMain new_c.flags { z80main | c = new_c.value }

execute_0x0E: Z80 -> Z80Delta
execute_0x0E z80 =
    -- case 0x0E: C=imm8(); break;
    let
       z80main = z80.main
       new_c = imm8 z80
    in
       --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
      MainRegsWithPcAndCpuTime { z80main | c = new_c.value } new_c.pc new_c.time

execute_0x0F: Z80 -> Z80Delta
execute_0x0F z80 =
    -- case 0x0F: rot(A*0x80800000>>24); break;
    --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
   FlagRegs (z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)))

execute_0x10: Z80 -> Z80Delta
execute_0x10 z80 =
   --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
   --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
   --PC=(char)v;} break;
   let
      z80_main = z80.main
      z80_1 = z80 |> add_cpu_time 1
      v = z80_1.pc
      mem_value = mem v z80_1.env
      d = byte mem_value.value
      v2 = v + 1
      env_0 = z80.env
      z80_2 = { z80_1 | env = { env_0 | time = mem_value.time |> add_cpu_time_time 3 } }
      b = and (z80_2.main.b - 1) 0xFF
      (z80_3, v3) = if b /= 0 then
                       (z80_2.env.time |> add_cpu_time_time 5, v2 + d)
                    else
                       (z80_2.env.time, v2)
  in
      --{ z80_3 | main = { z80_main | b = b } } |> set_pc v3
      MainRegsWithPcAndCpuTime { z80_main | b = b } v3 z80_3

execute_0x11: Z80 -> Z80Delta
execute_0x11 z80 =
  --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
  let
      v = imm16 z80
      main_regs = z80.main |> set_de_main v.value
  in
    MainRegsWithPcAndCpuTime main_regs v.pc v.time

execute_0x12: Z80 -> Z80Delta
execute_0x12 z80 =
   -- case 0x12: MP=(v=D<<8|E)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
  let
    addr = (shiftLeftBy8 z80.main.d) + z80.main.e
  in
    z80.env |> set_mem addr z80.flags.a |> add_cpu_time_env 3 |> OnlyEnv

execute_0x13: Z80 -> Z80Delta
execute_0x13 z80 =
  -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
  let
      z80_main = z80.main
      tmp_e = z80_main.e + 1
      (reg_d, reg_e) = if tmp_e == 256 then
                          ((and (z80_main.d + 1) 0xFF), 0)
                       else
                          (z80_main.d, tmp_e)
      env_1 = z80.env |> add_cpu_time_env 2
      main_1 = { z80_main | d = reg_d, e = reg_e }
  in
      --{ z80 | env = env_1, main = main_1 }
      MainRegsWithEnv main_1 env_1

execute_0x14: Z80 -> Z80Delta
execute_0x14 z80 =
   -- case 0x14: D=inc(D); break;
   let
      new_d = inc z80.main.d z80.flags
      z80_main = z80.main
      main_1 = { z80_main | d = new_d.value }
   in
      --{ z80 | flags = new_d.flags, main = { z80_main | d = new_d.value } }
      FlagsWithMain new_d.flags main_1

execute_0x15: Z80 -> Z80Delta
execute_0x15 z80 =
    -- case 0x15: D=dec(D); break;
    let
        new_d = dec z80.main.d z80.flags
        z80_main = z80.main
        main_1 = { z80_main | d = new_d.value }
    in
        --{ z80 | flags = new_d.flags, main = main_1 }
        FlagsWithMain new_d.flags main_1

execute_0x16: Z80 -> Z80Delta
execute_0x16 z80 =
    -- case 0x16: D=imm8(); break;
    let
        z80_main = z80.main
        new_d = imm8 z80
        main_1 = { z80_main | d = new_d.value }
    in
        --{ z80 | pc = new_d.pc, env = new_d.env, main = main_1 }
        MainRegsWithPcAndCpuTime main_1 new_d.pc new_d.time

execute_0x17: Z80 -> Z80Delta
execute_0x17 z80 =
   -- case 0x17: rot(A<<1|Ff>>>8&1); break;
   -- { z80 | flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a)
   --                                                                           (Bitwise.and (shiftRightBy8 z80.flags.ff) 1)) }
   let
      flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a) (Bitwise.and (shiftRightBy8 z80.flags.ff) 1))
   in
      FlagRegs flags

execute_0x18: Z80 -> Z80Delta
execute_0x18 z80 =
  -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
  -- This is just an inlined jr() call
  let
     mem_value = mem z80.pc z80.env
     dest = z80.pc + 1 + (byte mem_value.value)
     pc_val = ProgramCounter dest
      --x = if (dest |> subName |> (String.startsWith "CALL-SUB")) then
      --      -- HL still need to be in-directed, so not a subroutine address yet
      --      let
      --         called = z80.env |> mem16 z80.main.hl
      --      in
      --         debug_log "CALL-SUB" ("from " ++ (z80.pc |> toHexString) ++ " to " ++ (called.value |> subName)) Nothing
      --    else
      --      if Dict.member dest Z80Rom.c_COMMON_NAMES then
      --         Nothing
      --      else
      --         debug_log "jr" (dest |> subName) Nothing
  in
     --z80 |> set_pc dest |> add_cpu_time 8
     PcAndCpuTime pc_val 8

execute_0x19: IXIYHL -> Z80 -> Z80Delta
execute_0x19 ixiyhl z80 =
  -- case 0x19: HL=add16(HL,D<<8|E); break;
  -- case 0x19: xy=add16(xy,D<<8|E); break;
  let
     xy = get_xy ixiyhl z80.main
     new_xy = add16 xy (get_de z80) z80.flags
     new_z80 = set_xy new_xy.value ixiyhl z80.main
  in
     --{ z80 | main = new_z80, flags = new_xy.flags} |> add_cpu_time new_xy.time
     FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time

execute_0x1A: Z80 -> Z80Delta
execute_0x1A z80 =
  -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
  let
      z80_main = z80.main
      addr = or (shiftLeftBy8 z80_main.d) z80_main.e
      new_a = mem addr z80.env
      main_flags = z80.flags
      new_flags = { main_flags | a = new_a.value }
      env_1 = new_a.time |> add_cpu_time_time 3
  in
      --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
      CpuTimeWithFlags env_1 new_flags

execute_0x1B: Z80 -> Z80Delta
execute_0x1B z80 =
  -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
  let
      z80_main = z80.main
      tmp_e = z80_main.e - 1
      (reg_d, reg_e) = if tmp_e < 0 then
                          ((and (z80_main.d - 1) 0xFF), 0xFF)
                       else
                          (z80_main.d, tmp_e)
      main_1 = { z80_main | d = reg_d, e = reg_e }
  in
      --{ z80 | main = main_1 } |> add_cpu_time 2
      MainRegsAndCpuTime main_1 2

execute_0x1C: Z80 -> Z80Delta
execute_0x1C z80 =
   -- case 0x1C: E=inc(E); break;
   let
      z80_main = z80.main
      new_e = inc z80.main.e z80.flags
      main_1 = { z80_main | e = new_e.value }
   in
      --{ z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
      FlagsWithMain new_e.flags main_1

execute_0x1D: Z80 -> Z80Delta
execute_0x1D z80 =
    -- case 0x1D: E=dec(E); break;
    let
       z80_main = z80.main
       new_e = dec z80.main.c z80.flags
       main_1 = { z80_main | e = new_e.value }
    in
       --{ z80 | flags = new_e.flags, main = main_1 }
       FlagsWithMain new_e.flags main_1

execute_0x1E: Z80 -> Z80Delta
execute_0x1E z80 =
   -- case 0x1E: E=imm8(); break;
   let
      z80_main = z80.main
      new_e = imm8 z80
      main_1 = { z80_main | e = new_e.value }
   in
      --{ z80 | env = new_e.env, pc = new_e.pc, main = main_1 }
      MainRegsWithPcAndCpuTime main_1 new_e.pc new_e.time

execute_0x1F: Z80 -> Z80Delta
execute_0x1F z80 =
   -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
   --{ z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201)
   --                                                                           (Bitwise.and z80.flags.ff 0x100))) }
   let
      flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201) (Bitwise.and z80.flags.ff 0x100)))
   in
      FlagRegs flags

execute_0x20: Z80 -> Z80Delta
execute_0x20 z80 =
   -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
   if z80.flags.fr /= 0 then
      let
        x = jr z80
      in
        --{ z80 | pc = x.register_value, env = x.env }
        CpuTimeWithPc x.time x.pc
   else
      let
          x = imm8 z80
      in
         --{ z80 | env = x.env, pc = x.pc }
        CpuTimeWithPc x.time x.pc

execute_0x21: IXIYHL -> Z80 -> Z80Delta
execute_0x21 ixiyhl z80 =
  -- case 0x21: HL=imm16(); break;
  -- case 0x21: xy=imm16(); break;
  let
     new_xy = imm16 z80
     --z80_1 = { z80 | env = new_xy.env, pc = new_xy.pc }
     --x = debug_log ("LD " ++ (ixiyhl |> toString) ++ "," ++ (new_xy.value |> toHexString)) ("pc = " ++ (z80.pc |> toHexString)) Nothing
     main = z80.main |> set_xy new_xy.value ixiyhl
  in
     --{ z80_1 | main = main }
    MainRegsWithPcAndCpuTime main new_xy.pc new_xy.time

execute_0x22: Z80 -> Z80Delta
execute_0x22 z80 =
  -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
  let
     v = imm16 z80
     --new_z80 = { z80 | pc = v.pc }
     env = z80.env |> set_mem16 v.value z80.main.hl |> add_cpu_time_env 6
     --x = debug_log "LD nn, HL" ((z80.pc |> toHexString) ++ " addr " ++ (v.value |> toHexString) ++ " " ++ (new_z80.main.hl |> toHexString)) env
  in
     --{ new_z80 | env = env }
     EnvWithPc env v.pc

execute_0x23: IXIYHL -> Z80 -> Z80Delta
execute_0x23 ixiyhl z80 =
  -- case 0x23: HL=(char)(HL+1); time+=2; break;
  -- case 0x23: xy=(char)(xy+1); time+=2; break;
  let
    xy = z80.main |> get_xy ixiyhl
    --x = if z80.pc /= 0x11E7 then
    --        debug_log "INC HL" (z80.pc |> toHexString) Nothing
    --    else
    --        Nothing
    main = z80.main |> set_xy (char (xy + 1)) ixiyhl
  in
    --{ z80 | main = main } |> add_cpu_time 2
    MainRegsWithPcAndCpuTime main z80.pc (z80.env.time |> add_cpu_time_time 2)

execute_0x24: IXIYHL -> Z80 -> Z80Delta
execute_0x24 ixiyhl z80 =
   -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
   -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
   let
      xy = get_xy ixiyhl z80.main
      value = inc (shiftRightBy8 xy) z80.flags
      --z80_1 = { z80 | flags = value.flags }
      new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
      main = set_xy new_xy ixiyhl z80.main
   in
      --{ z80_1 | main = main }
    FlagsWithPCMainAndTime value.flags z80.pc main 0

execute_0x25: IXIYHL -> Z80 -> Z80Delta
execute_0x25 ixiyhl z80 =
   -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
   -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
   let
        xy = get_xy ixiyhl z80.main
        value = dec (shiftRightBy8 xy) z80.flags
        z80_1 = { z80 | flags = value.flags }
        new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
        main = set_xy new_xy ixiyhl z80_1.main
   in
      --{ z80_1 | main = main }
      FlagsWithPCMainAndTime value.flags z80.pc main 0

execute_0x26: IXIYHL -> Z80 -> Z80Delta
execute_0x26 ixiyhl z80 =
   -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
   -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
   let
     value = imm8 z80
     --new_z80 = { z80 | env = value.env, pc = value.pc }
     xy = get_xy ixiyhl z80.main
     new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
     main = set_xy new_xy ixiyhl z80.main
   in
      --{ new_z80 | main = main }
      MainRegsWithPcAndCpuTime main value.pc value.time

execute_0x27: Z80 -> Z80Delta
execute_0x27 z80 =
   -- case 0x27: daa(); break;
   --{ z80 | flags = daa z80.flags }
   z80.flags |> daa |> FlagRegs

execute_0x28: Z80 -> Z80Delta
execute_0x28 z80 =
   -- case 0x28: if(Fr==0) jr(); else imm8(); break;
   if z80.flags.fr == 0 then
      let
         x = jr z80
      in
         --{ z80 | env = x.env, pc = x.register_value }
         CpuTimeWithPc x.time x.pc
   else
      let
          x = imm8 z80
      in
         --{ z80 | env = x.env, pc = x.pc }
         CpuTimeWithPc x.time x.pc

execute_0x29: IXIYHL -> Z80 -> Z80Delta
execute_0x29 ixiyhl z80 =
  -- case 0x29: HL=add16(HL,HL); break;
  -- case 0x29: xy=add16(xy,xy); break;
  let
     xy = get_xy ixiyhl z80.main
     new_xy = add16 xy xy z80.flags
     new_z80 = set_xy new_xy.value ixiyhl z80.main
  in
     --{ z80 | main = new_z80, flags = new_xy.flags } |> add_cpu_time new_xy.time
     FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time

execute_0x2A: IXIYHL -> Z80 -> Z80Delta
execute_0x2A ixiyhl z80 =
  -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
  -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
  let
     v = imm16 z80
     --z80_1 = { z80 | pc = v.pc }
     new_xy = z80.env |> mem16 v.value
     --z80_2 = { z80_1 | env = new_xy.env }
     main = z80.main |> set_xy new_xy.value ixiyhl
  in
     --{ z80_2 | main = main } |> add_cpu_time 6
     MainRegsWithPcAndCpuTime main v.pc (new_xy.time |> add_cpu_time_time 6)

execute_0x2B: IXIYHL -> Z80 -> Z80Delta
execute_0x2B ixiyhl z80 =
  -- case 0x2B: HL=(char)(HL-1); time+=2; break;
  -- case 0x2B: xy=(char)(xy-1); time+=2; break;
  let
    xy = get_xy ixiyhl z80.main
    new_xy = and (xy - 1) 0xFFFF
    new_z80 = set_xy new_xy ixiyhl z80.main
  in
    --{ z80 | main = new_z80 } |> add_cpu_time 2
    MainRegsWithPcAndCpuTime new_z80 z80.pc (z80.env.time |> add_cpu_time_time 2)

execute_0x2C: IXIYHL -> Z80 -> Z80Delta
execute_0x2C ixiyhl z80 =
   -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
   -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
   let
        z80_flags = z80.flags
        xy = get_xy ixiyhl z80.main
        h = and xy 0xFF00
        l = inc (and xy 0xFF) z80_flags
        --z80_1 = { z80 | flags = l.flags }
        new_xy = or h l.value
        main = set_xy new_xy ixiyhl z80.main
   in
      --{ z80_1 | main = main }
      FlagsWithPCMainAndTime l.flags z80.pc main 0

execute_0x2D: IXIYHL -> Z80 -> Z80Delta
execute_0x2D ixiyhl z80 =
  -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
  -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
  let
    z80_flags = z80.flags
    xy = get_xy ixiyhl z80.main
    h = and xy 0xFF00
    l = dec (and xy 0xFF) z80_flags
    --new_z80 = { z80 | flags = l.flags }
    new_xy = or h l.value
    main = set_xy new_xy ixiyhl z80.main
  in
      --{ new_z80 | main = main }
      FlagsWithPCMainAndTime l.flags z80.pc main 0

execute_0x2E: IXIYHL -> Z80 -> Z80Delta
execute_0x2E ixiyhl z80 =
  -- case 0x2E: HL=HL&0xFF00|imm8(); break;
  -- case 0x2E: xy=xy&0xFF00|imm8(); break;
  let
     xy = get_xy ixiyhl z80.main
     h = and xy 0xFF00
     l = imm8 z80
     --new_z80 = { z80 | env = l.env, pc = l.pc }
     new_xy = or h l.value
     main = set_xy new_xy ixiyhl z80.main
  in
      --{ new_z80 | main = main }
     MainRegsWithPcAndCpuTime main l.pc l.time

execute_0x2F: Z80 -> Z80Delta
execute_0x2F z80 =
   -- case 0x2F: cpl(); break;
   --{ z80 | flags = cpl z80.flags }
   z80.flags |> cpl |> FlagRegs

execute_0x30: Z80 -> Z80Delta
execute_0x30 z80 =
   -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
   if (and z80.flags.ff 0x100) == 0 then
      let
         x = jr z80
      in
         --{ z80 | env = x.env, pc = x.register_value }
         CpuTimeWithPc x.time x.pc
   else
      let
         v = imm8 z80
      in
         --{ z80 | env = v.env, pc = v.pc }
         CpuTimeWithPc v.time v.pc

execute_0x31: Z80 -> Z80Delta
execute_0x31 z80 =
  -- case 0x31: SP=imm16(); break;
  let
      v = imm16 z80
  in
      --{ z80 | env = v.env, pc = v.pc, sp = v.value }
      CpuTimeWithSpAndPc v.time v.value v.pc

execute_0x32: Z80 -> Z80Delta
execute_0x32 z80 =
   -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
   let
      v = imm16 z80
      --new_z80 = { z80 | pc = v.pc }
   in
      EnvWithPc (z80.env |> set_mem v.value z80.flags.a |> add_cpu_time_env 3) v.pc
      --{ new_z80 | env = v.env |> set_mem v.value new_z80.flags.a } |> add_cpu_time 3

execute_0x33: Z80 -> Z80Delta
execute_0x33 z80 =
   -- case 0x33: SP=(char)(SP+1); time+=2; break;
   let
       new_sp = Bitwise.and (z80.env.sp + 1) 0xFFFF
   in
       SpAndCpuTime new_sp 2
      --{ z80 | sp = new_sp } |> add_cpu_time 2

execute_0x34: IXIYHL -> Z80 -> Z80Delta
execute_0x34 ixiyhl z80 =
  -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
  -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
  let
     z80_flags = z80.flags
     a = env_mem_hl ixiyhl z80
     env_1 = z80.env
     env_2 = { env_1 | time = a.time }
     value = mem a.value env_2
     v = z80_flags |> inc value.value
     --z80_1 = { z80 | pc = a.pc } |> add_cpu_time 4
     new_env = { env_2 | time = value.time } |> add_cpu_time_env 4 |> set_mem a.value v.value
  in
     --{ z80_1 | env = new_env, flags = v.flags } |> add_cpu_time 3
    EnvWithFlagsAndPc (new_env |> add_cpu_time_env 3) v.flags a.pc

execute_0x35: IXIYHL -> Z80 -> Z80Delta
execute_0x35 ixiyhl z80 =
  -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
  -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
   let
      a = env_mem_hl ixiyhl z80
      --z80_1 = { z80 | pc = a.pc }
      value = mem a.value z80.env
      env_1 = z80.env
      v = z80.flags |> dec value.value
      new_env = { env_1 | time = value.time } |> add_cpu_time_env 4 |> set_mem a.value v.value
      env_2 = new_env |> add_cpu_time_env 3
   in
      --{ z80_1 | env = env_2, flags = v.flags }
      EnvWithFlagsAndPc env_2 v.flags a.pc

execute_0x36: IXIYHL -> Z80 -> Z80Delta
execute_0x36 ixiyhl z80 =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    --	v=env.mem((char)(PC+1)); time+=5;
    --	env.mem(a,v); PC=(char)(PC+2); time+=3;} break;
    case ixiyhl of
       HL -> let
                v = imm8 z80
                env_1 = z80.env
                new_z80 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
                new_env = set_mem z80.main.hl v.value new_z80.env
             in
                --{ new_z80 | env = new_env }
                EnvWithPc new_env v.pc
       _ -> let
                xy = get_xy ixiyhl z80.main
                mempc = mem z80.pc z80.env
                env_1 = z80.env
                a = char (xy + byte mempc.value)
                --z80_1 = { z80 | env = mempc.env } |> add_cpu_time 3
                z80_1_env = { env_1 | time = mempc.time } |> add_cpu_time_env 3
                v = mem (char (z80.pc + 1)) z80_1_env
                z80_2 = z80_1_env |> add_cpu_time_env 5
                x = set_mem a v.value z80_2
                new_pc = Bitwise.and (z80.pc + 2) 0xFFFF
             in
                --{ z80_2 | env = x, pc = new_pc } |> add_cpu_time 3
                EnvWithPc (x |> add_cpu_time_env 3) new_pc

execute_0x37: Z80 -> Z80Delta
execute_0x37 z80 =
   -- case 0x37: scf_ccf(0); break;
   --{ z80 | flags = z80.flags |> scf_ccf 0 }
   z80.flags |> scf_ccf 0 |> FlagRegs

execute_0x38: Z80 -> Z80Delta
execute_0x38 z80 =
   -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
   if (and z80.flags.ff 0x100) /= 0 then
      let
         x = jr z80
      in
         --{ z80 | env = x.env, pc = x.register_value }
         CpuTimeWithPc x.time x.pc
   else
      let
         v = imm8 z80
      in
         --{ z80 | env = v.env, pc = v.pc }
         CpuTimeWithPc v.time v.pc

execute_0x39: IXIYHL -> Z80 -> Z80Delta
execute_0x39 ixiyhl z80 =
  --case 0x39: HL=add16(HL,SP); break;
  --case 0x39: xy=add16(xy,SP); break;
  let
     xy = get_xy ixiyhl z80.main
     new_xy = add16 xy z80.env.sp z80.flags
     new_z80 = set_xy new_xy.value ixiyhl z80.main
  in
     --{ z80 | main = new_z80, flags = new_xy.flags }  |> add_cpu_time new_xy.time
     FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time

execute_0x3A: Z80 -> Z80Delta
execute_0x3A z80 =
  -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
  let
     z80_flags = z80.flags
     v = imm16 z80
     --new_z80 = { z80 | pc = v.pc }
     mem_value = z80.env |> mem v.value
  in
     --{ new_z80 | flags = { z80_flags | a = mem_value.value }, env = mem_value.env |> add_cpu_time_env 3 }
    CpuTimeWithFlagsAndPc (mem_value.time |> add_cpu_time_time 3) { z80_flags | a = mem_value.value } v.pc

execute_0x3B: Z80 -> Z80Delta
execute_0x3B z80 =
   -- case 0x3B: SP=(char)(SP-1); time+=2; break;
   let
      new_sp = Bitwise.and (z80.env.sp - 1) 0xFFFF
   in
      --{ z80 | sp = new_sp } |> add_cpu_time 2
      SpAndCpuTime new_sp 2

execute_0x3C: Z80 -> Z80Delta
execute_0x3C z80 =
   -- case 0x3C: A=inc(A); break;
   let
      v = inc z80.flags.a z80.flags
      new_flags = v.flags
   in
      --{ z80 | flags = { new_flags | a = v.value } }
      FlagRegs { new_flags | a = v.value }

execute_0x3D: Z80 -> Z80Delta
execute_0x3D z80 =
   -- case 0x3D: A=dec(A); break;
   let
      v = dec z80.flags.a z80.flags
      new_flags = v.flags
   in
      --{ z80 | flags = { new_flags | a = v.value } }
      FlagRegs { new_flags | a = v.value }

execute_0x3E: Z80 -> Z80Delta
execute_0x3E z80 =
   -- case 0x3E: A=imm8(); break;
   let
      z80_flags = z80.flags
      v = imm8 z80
      --new_z80 = { z80 | env = v.env, pc = v.pc }
   in
      --{ new_z80 | flags = { z80_flags | a = v.value } }
      CpuTimeWithFlagsAndPc v.time { z80_flags | a = v.value } v.pc

execute_0x3F: Z80 -> Z80Delta
execute_0x3F z80 =
   -- case 0x3F: scf_ccf(Ff&0x100); break;
   --{ z80 | flags = z80.flags |> scf_ccf (and z80.flags.ff 0x100) }
      z80.flags |> scf_ccf (and z80.flags.ff 0x100) |> FlagRegs

delta_noop: Z80 -> Z80Delta
delta_noop z80 = NoChange

--noop: Z80 -> Z80
--noop z80 = z80

execute_0x41: Z80 -> Z80Delta
execute_0x41 z80 =
   -- case 0x41: B=C; break;
   --z80 |> set_b z80.main.c
   let
       main = z80.main
   in
       { main | b = main.c } |> MainRegs

execute_0x42: Z80 -> Z80Delta
execute_0x42 z80 =
   -- case 0x42: B=D; break;
   --z80 |> set_b z80.main.d
   let
       main = z80.main
   in
       { main | b = main.d } |> MainRegs

execute_0x43: Z80 -> Z80Delta
execute_0x43 z80 =
   -- case 0x43: B=E; break;
   --z80 |> set_b z80.main.e
   let
       main = z80.main
   in
       { main | b = main.e } |> MainRegs

execute_0x44: IXIYHL -> Z80 -> Z80Delta
execute_0x44 ixiyhl z80 =
    -- case 0x44: B=HL>>>8; break;
    -- case 0x44: B=xy>>>8; break;
    --z80 |> set_b (get_h ixiyhl z80.main)
   let
       main = z80.main
   in
       MainRegsWithPc { main | b = (get_h ixiyhl z80.main) } z80.pc

execute_0x45: IXIYHL -> Z80 -> Z80Delta
execute_0x45 ixiyhl z80 =
  -- case 0x45: B=HL&0xFF; break;
  -- case 0x45: B=xy&0xFF; break;
  --  z80 |> set_b (get_l ixiyhl z80.main)
   let
       main = z80.main
   in
       MainRegsWithPc { main | b = (get_l ixiyhl z80.main) } z80.pc

execute_0x46: IXIYHL -> Z80 -> Z80Delta
execute_0x46 ixiyhl z80 =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
       value = hl_deref_with_z80 ixiyhl z80
       main = z80.main
    in
       --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
       MainRegsWithPcAndCpuTime { main | b = value.value } value.pc value.time

execute_0x47: Z80 -> Z80Delta
execute_0x47 z80 =
   -- case 0x47: B=A; break;
   --z80 |> set_b z80.flags.a
   let
       main = z80.main
   in
       { main | b = z80.flags.a } |> MainRegs

execute_0x48: Z80 -> Z80Delta
execute_0x48 z80 =
    -- case 0x48: C=B; break;
    --z80 |> set_c z80.main.b
   let
       main = z80.main
   in
       { main | c = z80.main.b } |> MainRegs

execute_0x4A: Z80 -> Z80Delta
execute_0x4A z80 =
    -- case 0x4A: C=D; break;
    --z80 |> set_c z80.main.d
   let
       main = z80.main
   in
      { main | c = z80.main.d } |> MainRegs

execute_0x4B: Z80 -> Z80Delta
execute_0x4B z80 =
    -- case 0x4B: C=E; break;
    --z80 |> set_c z80.main.e
   let
       main = z80.main
   in
      { main | c = z80.main.e } |> MainRegs

execute_0x4C: IXIYHL -> Z80 -> Z80Delta
execute_0x4C ixiyhl z80 =
    -- case 0x4C: C=HL>>>8; break;
    --z80 |> set_c (get_h ixiyhl z80.main)
   let
       main = z80.main
   in
       MainRegsWithPc { main | c = (get_h ixiyhl z80.main) } z80.pc

execute_0x4D: IXIYHL -> Z80 -> Z80Delta
execute_0x4D ixiyhl z80 =
    -- case 0x4D: C=HL&0xFF; break;
    --z80 |> set_c (get_l ixiyhl z80.main)
   let
       main = z80.main
   in
      { main | c = (get_l ixiyhl z80.main) } |> MainRegs

execute_0x4E: IXIYHL -> Z80 -> Z80Delta
execute_0x4E ixiyhl z80 =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
       value = hl_deref_with_z80 ixiyhl z80
       main = z80.main
    in
       --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
       MainRegsWithPcAndCpuTime { main | c = value.value} value.pc value.time

execute_0x4F: Z80 -> Z80Delta
execute_0x4F z80 =
    -- case 0x4F: C=A; break;
    --z80 |> set_c z80.flags.a
    let
       main = z80.main
    in
       { main | c = z80.flags.a } |> MainRegs

execute_0x50: Z80 -> Z80Delta
execute_0x50 z80 =
    -- case 0x50: D=B; break;
    --z80 |> set_d z80.main.b
   let
       main = z80.main
   in
      { main | d = z80.main.b } |> MainRegs

execute_0x51: Z80 -> Z80Delta
execute_0x51 z80 =
    -- case 0x51: D=C; break;
    --z80 |> set_d z80.main.c
   let
       main = z80.main
   in
      { main | d = z80.main.c } |> MainRegs

execute_0x53: Z80 -> Z80Delta
execute_0x53 z80 =
    -- case 0x53: D=E; break;
    --z80 |> set_d z80.main.e
   let
       main = z80.main
   in
      { main | d = z80.main.e } |> MainRegs

execute_0x54: IXIYHL -> Z80 -> Z80Delta
execute_0x54 ixiyhl z80 =
    -- case 0x54: D=HL>>>8; break;
    --z80 |> set_d (get_h ixiyhl z80.main)
   let
       main = z80.main
   in
      { main | d = (get_h ixiyhl z80.main) } |> MainRegs

execute_0x55: IXIYHL -> Z80 -> Z80Delta
execute_0x55 ixiyhl z80 =
    -- case 0x55: D=HL&0xFF; break;
    --z80 |> set_d (get_l ixiyhl z80.main)
   let
       main = z80.main
   in
      { main | d = (get_l ixiyhl z80.main) } |> MainRegs

execute_0x56: IXIYHL -> Z80 -> Z80Delta
execute_0x56 ixiyhl z80 =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
       main = z80.main
       value = hl_deref_with_z80 ixiyhl z80
    in
       --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
       MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time

execute_0x57: Z80 -> Z80Delta
execute_0x57 z80 =
   -- case 0x57: D=A; break;
   --z80 |> set_d z80.flags.a
   let
       main = z80.main
   in
      { main | d = z80.flags.a } |> MainRegs

execute_0x58: Z80 -> Z80Delta
execute_0x58 z80 =
  -- case 0x58: E=B; break;
  --z80 |> set_e z80.main.b
   let
       main = z80.main
   in
      { main | e = z80.main.b } |> MainRegs

execute_0x59: Z80 -> Z80Delta
execute_0x59 z80 =
  -- case 0x59: E=C; break;
  --z80 |> set_e z80.main.c
   let
       main = z80.main
   in
      { main | e = z80.main.c } |> MainRegs

execute_0x5A: Z80 -> Z80Delta
execute_0x5A z80 =
   -- case 0x5A: E=D; break;
   --z80 |> set_e z80.main.d
   let
       main = z80.main
   in
      { main | e = z80.main.d } |> MainRegs

execute_0x5C: IXIYHL -> Z80 -> Z80Delta
execute_0x5C ixiyhl z80 =
   -- case 0x5C: E=HL>>>8; break;
   --z80 |> set_e (get_h ixiyhl z80.main)
   let
       main = z80.main
   in
      { main | e = (get_h ixiyhl z80.main) } |> MainRegs

execute_0x5D: IXIYHL -> Z80 -> Z80Delta
execute_0x5D ixiyhl z80 =
   -- case 0x5D: E=HL&0xFF; break;
   --z80 |> set_e (get_l ixiyhl z80.main)
   let
       main = z80.main
   in
      { main | e = (get_l ixiyhl z80.main) } |> MainRegs

execute_0x5E: IXIYHL -> Z80 -> Z80Delta
execute_0x5E ixiyhl z80 =
   -- case 0x5E: E=env.mem(HL); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      main = z80.main
   in
      --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
      MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time

execute_0x5F: Z80 -> Z80Delta
execute_0x5F z80 =
   -- case 0x5F: E=A; break;
   --z80 |> set_e z80.flags.a
   let
       main = z80.main
   in
      { main | e = z80.flags.a } |> MainRegs

execute_0x60: IXIYHL -> Z80 -> Z80Delta
execute_0x60 ixiyhl z80 =
   -- case 0x60: HL=HL&0xFF|B<<8; break;
   -- case 0x60: xy=xy&0xFF|B<<8; break;
   --z80 |> set_h_z80 z80.main.b ixiyhl
   let
      value = z80.main |> set_h z80.main.b ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x61: IXIYHL -> Z80 -> Z80Delta
execute_0x61 ixiyhl z80 =
   -- case 0x61: HL=HL&0xFF|C<<8; break;
   -- case 0x61: xy=xy&0xFF|C<<8; break;
   --z80 |> set_h_z80 z80.main.c ixiyhl
   let
      value = z80.main |> set_h z80.main.c ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x62: IXIYHL -> Z80 -> Z80Delta
execute_0x62 ixiyhl z80 =
   -- case 0x62: HL=HL&0xFF|D<<8; break;
   -- case 0x62: xy=xy&0xFF|D<<8; break;
   --z80 |> set_h_z80 z80.main.d ixiyhl
   let
      value = z80.main |> set_h z80.main.d ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x63: IXIYHL -> Z80 -> Z80Delta
execute_0x63 ixiyhl z80 =
   -- case 0x63: HL=HL&0xFF|E<<8; break;
   -- case 0x63: xy=xy&0xFF|E<<8; break;
   --z80 |> set_h_z80 z80.main.e ixiyhl
   let
      value = z80.main |> set_h z80.main.e ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x65: IXIYHL -> Z80 -> Z80Delta
execute_0x65 ixiyhl z80 =
   -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
   -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
   --z80 |> set_h_z80 (get_l ixiyhl z80.main) ixiyhl
   let
      value = z80.main |> set_h (get_l ixiyhl z80.main) ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x66: IXIYHL -> Z80 -> Z80Delta
execute_0x66 ixiyhl z80 =
   -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
   -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      main = z80.main
   in
      --{ z80 | pc = value.pc, env = value.env } |> set_h_z80 value.value HL |> add_cpu_time 3
      MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> add_cpu_time_time 3)

execute_0x67: IXIYHL -> Z80 -> Z80Delta
execute_0x67 ixiyhl z80 =
   -- case 0x67: HL=HL&0xFF|A<<8; break;
   -- case 0x67: xy=xy&0xFF|A<<8; break;
   --z80 |> set_h_z80 z80.flags.a ixiyhl
   let
      value = z80.main |> set_h z80.flags.a ixiyhl
   in
      MainRegsWithPc value z80.pc

execute_0x68: IXIYHL -> Z80 -> Z80Delta
execute_0x68 ixiyhl z80 =
   -- case 0x68: HL=HL&0xFF00|B; break;
   -- case 0x68: xy=xy&0xFF00|B; break;
   --z80 |> set_l_z80 z80.main.b ixiyhl
   MainRegsWithPc (z80.main |> set_l z80.main.b ixiyhl) z80.pc

execute_0x69: IXIYHL -> Z80 -> Z80Delta
execute_0x69 ixiyhl z80 =
   -- case 0x69: HL=HL&0xFF00|C; break;
   -- case 0x69: xy=xy&0xFF00|C; break;
   --z80 |> set_l_z80 z80.main.c ixiyhl
   MainRegsWithPc (z80.main |> set_l z80.main.c ixiyhl) z80.pc

execute_0x6A: IXIYHL -> Z80 -> Z80Delta
execute_0x6A ixiyhl z80 =
   -- case 0x6A: HL=HL&0xFF00|D; break;
   -- case 0x6A: xy=xy&0xFF00|D; break;
   --z80 |> set_l_z80 z80.main.d ixiyhl
   MainRegsWithPc (z80.main |> set_l z80.main.d ixiyhl) z80.pc

execute_0x6B: IXIYHL -> Z80 -> Z80Delta
execute_0x6B ixiyhl z80 =
   -- case 0x6B: HL=HL&0xFF00|E; break;
   -- case 0x6B: xy=xy&0xFF00|E; break;
   --z80 |> set_l_z80 z80.main.e ixiyhl
   MainRegsWithPc (z80.main |> set_l z80.main.e ixiyhl) z80.pc

execute_0x6C: IXIYHL -> Z80 -> Z80Delta
execute_0x6C ixiyhl z80 =
   -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
   -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
   --z80 |> set_l_z80 (get_h ixiyhl z80.main) ixiyhl
   MainRegsWithPc (z80.main |> set_l (get_h ixiyhl z80.main) ixiyhl) z80.pc

execute_0x6E: IXIYHL -> Z80 -> Z80Delta
execute_0x6E ixiyhl z80 =
   -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
   -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      main = z80.main
      env_1 = z80.env
   in
      --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_l_z80 value.value HL |> add_cpu_time 3
      MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> add_cpu_time_time 3)

execute_0x6F: IXIYHL -> Z80 -> Z80Delta
execute_0x6F ixiyhl z80 =
    -- case 0x6F: HL=HL&0xFF00|A; break;
    -- case 0x6F: xy=xy&0xFF00|A; break;
    --z80 |> set_l_z80 z80.flags.a ixiyhl
   MainRegsWithPc (z80.main |> set_l z80.flags.a ixiyhl) z80.pc

execute_0x7077: IXIYHL -> Z80 -> Int -> Z80Delta
execute_0x7077 ixiyhl z80 value =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    let
       mem_target = z80 |> env_mem_hl ixiyhl
       env_1 = z80.env
       new_env = { env_1 | time = mem_target.time }
                 |> set_mem mem_target.value value
                 |> add_cpu_time_env 3
    in
       --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
       EnvWithPc new_env mem_target.pc

execute_0x70: IXIYHL -> Z80 -> Z80Delta
execute_0x70 ixiyhl z80 =
    -- case 0x70: env.mem(HL,B); time+=3; break;
    -- case 0x70: env.mem(getd(xy),B); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time }
    --             |> set_mem mem_target.value z80.main.b
    --             |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.b

execute_0x71: IXIYHL -> Z80 -> Z80Delta
execute_0x71 ixiyhl z80 =
    -- case 0x71: env.mem(HL,C); time+=3; break;
    -- case 0x71: env.mem(getd(xy),C); time+=3; break;
    --let
    --    mem_target = z80 |> env_mem_hl ixiyhl
    --    env_1 = z80.env
    --    new_env = { env_1 | time = mem_target.time }
    --              |> set_mem mem_target.value z80.main.c
    --              |> add_cpu_time_env 3
    --in
    --    --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --    EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.c

execute_0x72: IXIYHL -> Z80 -> Z80Delta
execute_0x72 ixiyhl z80 =
    -- case 0x72: env.mem(HL,D); time+=3; break;
    -- case 0x72: env.mem(getd(xy),D); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.d |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.d

execute_0x73: IXIYHL -> Z80 -> Z80Delta
execute_0x73 ixiyhl z80 =
    -- case 0x73: env.mem(HL,E); time+=3; break;
    -- case 0x73: env.mem(getd(xy),E); time+=3; break;
    --let
    --   mem_target = z80 |> env_mem_hl ixiyhl
    --   env_1 = z80.env
    --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.main.e |> add_cpu_time_env 3
    --in
    --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 z80.main.e

execute_0x74: IXIYHL -> Z80 -> Z80Delta
execute_0x74 ixiyhl z80 =
   -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
   -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_h HL z80.main) |> add_cpu_time_env 3
   --in
   --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
   --    EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 (get_h HL z80.main)

execute_0x75: IXIYHL -> Z80 -> Z80Delta
execute_0x75 ixiyhl z80 =
   -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
   -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value (get_l HL z80.main) |> add_cpu_time_env 3
   --in
   --   --{ z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
   --   EnvWithPc new_env mem_target.pc
    execute_0x7077 ixiyhl z80 (get_l HL z80.main)
--
--	private void halt()
--	{
--		halted = true;
--		int n = time_limit-time+3 >> 2;
--		if(n>0) {
--			n = env.halt(n, IR|R&0x7F);
--			R+=n; time+=4*n;
--		}
--	}

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

halt: Z80 -> Z80Delta
halt z80 =
   let
      interrupts = z80.interrupts
      n = shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
      (new_interrupts, time) = if n > 0 then
                                  -- turns out env.halt(n, r) just returns n...?
                                  --{ z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
                                 ({ interrupts | r = interrupts.r + n }, z80.env.time |> add_cpu_time_time (4 * n))
                               else
                                 (interrupts, z80.env.time)
                 --z80
    in
      --{ z80_1 | interrupts = { interrupts | halted = True } }
      InterruptsWithCpuTime { new_interrupts | halted = True } time

execute_0x77: IXIYHL -> Z80 -> Z80Delta
execute_0x77 ixiyhl z80 =
   -- case 0x77: env.mem(HL,A); time+=3; break;
   -- case 0x77: env.mem(getd(xy),A); time+=3; break;
   --let
   --   mem_target = z80 |> env_mem_hl ixiyhl
   --   env_1 = z80.env
   --   new_env = { env_1 | time = mem_target.time } |> set_mem mem_target.value z80.flags.a
   --in
   --   { z80 | pc = mem_target.pc } |> set_env new_env |> add_cpu_time 3
    execute_0x7077 ixiyhl z80 z80.flags.a

execute_0x78: Z80 -> Z80Delta
execute_0x78 z80 =
   -- case 0x78: A=B; break;
   --z80 |> set_a z80.main.b
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.b }

execute_0x79: Z80 -> Z80Delta
execute_0x79 z80 =
   -- case 0x79: A=C; break;
   --z80 |> set_a z80.main.c
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.c }

execute_0x7A: Z80 -> Z80Delta
execute_0x7A z80 =
   -- case 0x7A: A=D; break;
   --z80 |> set_a z80.main.d
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.d }

execute_0x7B: Z80 -> Z80Delta
execute_0x7B z80 =
   -- case 0x7B: A=E; break;
   --z80 |> set_a z80.main.e
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = z80.main.e }

execute_0x7C: IXIYHL -> Z80 -> Z80Delta
execute_0x7C ixiyhl z80 =
   -- case 0x7C: A=HL>>>8; break;
   -- case 0x7C: A=xy>>>8; break;
   --z80 |> set_a (get_h ixiyhl z80.main)
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = (get_h ixiyhl z80.main) }

execute_0x7D: IXIYHL -> Z80 -> Z80Delta
execute_0x7D ixiyhl z80 =
   -- case 0x7D: A=HL&0xFF; break;
   -- case 0x7D: A=xy&0xFF; break;
   --z80 |> set_a (get_l ixiyhl z80.main)
   let
       flags = z80.flags
   in
      FlagRegs { flags | a = (get_l ixiyhl z80.main) }

execute_0x7E: IXIYHL -> Z80 -> Z80Delta
execute_0x7E ixiyhl z80 =
   -- case 0x7E: A=env.mem(HL); time+=3; break;
   -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      --env_1 = z80.env
      flags = z80.flags
   in
      --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
      --EnvWithFlagsAndPc { env_1 | time = value.time } { flags | a = value.value } value.pc
      FlagsWithPcAndTime { flags | a = value.value } value.pc value.time

execute_0x80: Z80 -> Z80Delta
execute_0x80 z80 =
   -- case 0x80: add(B); break;
   --z80 |> set_flag_regs (z80_add z80.main.b z80.flags)
   FlagRegs (z80_add z80.main.b z80.flags)

execute_0x81: Z80 -> Z80Delta
execute_0x81 z80 =
   -- case 0x81: add(C); break;
   --z80 |> set_flag_regs (z80_add z80.main.c z80.flags)
   FlagRegs (z80_add z80.main.c z80.flags)

execute_0x82: Z80 -> Z80Delta
execute_0x82 z80 =
   -- case 0x82: add(D); break;
   --z80 |> set_flag_regs (z80_add z80.main.d z80.flags)
   FlagRegs (z80_add z80.main.d z80.flags)

execute_0x83: Z80 -> Z80Delta
execute_0x83 z80 =
   -- case 0x83: add(E); break;
   --z80 |> set_flag_regs (z80_add z80.main.e z80.flags)
   FlagRegs (z80_add z80.main.e z80.flags)

execute_0x84: IXIYHL -> Z80 -> Z80Delta
execute_0x84 ixiyhl z80 =
   -- case 0x84: add(HL>>>8); break;
   -- case 0x84: add(xy>>>8); break;
   --z80 |> set_flag_regs (z80_add (get_h ixiyhl z80.main) z80.flags)
   FlagRegs (z80_add (get_h ixiyhl z80.main) z80.flags)

execute_0x85: IXIYHL -> Z80 -> Z80Delta
execute_0x85 ixiyhl z80 =
   -- case 0x85: add(HL&0xFF); break;
   -- case 0x85: add(xy&0xFF); break;
   --z80 |> set_flag_regs (z80_add (get_l ixiyhl z80.main) z80.flags)
   FlagRegs (z80_add (get_l ixiyhl z80.main) z80.flags)

execute_0x86: IXIYHL -> Z80 -> Z80Delta
execute_0x86 ixiyhl z80 =
   -- case 0x86: add(env.mem(HL)); time+=3; break;
   -- case 0x86: add(env.mem(getd(xy))); time+=3; break;
  let
    value = hl_deref_with_z80 ixiyhl z80
    --env_1 = z80.env
    --z80_1 = { z80 | pc = value.pc, env = { env_1 | time = value.time } }
    flags_one = z80_add value.value z80.flags
  in
    FlagsWithPcAndTime flags_one value.pc value.time
       --{ z80_1 | flags = flags_one }

execute_0x87: Z80 -> Z80Delta
execute_0x87 z80 =
   -- case 0x87: add(A); break;
   --z80 |> set_flag_regs (z80_add z80.flags.a z80.flags)
   z80.flags |> z80_add z80.flags.a |> FlagRegs

execute_0x88: Z80 -> Z80Delta
execute_0x88 z80 =
   -- case 0x88: adc(B); break;
   --z80 |> set_flag_regs (adc z80.main.b z80.flags)
   z80.flags |> adc z80.main.b |> FlagRegs

execute_0x89: Z80 -> Z80Delta
execute_0x89 z80 =
   -- case 0x89: adc(C); break;
   --z80 |> set_flag_regs (adc z80.main.c z80.flags)
   z80.flags |> adc z80.main.c |> FlagRegs

execute_0x8A: Z80 -> Z80Delta
execute_0x8A z80 =
   -- case 0x8A: adc(D); break;
   --z80 |> set_flag_regs (adc z80.main.d z80.flags)
   z80.flags |> adc z80.main.d |> FlagRegs

execute_0x8B: Z80 -> Z80Delta
execute_0x8B z80 =
   -- case 0x8B: adc(E); break;
   --z80 |> set_flag_regs (adc z80.main.e z80.flags)
   z80.flags |> adc z80.main.e |> FlagRegs

execute_0x8C: IXIYHL -> Z80 -> Z80Delta
execute_0x8C ixiyhl z80 =
   -- case 0x8C: adc(HL>>>8); break;
   -- case 0x8C: adc(xy>>>8); break;
   --z80 |> set_flag_regs (adc (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> adc (get_h ixiyhl z80.main) |> FlagRegs

execute_0x8D: IXIYHL -> Z80 -> Z80Delta
execute_0x8D ixiyhl z80 =
   -- case 0x8D: adc(HL&0xFF); break;
   -- case 0x8D: adc(xy&0xFF); break;
   --z80 |> set_flag_regs (adc (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> adc (get_l ixiyhl z80.main) |> FlagRegs

execute_0x8E: IXIYHL -> Z80 -> Z80Delta
execute_0x8E ixiyhl z80 =
   -- case 0x8E: adc(env.mem(HL)); time+=3; break;
   -- case 0x8E: adc(env.mem(getd(xy))); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      --env_1 = z80.env
   in
      --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (adc value.value z80.flags)
      FlagsWithPcAndTime (z80.flags |> adc value.value) value.pc value.time

execute_0x8F: Z80 -> Z80Delta
execute_0x8F z80 =
   -- case 0x8F: adc(A); break;
   z80.flags |> adc z80.flags.a |> FlagRegs

execute_0x90: Z80 -> Z80Delta
execute_0x90 z80 =
   -- case 0x90: sub(B); break;
   --z80 |> set_flag_regs (z80_sub z80.main.b z80.flags)
   z80.flags |> z80_sub z80.main.b |> FlagRegs

execute_0x91: Z80 -> Z80Delta
execute_0x91 z80 =
   -- case 0x91: sub(C); break;
   --z80 |> set_flag_regs (z80_sub z80.main.c z80.flags)
   z80.flags |> z80_sub z80.main.c |> FlagRegs

execute_0x92: Z80 -> Z80Delta
execute_0x92 z80 =
   -- case 0x92: sub(D); break;
   --z80 |> set_flag_regs (z80_sub z80.main.d z80.flags)
   z80.flags |> z80_sub z80.main.d |> FlagRegs

execute_0x93: Z80 -> Z80Delta
execute_0x93 z80 =
   -- case 0x93: sub(E); break;
   --z80 |> set_flag_regs (z80_sub z80.main.e z80.flags)
   z80.flags |> z80_sub z80.main.e |> FlagRegs

execute_0x94: IXIYHL -> Z80 -> Z80Delta
execute_0x94 ixiyhl z80 =
   -- case 0x94: sub(HL>>>8); break;
   -- case 0x94: sub(xy>>>8); break;
   --z80 |> set_flag_regs (z80_sub (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> z80_sub (get_h ixiyhl z80.main) |> FlagRegs

execute_0x95: IXIYHL -> Z80 -> Z80Delta
execute_0x95 ixiyhl z80 =
   -- case 0x95: sub(HL&0xFF); break;
   -- case 0x95: sub(xy&0xFF); break;
   --z80 |> set_flag_regs (z80_sub (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> z80_sub (get_l ixiyhl z80.main) |> FlagRegs

execute_0x96: IXIYHL -> Z80 -> Z80Delta
execute_0x96 ixiyhl z80 =
   -- case 0x96: sub(env.mem(HL)); time+=3; break;
   -- case 0x96: sub(env.mem(getd(xy))); time+=3; break;
   let
       value = hl_deref_with_z80 ixiyhl z80
       --env_1 = z80.env
   in
       --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_sub value.value z80.flags)
      FlagsWithPcAndTime (z80.flags |> z80_sub value.value) value.pc value.time

execute_0x97: Z80 -> Z80Delta
execute_0x97 z80 =
   -- case 0x97: sub(A); break;
   --z80 |> set_flag_regs (z80_sub z80.flags.a z80.flags)
   z80.flags |> z80_sub z80.flags.a |> FlagRegs

execute_0x98: Z80 -> Z80Delta
execute_0x98 z80 =
   -- case 0x98: sbc(B); break;
   --z80 |> set_flag_regs (sbc z80.main.b z80.flags)
   z80.flags |> sbc z80.main.b |> FlagRegs

execute_0x99: Z80 -> Z80Delta
execute_0x99 z80 =
   -- case 0x99: sbc(C); break;
   --z80 |> set_flag_regs (sbc z80.main.c z80.flags)
   z80.flags |> sbc z80.main.c |> FlagRegs

execute_0x9A: Z80 -> Z80Delta
execute_0x9A z80 =
   -- case 0x9A: sbc(D); break;
   --z80 |> set_flag_regs (sbc z80.main.d z80.flags)
   z80.flags |> sbc z80.main.d |> FlagRegs

execute_0x9B: Z80 -> Z80Delta
execute_0x9B z80 =
   -- case 0x9B: sbc(E); break;
   --z80 |> set_flag_regs (sbc z80.main.e z80.flags)
   z80.flags |> sbc z80.main.e |> FlagRegs

execute_0x9C: IXIYHL -> Z80 -> Z80Delta
execute_0x9C ixiyhl z80 =
    -- case 0x9C: sbc(HL>>>8); break;
    -- case 0x9C: sbc(xy>>>8); break;
    --z80 |> set_flag_regs (sbc (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> sbc (get_h ixiyhl z80.main) |> FlagRegs

execute_0x9D: IXIYHL -> Z80 -> Z80Delta
execute_0x9D ixiyhl z80 =
    -- case 0x9D: sbc(HL&0xFF); break;
    -- case 0x9D: sbc(xy&0xFF); break;
    --z80 |> set_flag_regs (sbc (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> sbc (get_l ixiyhl z80.main) |> FlagRegs

execute_0x9E: IXIYHL -> Z80 -> Z80Delta
execute_0x9E ixiyhl z80 =
    -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
    -- case 0x9E: sbc(env.mem(getd(xy))); time+=3; break;
    let
       value = hl_deref_with_z80 ixiyhl z80
       --env_1 = z80.env
    in
       --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (sbc value.value z80.flags)
      FlagsWithPcAndTime (z80.flags |> sbc value.value) value.pc value.time

execute_0x9F: Z80 -> Z80Delta
execute_0x9F z80 =
   -- case 0x9F: sbc(A); break;
   --z80 |> set_flag_regs (sbc z80.flags.a z80.flags)
   z80.flags |> sbc z80.flags.a |> FlagRegs

execute_0xA0: Z80 -> Z80Delta
execute_0xA0 z80 =
   -- case 0xA0: and(B); break;
   --z80 |> set_flag_regs (z80_and z80.main.b z80.flags)
   z80.flags |> z80_and z80.main.b |> FlagRegs

execute_0xA1: Z80 -> Z80Delta
execute_0xA1 z80 =
   -- case 0xA1: and(C); break;
   --z80 |> set_flag_regs (z80_and z80.main.c z80.flags)
   z80.flags |> z80_and z80.main.c |> FlagRegs

execute_0xA2: Z80 -> Z80Delta
execute_0xA2 z80 =
   -- case 0xA2: and(D); break;
   --z80 |> set_flag_regs (z80_and z80.main.d z80.flags)
   z80.flags |> z80_and z80.main.d |> FlagRegs

execute_0xA3: Z80 -> Z80Delta
execute_0xA3 z80 =
   -- case 0xA3: and(E); break;
   --z80 |> set_flag_regs (z80_and z80.main.e z80.flags)
   z80.flags |> z80_and z80.main.e |> FlagRegs

execute_0xA4: IXIYHL -> Z80 -> Z80Delta
execute_0xA4 ixiyhl z80 =
    -- case 0xA4: and(HL>>>8); break;
    -- case 0xA4: and(xy>>>8); break;
    --z80 |> set_flag_regs (z80_and (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> z80_and (get_h ixiyhl z80.main) |> FlagRegs

execute_0xA5: IXIYHL -> Z80 -> Z80Delta
execute_0xA5 ixiyhl z80 =
   -- case 0xA5: and(HL&0xFF); break;
   -- case 0xA5: and(xy&0xFF); break;
   --z80 |> set_flag_regs (z80_and (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> z80_and (get_l ixiyhl z80.main) |> FlagRegs

execute_0xA6: IXIYHL -> Z80 -> Z80Delta
execute_0xA6 ixiyhl z80 =
    -- case 0xA6: and(env.mem(HL)); time+=3; break;
    -- case 0xA6: and(env.mem(getd(xy))); time+=3; break;
    let
       value = hl_deref_with_z80 ixiyhl z80
       env_1 = z80.env
    in
       --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_and value.value z80.flags)
      FlagsWithPcAndTime (z80.flags |> z80_and value.value) value.pc value.time

execute_0xA7: Z80 -> Z80Delta
execute_0xA7 z80 =
    -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
    -- and a is correct - I guess the above is a faster implementation
    --z80 |> set_flag_regs (z80_and z80.flags.a z80.flags)
   z80.flags |> z80_and z80.flags.a |> FlagRegs

execute_0xA8: Z80 -> Z80Delta
execute_0xA8 z80 =
         -- case 0xA8: xor(B); break;
   --z80 |> set_flag_regs (z80_xor z80.main.b z80.flags)
   z80.flags |> z80_xor z80.main.b |> FlagRegs

execute_0xA9: Z80 -> Z80Delta
execute_0xA9 z80 =
         -- case 0xA9: xor(C); break;
   --z80 |> set_flag_regs (z80_xor z80.main.c z80.flags)
   z80.flags |> z80_xor z80.main.c |> FlagRegs

execute_0xAA: Z80 -> Z80Delta
execute_0xAA z80 =
         -- case 0xAA: xor(D); break;
   --z80 |> set_flag_regs (z80_xor z80.main.d z80.flags)
   z80.flags |> z80_xor z80.main.d |> FlagRegs

execute_0xAB: Z80 -> Z80Delta
execute_0xAB z80 =
   -- case 0xAB: xor(E); break;
   --z80 |> set_flag_regs (z80_xor z80.main.e z80.flags)
   z80.flags |> z80_xor z80.main.e |> FlagRegs

execute_0xAC: IXIYHL -> Z80 -> Z80Delta
execute_0xAC ixiyhl z80 =
   -- case 0xAC: xor(HL>>>8); break;
   -- case 0xAC: xor(xy>>>8); break;
   --z80 |> set_flag_regs (z80_xor (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> z80_xor (get_h ixiyhl z80.main) |> FlagRegs

execute_0xAD: IXIYHL -> Z80 -> Z80Delta
execute_0xAD ixiyhl z80 =
   -- case 0xAD: xor(HL&0xFF); break;
   -- case 0xAD: xor(xy&0xFF); break;
   --z80 |> set_flag_regs (z80_xor (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> z80_xor (get_l ixiyhl z80.main) |> FlagRegs

execute_0xAE: IXIYHL -> Z80 -> Z80Delta
execute_0xAE ixiyhl z80 =
   -- case 0xAE: xor(env.mem(HL)); time+=3; break;
   -- case 0xAE: xor(env.mem(getd(xy))); time+=3; break;
   let
      value = hl_deref_with_z80 ixiyhl z80
      env_1 = z80.env
   in
      --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_xor value.value z80.flags)
      FlagsWithPcAndTime (z80.flags |> z80_xor value.value) value.pc value.time

execute_0xAF: Z80 -> Z80Delta
execute_0xAF z80 =
   -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
   --z80 |> set_flag_regs (z80_xor z80.flags.a z80.flags)
   z80.flags |> z80_xor z80.flags.a |> FlagRegs

execute_0xB0: Z80 -> Z80Delta
execute_0xB0 z80 =
    -- case 0xB0: or(B); break;
    --z80 |> set_flag_regs (z80_or z80.main.b z80.flags)
   z80.flags |> z80_or z80.main.b |> FlagRegs

execute_0xB1: Z80 -> Z80Delta
execute_0xB1 z80 =
    -- case 0xB1: or(C); break;
    --z80 |> set_flag_regs (z80_or z80.main.c z80.flags)
   z80.flags |> z80_or z80.main.c |> FlagRegs

execute_0xB2: Z80 -> Z80Delta
execute_0xB2 z80 =
    -- case 0xB2: or(D); break;
    --z80 |> set_flag_regs (z80_or z80.main.d z80.flags)
   z80.flags |> z80_or z80.main.d |> FlagRegs

execute_0xB3: Z80 -> Z80Delta
execute_0xB3 z80 =
    -- case 0xB3: or(E); break;
    --z80 |> set_flag_regs (z80_or z80.main.e z80.flags)
   z80.flags |> z80_or z80.main.e |> FlagRegs

execute_0xB4: IXIYHL -> Z80 -> Z80Delta
execute_0xB4 ixiyhl z80 =
    -- case 0xB4: or(HL>>>8); break;
    -- case 0xB4: or(xy>>>8); break;
    --z80 |> set_flag_regs (z80_or (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> z80_or (get_h ixiyhl z80.main) |> FlagRegs

execute_0xB5: IXIYHL -> Z80 -> Z80Delta
execute_0xB5 ixiyhl z80 =
    -- case 0xB5: or(HL&0xFF); break;
    -- case 0xB5: or(xy&0xFF); break;
    --z80 |> set_flag_regs (z80_or (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> z80_or (get_l ixiyhl z80.main) |> FlagRegs

execute_0xB6: IXIYHL -> Z80 -> Z80Delta
execute_0xB6 ixiyhl z80 =
  -- case 0xB6: or(env.mem(HL)); time+=3; break;
  -- case 0xB6: or(env.mem(getd(xy))); time+=3; break;
  let
    value = hl_deref_with_z80 ixiyhl z80
    --env_1 = z80.env
  in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (z80_or value.value z80.flags)
    FlagsWithPcAndTime (z80.flags |> z80_or value.value) value.pc value.time

execute_0xB7: Z80 -> Z80Delta
execute_0xB7 z80 =
    -- case 0xB7: or(A); break;
    --z80 |> set_flag_regs (z80_or z80.flags.a z80.flags)
   z80.flags |> z80_or z80.flags.a |> FlagRegs

execute_0xB8: Z80 -> Z80Delta
execute_0xB8 z80 =
   -- case 0xB8: cp(B); break;
   --z80 |> set_flag_regs (cp z80.main.b z80.flags)
   z80.flags |> cp z80.main.b |> FlagRegs

execute_0xB9: Z80 -> Z80Delta
execute_0xB9 z80 =
   -- case 0xB9: cp(C); break;
   --z80 |> set_flag_regs (cp z80.main.c z80.flags)
   z80.flags |> cp z80.main.c |> FlagRegs

execute_0xBA: Z80 -> Z80Delta
execute_0xBA z80 =
   -- case 0xBA: cp(D); break;
   --z80 |> set_flag_regs (cp z80.main.d z80.flags)
   z80.flags |> cp z80.main.d |> FlagRegs

execute_0xBB: Z80 -> Z80Delta
execute_0xBB z80 =
   -- case 0xBB: cp(E); break;
   --z80 |> set_flag_regs (cp z80.main.e z80.flags)
   z80.flags |> cp z80.main.e |> FlagRegs

execute_0xBC: IXIYHL -> Z80 -> Z80Delta
execute_0xBC ixiyhl z80 =
    -- case 0xBC: cp(HL>>>8); break;
    -- case 0xBC: cp(xy>>>8); break;
    --z80 |> set_flag_regs (cp (get_h ixiyhl z80.main) z80.flags)
   z80.flags |> cp (get_h ixiyhl z80.main) |> FlagRegs

execute_0xBD: IXIYHL -> Z80 -> Z80Delta
execute_0xBD ixiyhl z80 =
    -- case 0xBD: cp(HL&0xFF); break;
    -- case 0xBD: cp(xy&0xFF); break;
    --z80 |> set_flag_regs (cp (get_l ixiyhl z80.main) z80.flags)
   z80.flags |> cp (get_l ixiyhl z80.main) |> FlagRegs

execute_0xBE: IXIYHL -> Z80 -> Z80
execute_0xBE ixiyhl z80 =
    -- case 0xBE: cp(env.mem(HL)); time+=3; break;
    -- case 0xBE: cp(env.mem(getd(xy))); time+=3; break;
    let
       value = hl_deref_with_z80 ixiyhl z80
       env_1 = z80.env
    in
       { z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_flag_regs (cp value.value z80.flags)

execute_0xBF: Z80 -> Z80Delta
execute_0xBF z80 =
    -- case 0xBF: cp(A); break;
    --z80 |> set_flag_regs (cp z80.flags.a z80.flags)
   z80.flags |> cp z80.flags.a |> FlagRegs

lt40_array_lite: Array (Maybe (Z80 -> Z80Delta))
lt40_array_lite = makeLiteArray

list0255 = List.range 0 255

z80_to_delta: Maybe (Z80 -> Z80) -> Maybe (Z80 -> Z80Delta)
z80_to_delta z80func =
    case z80func of
        Just f ->  Just (\z80 -> Whole (z80 |> f))
        Nothing -> Nothing

ixiyhl_z80_to_delta: Maybe (IXIYHL -> Z80 -> Z80) -> Maybe (IXIYHL -> Z80 -> Z80Delta)
ixiyhl_z80_to_delta z80func =
    case z80func of
        Just f ->  Just (\ixiyhl z80  -> Whole (z80 |> f ixiyhl))
        Nothing -> Nothing

mergeFuncList:  Maybe (Z80 -> Z80Delta) -> Maybe (Z80 -> Z80Delta) -> Maybe (Z80 -> Z80Delta)
mergeFuncList afunc bfunc =
    case afunc of
        Just a -> Just a
        Nothing -> case bfunc of
                        Just b -> Just b
                        Nothing -> Nothing

makeLiteArray: Array (Maybe (Z80 -> Z80Delta))
makeLiteArray =
    let
       z80_funcs = list0255 |> List.map (\index -> lt40_dict_lite |> Dict.get index |> z80_to_delta)
       delta_funcs = list0255 |> List.map (\index -> lt40_delta_dict_lite |> Dict.get index)
    in
       List.map2 mergeFuncList z80_funcs delta_funcs |> Array.fromList

lt40_array: Array (Maybe ((IXIYHL -> Z80 -> Z80Delta)))
lt40_array = makeLt40Array

mergeIxiyFuncList:  Maybe (IXIYHL -> Z80 -> Z80Delta) -> Maybe (IXIYHL -> Z80 -> Z80Delta) -> Maybe (IXIYHL -> Z80 -> Z80Delta)
mergeIxiyFuncList afunc bfunc =
    case afunc of
        Just a -> Just a
        Nothing -> case bfunc of
                        Just b -> Just b
                        Nothing -> Nothing

lt40_dict: Dict Int (IXIYHL -> Z80 -> Z80)
lt40_dict = Dict.fromList
    [
          (0xBE, execute_0xBE),
          (0xCB, execute_0xCB)
    ]

makeLt40Array: Array (Maybe ((IXIYHL -> Z80 -> Z80Delta)))
makeLt40Array =
    let
       z80_funcs = list0255 |> List.map (\index -> lt40_dict |> Dict.get index |> ixiyhl_z80_to_delta)
       delta_funcs = list0255 |> List.map (\index -> lt40_delta_dict |> Dict.get index)
    in
       List.map2 mergeIxiyFuncList z80_funcs delta_funcs |> Array.fromList

lt40_delta_dict_lite: Dict Int (Z80 -> Z80Delta)
lt40_delta_dict_lite = Dict.fromList
    [
          (0x01, execute_0x01),
          (0x02, execute_0x02),
          (0x03, execute_0x03),
          (0x04, execute_0x04),
          (0x05, execute_0x05),
          (0x06, execute_0x06),
          (0x07, execute_0x07),
          (0x08, ex_af),
          (0x0A, execute_0x0A),
          (0x0B, execute_0x0B),
          (0x0C, execute_0x0C),
          (0x0D, execute_0x0D),
          (0x0E, execute_0x0E),
          (0x0F, execute_0x0F),
          (0x10, execute_0x10),
          (0x11, execute_0x11),
          (0x12, execute_0x12),
          (0x13, execute_0x13),
          (0x14, execute_0x14),
          (0x15, execute_0x15),
          (0x16, execute_0x16),
          (0x17, execute_0x17),
          (0x18, execute_0x18),
          (0x1A, execute_0x1A),
          (0x1B, execute_0x1B),
          (0x1C, execute_0x1C),
          (0x1D, execute_0x1D),
          (0x1E, execute_0x1E),
          (0x1F, execute_0x1F),
          (0x20, execute_0x20),
          (0x22, execute_0x22),
          (0x27, execute_0x27),
          (0x28, execute_0x28),
          (0x2F, execute_0x2F),
          (0x30, execute_0x30),
          (0x31, execute_0x31),
          (0x32, execute_0x32),
          (0x33, execute_0x33),
          (0x37, execute_0x37),
          (0x38, execute_0x38),
          (0x3A, execute_0x3A),
          (0x3B, execute_0x3B),
          (0x3C, execute_0x3C),
          (0x3D, execute_0x3D),
          (0x3E, execute_0x3E),
          (0x3F, execute_0x3F),
          -- case 0x40: break;
          (0x40, delta_noop),
          (0x00, delta_noop),
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
          (0x76, halt),
          (0x78, execute_0x78),
          (0x79, execute_0x79),
          (0x7A, execute_0x7A),
          (0x7B, execute_0x7B),
          -- case 0x7F: break;
          (0x7F, delta_noop),
          (0x80, execute_0x80),
          (0x81, execute_0x81),
          (0x82, execute_0x82),
          (0x83, execute_0x83),
          (0x87, execute_0x87),
          (0x88, execute_0x88),
          (0x89, execute_0x89),
          (0x8A, execute_0x8A),
          (0x8B, execute_0x8B),
          (0x8F, execute_0x8F),
          (0x90, execute_0x90),
          (0x91, execute_0x91),
          (0x92, execute_0x92),
          (0x93, execute_0x93),
          (0x97, execute_0x97),
          (0x98, execute_0x98),
          (0x99, execute_0x99),
          (0x9A, execute_0x9A),
          (0x9B, execute_0x9B),
          (0x9F, execute_0x9F),
          (0xA0, execute_0xA0),
          (0xA1, execute_0xA1),
          (0xA2, execute_0xA2),
          (0xA3, execute_0xA3),
          (0xA7, execute_0xA7),
          (0xA8, execute_0xA8),
          (0xA9, execute_0xA9),
          (0xAA, execute_0xAA),
          (0xAB ,execute_0xAB),
          (0xAF, execute_0xAF),
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
          (0xC8, execute_0xC8),
          (0xC9, execute_0xC9),
          (0xCA, execute_0xCA),
          (0xCC, execute_0xCC),
          (0xCD, execute_0xCD),
          (0xCE, execute_0xCE),
          (0xCF, execute_0xCF),
          (0xD0, execute_0xD0),
          (0xD1, execute_0xD1),
          (0xD2, execute_0xD2),
          (0xD3, execute_0xD3),
          (0xD5, execute_0xD5),
          (0xD6, execute_0xD6),
          (0xD7, execute_0xD7),
          (0xDD, (\z80 -> group_xy IXIY_IX z80)),
          (0xFD, (\z80 -> group_xy IXIY_IY z80))
    ]

lt40_dict_lite: Dict Int (Z80 -> Z80)
lt40_dict_lite = Dict.fromList
    [
          (0xF3, execute_0xF3),
          -- case 0xD9: exx(); break;
          (0xD9, exx),
          (0xEB, execute_0xEB),
          (0xF9, execute_0xF9),
          (0xFB, execute_0xFB),
          (0xE6, execute_0xE6),
          (0xF6, execute_0xF6),
          (0xF5, execute_0xF5),
          (0xED, group_ed),
          (0xE1, execute_0xE1),
          (0xE5, execute_0xE5),
          (0xE3, execute_0xE3),
          (0xF1, execute_0xF1),
          (0xD8, execute_0xD8),
          (0xDB, execute_0xDB),
          (0xF8, execute_0xF8),
          (0xEE, execute_0xEE),
          (0xF2, execute_0xF2),
          (0xFA, execute_0xFA),
          (0xDA, execute_0xDA),
          (0xFE, execute_0xFE),
          (0xDF, execute_0xDF),
          (0xE7, execute_0xE7),
          (0xEF, execute_0xEF),
          (0xF7, execute_0xF7),
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

execute_0xDF: Z80 -> Z80
execute_0xDF z80 =
    z80 |> rst_z80 0xDF

execute_0xE7: Z80 -> Z80
execute_0xE7 z80 =
    z80 |> rst_z80 0xE7

execute_0xEF: Z80 -> Z80
execute_0xEF z80 =
    z80 |> rst_z80 0xEF

execute_0xF7: Z80 -> Z80
execute_0xF7 z80 =
    z80 |> rst_z80 0xF7

execute_0xFF: Z80 -> Z80
execute_0xFF z80 =
    z80 |> rst_z80 0xFF

lt40_delta_dict: Dict Int (IXIYHL -> Z80 -> Z80Delta)
lt40_delta_dict = Dict.fromList
    [
          (0x09, execute_0x09),
          (0x19, execute_0x19),
          (0x21, execute_0x21),
          (0x23, execute_0x23),
          (0x24, execute_0x24),
          (0x25, execute_0x25),
          (0x26, execute_0x26),
          (0x29, execute_0x29),
          (0x2A, execute_0x2A),
          (0x2B, execute_0x2B),
          (0x2C, execute_0x2C),
          (0x2D, execute_0x2D),
          (0x2E, execute_0x2E),
          (0x34, execute_0x34),
          (0x35, execute_0x35),
          (0x36, execute_0x36),
          (0x39, execute_0x39),
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
          (0x84, execute_0x84),
          (0x85, execute_0x85),
          (0x86, execute_0x86),
          (0x8C, execute_0x8C),
          (0x8D, execute_0x8D),
          (0x8E, execute_0x8E),
          (0x94, execute_0x94),
          (0x95, execute_0x95),
          (0x96, execute_0x96),
          (0x9C, execute_0x9C),
          (0x9D, execute_0x9D),
          (0x9E, execute_0x9E),
          (0xA4, execute_0xA4),
          (0xA5, execute_0xA5),
          (0xA6, execute_0xA6),
          (0xAC, execute_0xAC),
          (0xAD, execute_0xAD),
          (0xAE, execute_0xAE),
          (0xB4, execute_0xB4),
          (0xB5, execute_0xB5),
          (0xB6, execute_0xB6),
          (0xBC, execute_0xBC),
          (0xBD, execute_0xBD),
          (0xE9, execute_0xE9)
    ]

--
--	private int getd(int xy)
--	{
--		int d = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 8;
--		return MP = (char)(xy + (byte)d);
--	}
--getd_value: Int -> Z80 -> CpuTimePcAndValue
--getd_value xy z80 =
--   let
--      d = z80.env |> mem z80.pc
--   in
--      CpuTimePcAndValue (d.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (xy + byte d.value))
env_mem_hl: IXIYHL -> Z80 -> CpuTimePcAndValue
env_mem_hl ixiyhl z80 =
  case ixiyhl of
    HL -> CpuTimePcAndValue z80.env.time z80.pc z80.main.hl
    IX ->
      let
        dval = z80.env |> mem z80.pc
      in
        CpuTimePcAndValue (dval.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (z80.main.ix + byte dval.value))
    IY ->
      let
        dval = z80.env |> mem z80.pc
      in
        CpuTimePcAndValue (dval.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (z80.main.iy + byte dval.value))


set_a: Int -> Z80 -> Z80
set_a value z80 =
    let
        z80_flags = z80.flags
    in
       { z80 | flags = { z80_flags | a = value } }

-- There appear to be many situations where we already know that we don't need all
-- this complexity as we're just doing LD A,B or something similar - so stop using it in those cases
load408bit: Int -> IXIYHL -> Z80 -> CpuTimePcAndValue
load408bit c_value ixiyhl z80 =
   case (and c_value 0x07) of
      0 -> b_with_z80 z80
      1 -> c_with_z80 z80
      2 -> d_with_z80 z80
      3 -> e_with_z80 z80
      4 -> h_with_z80 ixiyhl z80
      5 -> l_with_z80 ixiyhl z80
      6 -> hl_deref_with_z80 ixiyhl z80
      _ -> a_with_z80 z80

b_with_z80: Z80 -> CpuTimePcAndValue
b_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.b

c_with_z80: Z80 -> CpuTimePcAndValue
c_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.c

d_with_z80: Z80 -> CpuTimePcAndValue
d_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.d

e_with_z80: Z80 -> CpuTimePcAndValue
e_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.e

get_h: IXIYHL -> MainWithIndexRegisters -> Int
get_h ixiyhl z80 =
    shiftRightBy8 (get_xy ixiyhl z80)

h_with_z80: IXIYHL -> Z80 -> CpuTimePcAndValue
h_with_z80 ixiyhl z80 =
    CpuTimePcAndValue z80.env.time z80.pc (shiftRightBy8 (get_xy ixiyhl z80.main))

get_l: IXIYHL -> MainWithIndexRegisters -> Int
get_l ixiyhl z80 =
    and (get_xy ixiyhl z80) 0xFF

l_with_z80: IXIYHL -> Z80 -> CpuTimePcAndValue
l_with_z80 ixiyhl z80 =
    CpuTimePcAndValue z80.env.time z80.pc (and (get_xy ixiyhl z80.main) 0xFF)

hl_deref_with_z80: IXIYHL -> Z80 -> CpuTimePcAndValue
hl_deref_with_z80 ixiyhl z80 =
    let
        a = env_mem_hl ixiyhl z80
        new_b = mem a.value z80.env
    in
        CpuTimePcAndValue new_b.time a.pc new_b.value

a_with_z80: Z80 -> CpuTimePcAndValue
a_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.flags.a

set408bit: Int -> Int -> IXIYHL -> Z80 -> Z80
set408bit c value ixiyhl z80 =
    case (and c 0x07) of
       0 -> z80 |> set_b value
       1 -> z80 |> set_c value
       2 -> z80 |> set_d value
       3 -> z80 |> set_e value
       4 -> set_h_z80 value ixiyhl z80
       5 -> set_l_z80 value ixiyhl z80
       6 -> { z80 | env = set_mem z80.main.hl value z80.env }
       _ -> z80 |> set_a value

set_b: Int -> Z80 -> Z80
set_b value z80 =
   let
      z80_main = z80.main
   in
    { z80 | main = { z80_main | b = value } }

set_c: Int -> Z80 -> Z80
set_c value z80 =
   let
      z80_main = z80.main
   in
    { z80 | main = { z80_main | c = value } }

set_d: Int -> Z80 -> Z80
set_d value z80 =
   let
      z80_main = z80.main
   in
    { z80 | main = { z80_main | d = value } }

set_e: Int -> Z80 -> Z80
set_e value z80 =
   let
      z80_main = z80.main
   in
    { z80 | main = { z80_main | e = value } }

execute_0xC0: Z80 -> Z80Delta
execute_0xC0 z80 =
   -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
   let
      env =  z80.env |> add_cpu_time_env 1
   in
      if z80.flags.fr /= 0 then
         let
            result = env |> pop
         --   env = z80_1.env
         --   --x = debug_log "ret nz" (result.value |> subName) Nothing
         --   z80_2 = { z80_1 | env = { env | time = result.time, sp = result.sp } }
         in
         --   { z80_2 | pc = result.value }
         CpuTimeWithSpAndPc result.time result.sp result.value
      else
         --z80_1
         NoChange

execute_0xC1: Z80 -> Z80Delta
execute_0xC1 z80 =
   -- case 0xC1: v=pop(); B=v>>>8; C=v&0xFF; break;
   let
      v = z80.env |> pop
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
      --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
   in
      --z80_1 |> set_bc v.value
      MainRegsWithSpAndTime (z80.main |> set_bc_main v.value) v.sp v.time

execute_0xC2: Z80 -> Z80Delta
execute_0xC2 z80 =
  -- case 0xC2: jp(Fr!=0); break;
  --jp_z80 (z80.flags.fr /= 0) z80
  let
     v = jp (z80.flags.fr /= 0) z80
  in
     CpuTimeWithPc v.time v.pc

execute_0xC3: Z80 -> Z80Delta
execute_0xC3 z80 =
   -- case 0xC3: MP=PC=imm16(); break;
   let
      v = imm16 z80
      --env = z80.env
      --z80_1 = { z80 | pc = v.pc, env = { env | time = v.time } }
      --y = debug_log "jp" (v.value |> subName) Nothing
   in
      --z80_1 |> set_pc v.value
      CpuTimeWithPc v.time v.value

execute_0xC4: Z80 -> Z80Delta
execute_0xC4 z80 =
      -- case 0xC4: call(Fr!=0); break;
   --call_z80 (z80.flags.fr /= 0) z80
   let
     result = call (z80.flags.fr /= 0) z80
   in
      EnvWithPc result.env result.pc

execute_0xC5: Z80 -> Z80Delta
execute_0xC5 z80 =
   -- case 0xC5: push(B<<8|C); break;
   --z80 |> push (z80 |> get_bc)
   let
     bc =  (z80 |> get_bc)
     pushed = z80.env |> z80_push bc
   in
     --{ z80 | env = pushed }
     OnlyEnv pushed

execute_0xC6: Z80 -> Z80Delta
execute_0xC6 z80 =
   -- case 0xC6: add(imm8()); break;
   let
      v = imm8 z80
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

execute_0xC7: Z80 -> Z80Delta
execute_0xC7 z80 =
    --z80 |> rst_z80 0xC7
   --let
   --   result = z80 |> rst 0xC7
   --in
   --  EnvWithPc result.env result.pc
    z80 |> rst_delta 0xC7

execute_0xC8: Z80 -> Z80Delta
execute_0xC8 z80 =
    -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
   let
      z80_1_time = z80.env.time |> add_cpu_time_time 1
      env = z80.env
   in
      if z80.flags.fr == 0 then
           let
              popped = { env | time = z80_1_time } |> pop
           in
              --{ z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
              CpuTimeWithSpAndPc popped.time popped.sp popped.value
      else
           --z80_1
         OnlyTime z80_1_time

execute_0xC9: Z80 -> Z80Delta
execute_0xC9 z80 =
    -- case 0xC9: MP=PC=pop(); break;
   let
      a = z80.env |> pop
      --b = debug_log "ret" (a.value |> subName) Nothing
      --env = z80.env
   in
      --{ z80 | env = { env | time = a.time, sp = a.sp }, pc = a.value }
      CpuTimeWithSpAndPc a.time a.sp a.value

execute_0xCA: Z80 -> Z80Delta
execute_0xCA z80 =
    -- case 0xCA: jp(Fr==0); break;
    --jp_z80 (z80.flags.fr == 0) z80
  --let
  --  result = z80 |> jp (z80.flags.fr == 0)
  --in
  --  CpuTimeWithPc result.time result.pc
  z80 |> jp_delta (z80.flags.fr == 0)

execute_0xCB: IXIYHL -> Z80 -> Z80
execute_0xCB ixiyhl z80 =
    case ixiyhl of
        IX -> z80 |> group_xy_cb IXIY_IX
        IY -> z80 |> group_xy_cb IXIY_IY
        HL -> z80 |> group_cb

execute_0xCC: Z80 -> Z80Delta
execute_0xCC z80 =
    -- case 0xCC: call(Fr==0); break;
   --call_z80 (z80.flags.fr == 0) z80
  let
    result = z80 |> call (z80.flags.fr == 0)
  in
    EnvWithPc result.env result.pc

execute_0xCD: Z80 -> Z80Delta
execute_0xCD z80 =
   -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
   let
      v = z80 |> imm16
      env = z80.env
      --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
      pushed = { env | time = v.time } |> z80_push v.pc
   in
      --{ z80_1 | env = pushed, pc = v.value }
      EnvWithPc pushed v.value

execute_0xCE: Z80 -> Z80Delta
execute_0xCE z80 =
   -- case 0xCE: adc(imm8()); break;
   let
      v = z80 |> imm8
      flags = z80.flags |> adc v.value
      --env_1 = z80.env
   in
      --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
      FlagsWithPcAndTime flags v.pc v.time

execute_0xCF: Z80 -> Z80Delta
execute_0xCF z80 =
  --let
  --   result = z80 |> rst 0xCF
  --in
  --   EnvWithPc result.env result.pc
  z80 |> rst_delta 0xCF

execute_0xD0: Z80 -> Z80Delta
execute_0xD0 z80 =
  -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
  let
    z80_1_time = z80.env.time |> add_cpu_time_time 1
    --env = z80.env
  in
    if (and z80.flags.ff 0x100) == 0 then
      let
        popped = z80.env |> pop
        --x = debug_log "ret nc" (popped.value |> subName) Nothing
      in
        --{ z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
        CpuTimeWithSpAndPc popped.time popped.sp popped.value
    else
      --z80_1
      OnlyTime z80_1_time

execute_0xD1: Z80 -> Z80Delta
execute_0xD1 z80 =
   -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
   let
      v = z80.env |> pop
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
   in
      --z80_1 |> set_de v.value
      MainRegsWithSpAndTime (z80.main |> set_de_main v.value) v.sp v.time

jp_delta: Bool -> Z80 -> Z80Delta
jp_delta y z80 =
  let
    result = z80 |> jp y
  in
    CpuTimeWithPc result.time result.pc

execute_0xD2: Z80 -> Z80Delta
execute_0xD2 z80 =
   -- case 0xD2: jp((Ff&0x100)==0); break;
   --z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) == 0)
  --let
  --  result = z80 |> jp ((Bitwise.and z80.flags.ff 0x100) == 0)
  --in
  --  CpuTimeWithPc result.time result.pc
  z80 |> jp_delta ((Bitwise.and z80.flags.ff 0x100) == 0)

execute_0xD3: Z80 -> Z80Delta
execute_0xD3 z80 =
  -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
  let
    value = imm8 z80
    env_1 = z80.env
    env_2 = { env_1 | time = value.time }
    --z80_1 = { z80 | env = env_2, pc = value.pc }
    v = Bitwise.or value.value (shiftLeftBy8 z80.flags.a)
    env = out v z80.flags.a env_2 |> add_cpu_time_env 4
  in
    --{ z80_1 | env = env } |> add_cpu_time 4
    EnvWithPc env value.pc

execute_0xD5: Z80 -> Z80Delta
execute_0xD5 z80 =
  -- case 0xD5: push(D<<8|E); break;
  --z80 |> push (z80 |> get_de)
  let
    de = z80 |> get_de
    pushed = z80.env |> z80_push de
  in
    --{ z80 | env = pushed }
    OnlyEnv pushed

execute_0xD6: Z80 -> Z80Delta
execute_0xD6 z80 =
   -- case 0xD6: sub(imm8()); break;
  let
    v = z80 |> imm8
    flags = z80.flags |> z80_sub v.value
    --env_1 = z80.env
  in
      --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagsWithPcAndTime flags v.pc v.time

execute_0xD7: Z80 -> Z80Delta
execute_0xD7 z80 =
    z80 |> rst_delta 0xD7

execute_0xD8: Z80 -> Z80
execute_0xD8 z80 =
   -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
   let
      z80_1 = z80 |> add_cpu_time 1
      z80_2 = if and z80_1.flags.ff 0x100 /= 0 then
                 let
                     v = z80_1.env |> pop
                     env = z80_1.env
                 in
                    --debug_log "ret c" (v.value |> subName) ret
                    { z80_1 | env = { env | time = v.time, sp = v.sp }, pc = v.value }
              else
                 z80_1
   in
      z80_2

exx: Z80 -> Z80
exx z80 =
    let
        main = z80.main
        alt = z80.alt_main
   in
      { z80 | main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl },
              alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl } }


execute_0xDA: Z80 -> Z80
execute_0xDA z80 =
    -- case 0xDA: jp((Ff&0x100)!=0); break;
    z80 |> jp_z80 ((Bitwise.and z80.flags.ff 0x100) /= 0)

execute_0xEB: Z80 -> Z80
execute_0xEB z80 =
   -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
   let
      v = z80.main.hl
      de = z80 |> get_de
      --x = debug_log "EX DE,HL" ("DE " ++ (v |> toHexString) ++ " HL " ++ (de |> toHexString)) Nothing
   in
      z80 |> set_de v |> set_hl de

execute_0xF9: Z80 -> Z80
execute_0xF9 z80 =
   -- case 0xF9: SP=HL; time+=2; break;
   let
       env = z80.env
   in
   { z80 | env = { env | sp = z80.main.hl } |> add_cpu_time_env 2 }

execute_0xFB: Z80 -> Z80
execute_0xFB z80 =
    -- case 0xFB: IFF=3; break;
   z80 |> set_iff 3

execute_0xE6: Z80 -> Z80
execute_0xE6 z80 =
   -- case 0xE6: and(imm8()); break;
   let
      a = z80 |> imm8
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc  }
      flags = z80_1.flags |> z80_and a.value
   in
      { z80_1 | flags = flags }

execute_0xF3: Z80 -> Z80
execute_0xF3 z80 =
   -- case 0xF3: IFF=0; break;
   z80 |> set_iff 0

execute_0xF5: Z80 -> Z80
execute_0xF5 z80 =
   -- case 0xF5: push(A<<8|flags()); break;
   let
      a = z80 |> get_af
      pushed = z80.env |> z80_push a
   in
      { z80 | env = pushed }

execute_0xF6: Z80 -> Z80
execute_0xF6 z80 =
   -- case 0xF6: or(imm8()); break;
   let
      a = z80 |> imm8
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc }
      flags = z80_1.flags |> z80_or a.value
   in
      { z80_1 | flags = flags }

execute_0xE1: Z80 -> Z80
execute_0xE1 z80 =
   -- case 0xE1: HL=pop(); break;
   let
      hl = z80.env |> pop
      env = z80.env
      z80_1 = { z80 | env = { env | time = hl.time, sp = hl.sp } }
   in
      z80_1 |> set_hl hl.value

execute_0xE3: Z80 -> Z80
execute_0xE3 z80 =
   -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
   let
      v = z80.env |> pop
      env = z80.env
      z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
      pushed = z80_1.env |> z80_push z80_1.main.hl
   in
      { z80_1 | env = pushed } |> set_hl v.value |> add_cpu_time 2

execute_0xE5: Z80 -> Z80
execute_0xE5 z80 =
   -- case 0xE5: push(HL); break;
   let
       pushed = z80.env |> z80_push z80.main.hl
   in
      { z80 | env = pushed }

execute_0xF1: Z80 -> Z80
execute_0xF1 z80 =
    -- case 0xF1: af(pop()); break;
   let
      v = z80.env |> pop
      env = z80.env
      z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
   in
      z80_1 |> set_af v.value

execute_0xFE: Z80 -> Z80
execute_0xFE z80 =
   -- case 0xFE: cp(imm8()); break;
   let
      v = z80 |> imm8
      flags = z80.flags |> cp v.value
      env_1 = z80.env
   in
      { z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }

execute_0xE9: IXIYHL -> Z80 -> Z80Delta
execute_0xE9 ixiyhl z80 =
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

execute_0xDB: Z80 -> Z80
execute_0xDB z80 =
   -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
   let
      imm8val = z80 |> imm8
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = imm8val.time }, pc = imm8val.pc }
      v = or imm8val.value (shiftLeftBy8 z80_1.flags.a)
      a = z80_1.env |> z80_in v
      flags = z80_1.flags
   in
      { z80_1 | env = a.env, flags = { flags | a = a.value } }

execute_0xF8: Z80 -> Z80
execute_0xF8 z80 =
    -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
    let
       z80_1 = z80 |> add_cpu_time 1
       z80_2 = if (and z80_1.flags.ff c_FS) /= 0 then
                   let
                       popped = z80_1.env |> pop
                       env = z80_1.env
                   in
                       { z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
               else
                   z80_1
    in
       z80_2

execute_0xEE: Z80 -> Z80
execute_0xEE z80 =
   -- case 0xEE: xor(imm8()); break;
   let
      v = z80 |> imm8
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
      flags = z80_1.flags |> z80_xor v.value
   in
      { z80_1 | flags = flags }

execute_0xF2: Z80 -> Z80
execute_0xF2 z80 =
    -- case 0xF2: jp((Ff&FS)==0); break;
    z80 |> jp_z80 (Bitwise.and z80.flags.ff c_FS == 0)

execute_0xFA: Z80 -> Z80
execute_0xFA z80 =
   -- case 0xFA: jp((Ff&FS)!=0); break;
   z80 |> jp_z80 (Bitwise.and z80.flags.ff c_FS /= 0)

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

execute_delta: Z80 -> DeltaWithChanges
execute_delta tmp_z80 =
   --int v, c = env.m1(PC, IR|R++&0x7F);
   --PC = (char)(PC+1); time += 4;
   --switch(c) {
   let
     interrupts = tmp_z80.interrupts
     c = tmp_z80.env |> m1 tmp_z80.pc (or interrupts.ir (and interrupts.r 0x7F))
     env = tmp_z80.env
     old_z80 = { tmp_z80 | env = { env | time = c.time }, interrupts = { interrupts | r = interrupts.r + 1 } }
     new_pc = Bitwise.and (old_z80.pc + 1) 0xFFFF
     new_time = old_z80.env.time |> add_cpu_time_time 4
     z80 = { old_z80 | pc = new_pc } |> add_cpu_time 4
   in
     case execute_ltC0 c.value HL z80 of
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

execute_instruction: Z80 -> Z80
execute_instruction z80 =
   z80 |> execute_delta |> apply_delta z80

execute: Z80 -> Z80
execute z80 =
    if z80.interrupts.halted then
        z80_halt z80
    else
        --Loop.while (\x -> x.time_limit - x.env.time.cpu_time > 0) execute_instruction z80
        Loop.while (\x -> x.time_limit > x.env.time.cpu_time) execute_instruction z80
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
group_xy: IXIY -> Z80 -> Z80Delta
group_xy ixiy old_z80 =
  let
    c = m1 old_z80.pc (or old_z80.interrupts.ir (and old_z80.interrupts.r 0x7F)) old_z80.env
    intr = old_z80.interrupts
    env = old_z80.env
    z80_1 = { old_z80 | env = { env | time = c.time }, interrupts = { intr | r = intr.r + 1 } }
    new_pc = z80_1 |> inc_pc
    z80 = { z80_1 | pc = new_pc } |> add_cpu_time 4

    ltc0 = case ixiy of
      IXIY_IX -> execute_ltC0 c.value IX z80
      IXIY_IY -> execute_ltC0 c.value IY z80
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
-- case 0xE1: xy=pop(); break;
-- case 0xE5: push(xy); break;
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
-- case 0xE3: v=pop(); push(xy); MP=xy=v; time+=2; break;
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
--	private void group_ed()
--	{
--		int v, c = env.m1(PC, IR|R++&0x7F);
--		PC = (char)(PC+1); time += 4;
--		switch(c) {

execute_ED47: Z80 -> Z80
execute_ED47 z80 =
   -- case 0x47: i(A); time++; break;
   z80 |> set_i z80.flags.a |> add_cpu_time 1

execute_ED78: Z80 -> Z80
execute_ED78 z80 =
   --  case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
   let
      v = z80 |> get_bc
      new_a = z80.env |> z80_in v
      flags = z80.flags
   in
      { z80 | env = new_a.env, flags = { flags | a = new_a.value } } |> f_szh0n0p new_a.value |> add_cpu_time 4

execute_ED52: Z80 -> Z80
execute_ED52 z80 =
   -- case 0x52: sbc_hl(D<<8|E); break;
   z80 |> sbc_hl (or (shiftLeftBy8 z80.main.d) z80.main.e)

execute_ED43: Z80 -> Z80
execute_ED43 z80 =
   -- case 0x43: MP=(v=imm16())+1; env.mem16(v,B<<8|C); time+=6; break;
   let
     v = imm16 z80
     z80_2 = { z80 | pc = v.pc }
     env = z80_2.env |> set_mem16 v.value (or (shiftLeftBy8 z80.main.b) z80.main.c)
   in
     { z80_2 | env = env } |> add_cpu_time 6

execute_ED53: Z80 -> Z80
execute_ED53 z80 =
   -- case 0x53: MP=(v=imm16())+1; env.mem16(v,D<<8|E); time+=6; break;
   let
     v = imm16 z80
     z80_1 = { z80 | pc = v.pc }
     env = z80_1.env |> set_mem16 v.value (or (shiftLeftBy8 z80.main.d) z80.main.e)
   in
     { z80_1 | env = env } |> add_cpu_time 6

execute_EDB8: Z80 -> Z80
execute_EDB8 z80 =
   -- case 0xB8: ldir(-1,true); break;
   z80 |> ldir -1 True

execute_EDB0: Z80 -> Z80
execute_EDB0 z80 =
   --0xB0 -> debug_log "LDIR" ("HL " ++ (z80.main.hl |> toHexString) ++ " BC " ++ (z80 |> get_bc |> toHexString2)) (z80 |> ldir 1 True)
   -- case 0xB0: ldir(1,true); break;
   z80 |> ldir 1 True

execute_ED7B: Z80 -> Z80
execute_ED7B z80 =
   -- case 0x7B: MP=(v=imm16())+1; SP=env.mem16(v); time+=6; break;
  let
    v = z80 |> imm16
    z80_1 = { z80 | pc = v.pc }
    sp = z80_1.env |> mem16 v.value
    env = z80_1.env
  in
    { z80_1 | env = { env | time = sp.time } } |> add_cpu_time 6

execute_ED4B: Z80 -> Z80
execute_ED4B z80 =
   -- case 0x4B: MP=(v=imm16())+1; v=env.mem16(v); B=v>>>8; C=v&0xFF; time+=6; break;
  let
    v1 = z80 |> imm16
    z80_1 = { z80 | pc = v1.pc }
    v2 = z80_1.env |> mem16 v1.value
    env = z80_1.env
    --x = debug_log "LD BC,(nnnn)" (v2.value |> toHexString) Nothing
  in
    { z80_1 | env = { env | time = v2.time } } |> set_bc v2.value |> add_cpu_time 6

execute_ED73: Z80 -> Z80
execute_ED73 z80 =
  -- case 0x73: MP=(v=imm16())+1; env.mem16(v,SP); time+=6; break;
  let
    v = z80 |> imm16
    z80_1 = { z80 | pc = v.pc }
    env = z80.env |> set_mem16 v.value z80_1.env.sp
  in
    { z80 | env = env } |> add_cpu_time 6

execute_ED5B: Z80 -> Z80
execute_ED5B z80 =
  -- case 0x5B: MP=(v=imm16())+1; v=env.mem16(v); D=v>>>8; E=v&0xFF; time+=6; break;
  let
    v1 = z80 |> imm16
    z80_1 = { z80 | pc = v1.pc }
    v2 = z80_1.env |> mem16 v1.value
    env = z80_1.env
  in
    { z80_1 | env = { env | time = v2.time } } |> set_de v2.value |> add_cpu_time 6

execute_ED42: Z80 -> Z80
execute_ED42 z80 =
  -- case 0x42: sbc_hl(B<<8|C); break;
  let
    bc = z80 |> get_bc
  in
    z80 |> sbc_hl bc

execute_ED72: Z80 -> Z80
execute_ED72 z80 =
  -- case 0x72: sbc_hl(SP); break;
  z80 |> sbc_hl z80.env.sp

group_ed_dict: Dict Int (Z80 -> Z80)
group_ed_dict = Dict.fromList
    [
          (0x47, execute_ED47),
            -- case 0x4F: r(A); time++; break;
            -- case 0x57: ld_a_ir(IR>>>8); break;
            -- case 0x5F: ld_a_ir(r()); break;
            -- case 0x67: rrd(); break;
            -- case 0x6F: rld(); break;
          (0x6F, rld),
          (0x78, execute_ED78),
          (0x52, execute_ED52),
          (0x43, execute_ED43),
          (0x53, execute_ED53),
          (0xB8, execute_EDB8),
          (0xB0, execute_EDB0),
          (0x7B, execute_ED7B),
          (0x4B, execute_ED4B),
          (0x73, execute_ED73),
          (0x5B, execute_ED5B),
          (0x42, execute_ED42),
          (0x72, execute_ED72),
          (0x46, execute_ED46),
          (0x4E, execute_ED4E),
          (0x56, execute_ED56),
          (0x5E, execute_ED5E),
          (0x66, execute_ED66),
          (0x6E, execute_ED6E),
          (0x76, execute_ED76),
          (0x7E, execute_ED7E),
          (0x40, execute_ED40),
          (0x48, execute_ED48),
          (0x50, execute_ED50),
          (0x58, execute_ED58),
          (0x60, execute_ED60),
          (0x68, execute_ED68),
          (0x70, execute_ED70)
    ]

-- case 0x46:
-- case 0x4E:
-- case 0x56:
-- case 0x5E:
-- case 0x66:
-- case 0x6E:
-- case 0x76:
-- case 0x7E: IM = c>>3&3; break;

execute_ED464E565E666E767E: Int -> Z80 -> Z80
execute_ED464E565E666E767E value z80  =
    z80 |> set_im_direct (and (shiftRightBy 3 value) 3)

execute_ED46: Z80 -> Z80
execute_ED46 z80 =
    z80 |> execute_ED464E565E666E767E 0x46

execute_ED4E: Z80 -> Z80
execute_ED4E z80 =
    z80 |> execute_ED464E565E666E767E 0x4E

execute_ED56: Z80 -> Z80
execute_ED56 z80 =
    z80 |> execute_ED464E565E666E767E 0x56

execute_ED5E: Z80 -> Z80
execute_ED5E z80 =
    z80 |> execute_ED464E565E666E767E 0x5E

execute_ED66: Z80 -> Z80
execute_ED66 z80 =
    z80 |> execute_ED464E565E666E767E 0x66

execute_ED6E: Z80 -> Z80
execute_ED6E z80 =
    z80 |> execute_ED464E565E666E767E 0x6E

execute_ED76: Z80 -> Z80
execute_ED76 z80 =
    z80 |> execute_ED464E565E666E767E 0x76

execute_ED7E: Z80 -> Z80
execute_ED7E z80 =
    z80 |> execute_ED464E565E666E767E 0x7E

execute_ED40485058606870: Int -> Z80 -> Z80
execute_ED40485058606870 value z80  =
    let
      bc = z80 |> get_bc
      inval = z80.env |> z80_in bc
      z80_1 = z80 |> set408bit (shiftRightBy 3 (value - 0x40)) inval.value HL
    in
      z80_1 |> f_szh0n0p inval.value |> add_cpu_time 4

execute_ED40: Z80 -> Z80
execute_ED40 z80 =
    z80 |> execute_ED40485058606870 0x40

execute_ED48: Z80 -> Z80
execute_ED48 z80 =
    z80 |> execute_ED40485058606870 0x48

execute_ED50: Z80 -> Z80
execute_ED50 z80 =
    z80 |> execute_ED40485058606870 0x50

execute_ED58: Z80 -> Z80
execute_ED58 z80 =
    z80 |> execute_ED40485058606870 0x58

execute_ED60: Z80 -> Z80
execute_ED60 z80 =
    z80 |> execute_ED40485058606870 0x60

execute_ED68: Z80 -> Z80
execute_ED68 z80 =
    z80 |> execute_ED40485058606870 0x68

execute_ED70: Z80 -> Z80
execute_ED70 z80 =
    z80 |> execute_ED40485058606870 0x70

group_ed: Z80 -> Z80
group_ed z80_0 =
   let
      ints = z80_0.interrupts
      c = m1 z80_0.pc (or z80_0.interrupts.ir (and z80_0.interrupts.r 0x7F)) z80.env
      new_r = z80_0.interrupts.r + 1
      old_z80 = { z80_0 | interrupts = { ints | r = new_r } }
      new_pc = old_z80 |> inc_pc
      z80 = { old_z80 | pc = new_pc } |> add_cpu_time 4

      ed_func = group_ed_dict |> Dict.get c.value
   in
      case ed_func of
        Just f -> z80 |> f
        Nothing ->
          --// -------------- >8 ed
          ---- case 0x6A: adc_hl(HL); break;
          --0x6A -> z80 |> adc_hl z80.main.hl
          ---- case 0x5A: adc_hl(D<<8|E); break;
          --0x5A -> z80 |> adc_hl (z80 |> get_de)
          ---- case 0x4A: adc_hl(B<<8|C); break;
          --0x4A -> z80 |> adc_hl (z80 |> get_bc)
         --else if List.member c.value [0x44, 0x4C, 0x54, 0x5C, 0x64, 0x6C, 0x74, 0x7C] then
         --   -- case 0x44:
         --   -- case 0x4C:
         --   -- case 0x54:
         --   -- case 0x5C:
         --   -- case 0x64:
         --   -- case 0x6C:
         --   -- case 0x74:
         --   -- case 0x7C: v=A; A=0; sub(v); break;
         --   let
         --      flags = z80.flags
         --      flags_1 = { flags | a = 0 } |> z80_sub flags.a
         --   in
         --      z80 |> set_flag_regs flags_1
            debug_todo "group_ed" (c.value |> toHexString2) z80
-- case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
-- case 0x41: env.out(B<<8|C,B); time+=4; break;
-- case 0x49: env.out(B<<8|C,C); time+=4; break;
-- case 0x51: env.out(B<<8|C,D); time+=4; break;
-- case 0x59: env.out(B<<8|C,E); time+=4; break;
-- case 0x61: env.out(B<<8|C,HL>>>8); time+=4; break;
-- case 0x69: env.out(B<<8|C,HL&0xFF); time+=4; break;
-- case 0x71: env.out(B<<8|C,0); time+=4; break;
-- case 0x79: MP=(v=B<<8|C)+1; env.out(v,A); time+=4; break;
-- case 0x62: sbc_hl(HL); break;
-- case 0x63: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
-- case 0x6B: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
-- case 0x7A: adc_hl(SP); break;
-- case 0x45:
-- case 0x4D:
-- case 0x55:
-- case 0x5D:
-- case 0x65:
-- case 0x6D:
-- case 0x75:
-- case 0x7D: IFF|=IFF>>1; MP=PC=pop(); break;
-- case 0xA0: ldir(1,false); break;
-- case 0xA8: ldir(-1,false); break;
-- case 0xA1: cpir(1,false); break;
-- case 0xA9: cpir(-1,false); break;
-- case 0xB1: cpir(1,true); break;
-- case 0xB9: cpir(-1,true); break;
-- case 0xA2:
-- case 0xA3:
-- case 0xAA:
-- case 0xAB:
-- case 0xB2:
-- case 0xB3:
-- case 0xBA:
-- case 0xBB: inir_otir(c); break;
--// -------------- >8
--		default: System.out.println(PC+": Not emulated ED/"+c);
--		}
--	}
--
group_cb: Z80 -> Z80
group_cb tmp_z80 =
--	private void group_cb()
--	{
--		int v, c = env.m1(PC, IR|R++&0x7F);
--		PC = (char)(PC+1); time += 4;
--		int o = c>>>3 & 7;
--		switch(c & 0xC7) {
   let
      new_r = and (tmp_z80.interrupts.r + 1) 0x7F
      ir_or_r = or tmp_z80.interrupts.ir new_r
      c = m1 tmp_z80.pc (or tmp_z80.interrupts.ir ir_or_r) tmp_z80.env
      env = tmp_z80.env
      old_z80 = { tmp_z80 | env = { env | time = c.time } }
      new_pc = old_z80 |> inc_pc
      z80 = { old_z80 | pc = new_pc } |> add_cpu_time 4
      o = Bitwise.and (c.value |> shiftRightBy 3) 7
      caseval = Bitwise.and c.value 0xC7
      --y = debug_log "group_cb caseval" (caseval |> toHexString2) Nothing
   in
      -- case 0x00: B=shifter(o,B); break;
      -- case 0x01: C=shifter(o,C); break;
      -- case 0x02: D=shifter(o,D); break;
      -- case 0x03: E=shifter(o,E); break;
      -- case 0x04: HL=HL&0xFF|shifter(o,HL>>>8)<<8; break;
      -- case 0x05: HL=HL&0xFF00|shifter(o,HL&0xFF); break;
      -- case 0x06: v=shifter(o,env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
      -- case 0x07: A=shifter(o,A); break;
      if caseval < 0x08 then
         let
           raw = z80 |> load408bit caseval HL
           --z = debug_log "group_cb raw" (raw.value |> toHexString2) Nothing
           value = shifter o raw.value z80.flags
           --w = debug_log "group_cb value" (value.value |> toHexString2) Nothing
           env_1 = z80.env
         in
           { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs value.flags |> set408bit caseval value.value HL
      else if caseval >= 0x40 && caseval <= 0x47 then
         -- case 0x40: bit(o,B); break;
         -- case 0x41: bit(o,C); break;
         -- case 0x42: bit(o,D); break;
         -- case 0x43: bit(o,E); break;
         -- case 0x44: bit(o,HL>>>8); break;
         -- case 0x45: bit(o,HL&0xFF); break;
         -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
         -- case 0x47: bit(o,A); break;
         let
           raw = z80 |> load408bit caseval HL
           flags = bit o raw.value z80.flags
           env_1 = z80.env
         in
           { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set_flag_regs flags
      else if caseval >= 0x80 && caseval <= 0x87 then
         -- case 0x80: B=B&~(1<<o); break;
         -- case 0x81: C=C&~(1<<o); break;
         -- case 0x82: D=D&~(1<<o); break;
         -- case 0x83: E=E&~(1<<o); break;
         -- case 0x84: HL&=~(0x100<<o); break;
         -- case 0x85: HL&=~(1<<o); break;
         -- case 0x86: v=env.mem(HL)&~(1<<o); time+=4; env.mem(HL,v); time+=3; break;
         -- case 0x87: A=A&~(1<<o); break;
         let
             raw = z80 |> load408bit caseval HL
             result = and raw.value (1 |> (shiftLeftBy o) |> complement)
             env_1 = z80.env
         in
             { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
      else if caseval >= 0xC0 && caseval <= 0xC7 then
         -- case 0xC0: B=B|1<<o; break;
         -- case 0xC1: C=C|1<<o; break;
         -- case 0xC2: D=D|1<<o; break;
         -- case 0xC3: E=E|1<<o; break;
         -- case 0xC4: HL|=0x100<<o; break;
         -- case 0xC5: HL|=1<<o; break;
         -- case 0xC6: v=env.mem(HL)|1<<o; time+=4; env.mem(HL,v); time+=3; break;
         -- case 0xC7: A=A|1<<o; break;
         let
             raw = z80 |> load408bit caseval HL
             result = or raw.value (1 |> (shiftLeftBy o))
             env_1 = z80.env
         in
             { z80 | pc = raw.pc, env = { env_1 | time = raw.time } } |> set408bit caseval result HL
      else
         debug_todo "group_cb" (caseval |> toHexString) z80
--		}
--	}
--
--	private void group_xy_cb(int xy)
--	{
--		int pc = PC;
--		int a = MP = (char)(xy + (byte)env.mem(pc));
--		time += 3;
--		int c = env.mem((char)(pc+1));
--		PC = (char)(pc+2);
--		time += 5;
--		int v = env.mem(a);
--		time += 4;
--		int o = c>>>3 & 7;
group_xy_cb: IXIY -> Z80 -> Z80
group_xy_cb ixiyhl z80 =
   let
      xy = get_ixiy_xy ixiyhl z80.main
      offset = mem z80.pc z80.env
      a = char (xy + (byte offset.value))
      env_1 = z80.env
      z80_1 = { z80 | env = { env_1 | time = offset.time |> add_cpu_time_time 3} }
      c = z80_1.env |> mem (char (z80.pc + 1))
      new_pc = z80_1 |> inc_pc2
      env_2 = z80_1.env
      z80_2 = { z80_1 | env = { env_2 | time = c.time |> add_cpu_time_time 5 }, pc = new_pc }
      v1 = z80_2.env |> mem a
      env_3 = z80_2.env
      z80_3 = { z80_2 | env = { env_3 | time = v1.time |> add_cpu_time_time 4} }
      o = and (shiftRightBy 3 c.value) 7
--		switch(c&0xC0) {
--			case 0x00: v = shifter(o, v); break;
--			case 0x40: bit(o, v); Ff=Ff&~F53 | a>>8&F53; return;
--			case 0x80: v &= ~(1<<o); break;
--			case 0xC0: v |= 1<<o; break;
--		}
      v2 = case (and c.value 0xC0) of
         0x00 -> shifter o v1.value z80_3.flags
         0x40 -> let
                   flags = bit o v1.value z80_3.flags
                 in
                   IntWithFlags v1.value { flags | ff = or (and flags.ff (complement c_F53)) (shiftRightBy (and 8 c_F53) a) }
         0x80 -> IntWithFlags (and v1.value (complement (shiftLeftBy o 1))) z80_3.flags
         _ -> IntWithFlags (or v1.value (shiftLeftBy o 1)) z80_3.flags
      new_env = set_mem a v2.value z80_3.env
      --y = debug_log "xy_cb2" ((z80.pc |> toHexString) ++ " c " ++ (c.value |> toHexString2) ++
      --                                                   " set " ++ (a |> toHexString) ++
      --                                                   " from " ++ (v1.value |> toHexString2) ++
      --                                                   " to " ++ (v2.value |> toHexString2)) new_env
--		env.mem(a, v);
--		time += 3;
      z80_4 = { z80_3 | flags = v2.flags, env = new_env |> add_cpu_time_env 3 }
--		switch(c&0x07) {
--			case 0: B = v; break;
--			case 1: C = v; break;
--			case 2: D = v; break;
--			case 3: E = v; break;
--			case 4: HL = HL&0x00FF | v<<8; break;
--			case 5: HL = HL&0xFF00 | v; break;
--			case 7: A = v; break;
--		}
      caseval = (and c.value 0x07)
   in
      if caseval /= 6 then
         set408bit caseval v2.value HL z80_4
      else
         z80_4

im0: Int -> Z80 -> Z80
im0 bus z80 =
    if and bus 0x38 == 0xFF then
        let
            new_pc = bus - 199
        in
            z80 |> set_pc new_pc
    else
        z80

interrupt: Int -> Z80 -> Z80
interrupt bus z80 =
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
                        env_and_pc = z80.env |> mem16 addr
                        env = z80.env
                      in
                        { new_z80 | env = { env | time = env_and_pc.time } |> add_cpu_time_env 6, pc = env_and_pc.value }
                _ -> new_z80

set_im_direct: Int -> Z80 -> Z80
set_im_direct value z80 =
   let
      ints = debug_log "set_im" value z80.interrupts
   in
      { z80 | interrupts = { ints | iM = value } }

--set_env: Z80Env -> Z80 -> Z80
--set_env z80env z80 =
--   { z80 | env = z80env }

set_flag_regs: FlagRegisters -> Z80 -> Z80
set_flag_regs flags z80 =
   { z80 | flags = flags }
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

