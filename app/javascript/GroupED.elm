module GroupED exposing (..)

--	private void group_ed()
--	{
--		int v, c = env.m1(PC, IR|R++&0x7F);
--		PC = (char)(PC+1); time += 4;
--		switch(c) {

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (add_cpu_time_time)
import Dict exposing (Dict)
import Utils exposing (char, shiftLeftBy8, shiftRightBy8, toHexString2)
import Z80Debug exposing (debug_log, debug_todo)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (add_cpu_time_env, m1, mem, mem16, set_mem, set_mem16, z80_in)
import Z80Flags exposing (c_F3, c_F5, c_F53, c_FC)
import Z80Types exposing (IXIYHL(..), InterruptRegisters, Z80, add_cpu_time, dec_pc2, get_bc, get_de, imm16, inc_pc, set408bit, set_bc, set_bc_main, set_de, set_de_main)

execute_ED42: Z80 -> Z80Delta
execute_ED42 z80 =
  -- case 0x42: sbc_hl(B<<8|C); break;
  let
    bc = z80 |> get_bc
  in
    z80 |> sbc_hl bc

execute_ED43: Z80 -> Z80Delta
execute_ED43 z80 =
   -- case 0x43: MP=(v=imm16())+1; env.mem16(v,B<<8|C); time+=6; break;
   let
     v = imm16 z80
     z80_2 = { z80 | pc = v.pc }
     env = z80_2.env |> set_mem16 v.value (Bitwise.or (shiftLeftBy8 z80.main.b) z80.main.c)
   in
     --{ z80_2 | env = env } |> add_cpu_time 6 |> Whole
     EnvWithPc env v.pc

execute_ED47: Z80 -> Z80Delta
execute_ED47 z80 =
   -- case 0x47: i(A); time++; break;
   --z80 |> set_i z80.flags.a |> add_cpu_time 1 |> Whole
   InterruptsWithCpuTime (z80 |> set_i z80.flags.a) (z80.env.time |> add_cpu_time_time 1)

execute_ED4B: Z80 -> Z80Delta
execute_ED4B z80 =
   -- case 0x4B: MP=(v=imm16())+1; v=env.mem16(v); B=v>>>8; C=v&0xFF; time+=6; break;
  let
    v1 = z80 |> imm16
    z80_1 = { z80 | pc = v1.pc }
    env = z80_1.env
    v2 = { env | time = v1.time } |> mem16 v1.value
    --x = debug_log "LD BC,(nnnn)" (v2.value |> toHexString) Nothing
  in
    --{ z80_1 | env = { env | time = v2.time } } |> set_bc v2.value |> add_cpu_time 6 |> Whole
    MainRegsWithPcAndCpuTime (z80.main |> set_bc_main v2.value) v1.pc (v2.time |> add_cpu_time_time 6)

execute_ED52: Z80 -> Z80Delta
execute_ED52 z80 =
   -- case 0x52: sbc_hl(D<<8|E); break;
   z80 |> sbc_hl (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)

execute_ED53: Z80 -> Z80Delta
execute_ED53 z80 =
   -- case 0x53: MP=(v=imm16())+1; env.mem16(v,D<<8|E); time+=6; break;
   let
     v = imm16 z80
     z80_1 = { z80 | pc = v.pc }
     env = z80_1.env |> set_mem16 v.value (Bitwise.or (shiftLeftBy8 z80.main.d) z80.main.e)
   in
     --{ z80_1 | env = env } |> add_cpu_time 6 |> Whole
     EnvWithPc (env |> add_cpu_time_env 6) v.pc

execute_ED5B: Z80 -> Z80Delta
execute_ED5B z80 =
  -- case 0x5B: MP=(v=imm16())+1; v=env.mem16(v); D=v>>>8; E=v&0xFF; time+=6; break;
  let
    v1 = z80 |> imm16
    --z80_1 = { z80 | pc = v1.pc }
    env = z80.env
    v2 = { env | time = v1.time } |> mem16 v1.value
  in
    --{ z80_1 | env = { env | time = v2.time } } |> set_de v2.value |> add_cpu_time 6 |> Whole
    MainRegsWithPcAndCpuTime (z80.main |> set_de_main v2.value) v1.pc (v2.time |> add_cpu_time_time 6)

execute_ED72: Z80 -> Z80Delta
execute_ED72 z80 =
  -- case 0x72: sbc_hl(SP); break;
  z80 |> sbc_hl z80.env.sp

execute_ED73: Z80 -> Z80Delta
execute_ED73 z80 =
  -- case 0x73: MP=(v=imm16())+1; env.mem16(v,SP); time+=6; break;
  let
    v = z80 |> imm16
    z80_1 = { z80 | pc = v.pc }
    env = z80.env
    env2 = { env | time = v.time } |> set_mem16 v.value z80_1.env.sp
  in
    --{ z80 | env = env2 } |> add_cpu_time 6 |> Whole
    EnvWithPc (env2 |> add_cpu_time_env 6) v.pc

execute_ED78: Z80 -> Z80Delta
execute_ED78 z80 =
   --  case 0x78: MP=(v=B<<8|C)+1; f_szh0n0p(A=env.in(v)); time+=4; break;
   let
      v = z80 |> get_bc
      new_a = z80.env |> z80_in v
      flags = z80.flags
   in
      { z80 | env = new_a.env, flags = { flags | a = new_a.value } } |> f_szh0n0p new_a.value |> add_cpu_time 4 |> Whole

execute_ED7B: Z80 -> Z80Delta
execute_ED7B z80 =
   -- case 0x7B: MP=(v=imm16())+1; SP=env.mem16(v); time+=6; break;
  let
    v = z80 |> imm16
    z80_1 = { z80 | pc = v.pc }
    sp = z80_1.env |> mem16 v.value
    env = z80_1.env
  in
    { z80_1 | env = { env | time = sp.time } } |> add_cpu_time 6 |> Whole

execute_EDB0: Z80 -> Z80Delta
execute_EDB0 z80 =
   --0xB0 -> debug_log "LDIR" ("HL " ++ (z80.main.hl |> toHexString) ++ " BC " ++ (z80 |> get_bc |> toHexString2)) (z80 |> ldir 1 True)
   -- case 0xB0: ldir(1,true); break;
   z80 |> ldir 1 True

execute_EDB8: Z80 -> Z80Delta
execute_EDB8 z80 =
   -- case 0xB8: ldir(-1,true); break;
   z80 |> ldir -1 True

group_ed_dict: Dict Int (Z80 -> Z80Delta)
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

execute_ED464E565E666E767E: Int -> Z80 -> Z80Delta
execute_ED464E565E666E767E value z80  =
    z80 |> set_im_direct (Bitwise.and (shiftRightBy 3 value) 3)

execute_ED46: Z80 -> Z80Delta
execute_ED46 z80 =
    z80 |> execute_ED464E565E666E767E 0x46

execute_ED4E: Z80 -> Z80Delta
execute_ED4E z80 =
    z80 |> execute_ED464E565E666E767E 0x4E

execute_ED56: Z80 -> Z80Delta
execute_ED56 z80 =
    z80 |> execute_ED464E565E666E767E 0x56

execute_ED5E: Z80 -> Z80Delta
execute_ED5E z80 =
    z80 |> execute_ED464E565E666E767E 0x5E

execute_ED66: Z80 -> Z80Delta
execute_ED66 z80 =
    z80 |> execute_ED464E565E666E767E 0x66

execute_ED6E: Z80 -> Z80Delta
execute_ED6E z80 =
    z80 |> execute_ED464E565E666E767E 0x6E

execute_ED76: Z80 -> Z80Delta
execute_ED76 z80 =
    z80 |> execute_ED464E565E666E767E 0x76

execute_ED7E: Z80 -> Z80Delta
execute_ED7E z80 =
    z80 |> execute_ED464E565E666E767E 0x7E

execute_ED40485058606870: Int -> Z80 -> Z80Delta
execute_ED40485058606870 value z80  =
    let
      bc = z80 |> get_bc
      inval = z80.env |> z80_in bc
      z80_1 = z80 |> set408bit (shiftRightBy 3 (value - 0x40)) inval.value HL
    in
      z80_1 |> f_szh0n0p inval.value |> add_cpu_time 4 |> Whole

execute_ED40: Z80 -> Z80Delta
execute_ED40 z80 =
    z80 |> execute_ED40485058606870 0x40

execute_ED48: Z80 -> Z80Delta
execute_ED48 z80 =
    z80 |> execute_ED40485058606870 0x48

execute_ED50: Z80 -> Z80Delta
execute_ED50 z80 =
    z80 |> execute_ED40485058606870 0x50

execute_ED58: Z80 -> Z80Delta
execute_ED58 z80 =
    z80 |> execute_ED40485058606870 0x58

execute_ED60: Z80 -> Z80Delta
execute_ED60 z80 =
    z80 |> execute_ED40485058606870 0x60

execute_ED68: Z80 -> Z80Delta
execute_ED68 z80 =
    z80 |> execute_ED40485058606870 0x68

execute_ED70: Z80 -> Z80Delta
execute_ED70 z80 =
    z80 |> execute_ED40485058606870 0x70

group_ed: Z80 -> Z80Delta
group_ed z80_0 =
   let
      ints = z80_0.interrupts
      c = m1 z80_0.pc (Bitwise.or z80_0.interrupts.ir (Bitwise.and z80_0.interrupts.r 0x7F)) z80.env
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
            debug_todo "group_ed" (c.value |> toHexString2) z80 |> Whole
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

f_szh0n0p: Int -> Z80 -> Z80
f_szh0n0p r z80 =
   let
      fr = r
      ff = Bitwise.or (Bitwise.and z80.flags.ff (complement 0xFF)) fr
      fa = Bitwise.or r 0x100
      flags = z80.flags
   in
      { z80 | flags = { flags | fr = fr, ff = ff, fa = fa, fb = 0 } }

--	private void rld()
--	{
--		int v = env.mem(HL)<<4 | A&0x0F;
--		time += 7;
--		f_szh0n0p(A = A&0xF0 | v>>>8);
--		env.mem(HL, v & 0xFF);
--		MP = HL+1;
--		time += 3;
--	}
rld: Z80 -> Z80Delta
rld z80 =
    let
        v_lhs_1 = z80.env |> mem z80.main.hl
        v_rhs = Bitwise.and z80.flags.a 0x0F
        v_lhs = v_lhs_1.value |> (shiftLeftBy 4)
        v = Bitwise.or v_lhs v_rhs
        a1 = Bitwise.and z80.flags.a 0xF0
        new_a = Bitwise.or a1 (shiftRightBy8 v)
        flags = z80.flags
        new_flags = { flags | a = new_a }
        z80_1 = z80 |> f_szh0n0p new_a
        env_0 = z80.env
        env_1 = { env_0 | time = v_lhs_1.time } |> set_mem z80_1.main.hl (Bitwise.and v 0xFF)
    in
        { z80_1 | flags = new_flags, env = env_1 } |> add_cpu_time 10 |> Whole

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
sbc_hl: Int -> Z80 -> Z80Delta
sbc_hl b z80 =
   let
      a = z80.main.hl
      r1 = a - b - (Bitwise.and (shiftRightBy8 z80.flags.ff) c_FC)
      ff = shiftRightBy8 r1
      fa = shiftRightBy8 a
      fb = complement (shiftRightBy8 b)
      r = char r1
      fr = Bitwise.or (shiftRightBy8 r) (shiftLeftBy8 r)
      main = z80.main
      flags = z80.flags
   in
      --{ z80 | main = { main | hl = r }, flags = { flags | ff = ff, fa = fa, fb = fb, fr = fr} } |> add_cpu_time 7 |> Whole
      FlagsWithPCMainAndCpuTime { flags | ff = ff, fa = fa, fb = fb, fr = fr} z80.pc { main | hl = r } (z80.env.time |> add_cpu_time_time 7)

set_im_direct: Int -> Z80 -> Z80Delta
set_im_direct value z80 =
   let
      ints = debug_log "set_im" value z80.interrupts
   in
      { z80 | interrupts = { ints | iM = value } } |> Whole

--
--
--	private void ldir(int i, boolean r)
--	{
ldir: Int -> Bool -> Z80 -> Z80Delta
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
      ff = Bitwise.or (Bitwise.or (Bitwise.and z80_2.flags.ff (complement c_F53)) (Bitwise.and v2 c_F3)) (Bitwise.and (shiftLeftBy 4 v2) c_F5)
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
      a = Bitwise.and ((z80_2 |> get_bc) - 1) 0xFFFF
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
      { z80_4 | flags = { flags | fr = fr, ff = ff, fa = v, fb = v } } |> Whole

--	void i(int v) {IR = IR&0xFF | v<<8;}
set_i: Int -> Z80 -> InterruptRegisters
set_i v z80 =
    let
        ir = Bitwise.or (Bitwise.and z80.interrupts.ir 0xFF) (shiftLeftBy8 v)
        interrupts = z80.interrupts
    in
        --{ z80 | interrupts = { interrupts | ir = ir } }
        { interrupts | ir = ir }

