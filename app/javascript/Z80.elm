--
-- $Id$
--
module Z80 exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeAndValue, CpuTimeCTime, CpuTimePcAndValue, addCpuTimeTime)
import Dict exposing (Dict)
import Group0x00 exposing (delta_dict_00, delta_dict_lite_00)
import Group0x10 exposing (delta_dict_10, delta_dict_lite_10)
import Group0x20 exposing (delta_dict_20, delta_dict_lite_20, miniDict20)
import Group0x30 exposing (delta_dict_30, delta_dict_lite_30)
import Group0x40 exposing (delta_dict_40, delta_dict_lite_40, miniDict40)
import Group0x50 exposing (delta_dict_50, delta_dict_lite_50)
import Group0x60 exposing (delta_dict_60, delta_dict_lite_60)
import Group0x70 exposing (delta_dict_70, delta_dict_lite_70)
import Group0x80 exposing (delta_dict_80, delta_dict_lite_80)
import Group0x90 exposing (delta_dict_90, delta_dict_lite_90)
import Group0xA0 exposing (delta_dict_A0, delta_dict_lite_A0)
import Group0xB0 exposing (delta_dict_B0, delta_dict_lite_B0)
import Group0xC0 exposing (delta_dict_C0, delta_dict_lite_C0)
import Group0xE0 exposing (delta_dict_E0, delta_dict_lite_E0)
import Loop
import SimpleFlagOps exposing (singleByteFlags)
import SimpleSingleByte exposing (singleByteMainAndFlagRegisters, singleByteMainRegs)
import SingleWith8BitParameter exposing (singleWith8BitParam)
import Utils exposing (char, shiftLeftBy8, shiftRightBy8, toHexString)
import Z80Debug exposing (debugTodo)
import Z80Delta exposing (DeltaWithChangesData, Z80Delta(..), jp_delta, rst_delta)
import Z80Env exposing (Z80Env, addCpuTimeEnv, m1, mem, mem16, out, z80_in, z80_pop, z80_push, z80env_constructor)
import Z80Execute exposing (DeltaWithChanges(..), apply_delta)
import Z80Flags exposing (FlagRegisters, IntWithFlags, c_FC, c_FS, get_af, set_af, z80_cp, z80_or, z80_sub)
import Z80Ram exposing (c_FRSTART)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), IntWithFlagsTimeAndPC, InterruptRegisters, MainRegisters, MainWithIndexRegisters, Z80, add_cpu_time, get_de, imm8, inc_pc, jp_z80, rst_z80, set_de_main, set_iff)

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
get_ei: Z80 -> Bool
get_ei z80 =
    (Bitwise.and z80.interrupts.iff 1) /= 0  -- slightly odd != operator in Elm
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

execute_ltC0: Int -> Z80ROM -> Z80 -> Maybe Z80Delta
execute_ltC0 c rom48k z80 =
   case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
      Just f -> Just (z80 |> f HL rom48k)
      Nothing ->
         case lt40_array_lite |> Array.get c |> Maybe.withDefault Nothing of
            Just f_without_ixiyhl -> Just (z80 |> f_without_ixiyhl rom48k)
            Nothing -> Nothing

execute_ltC0_xy: Int -> IXIY -> Z80ROM -> Z80 -> Maybe Z80Delta
execute_ltC0_xy c ixoriy rom48k z80 =
    case xYDict |> Dict.get c of
        Just xyFunc -> Just (z80 |> xyFunc ixoriy rom48k)
        Nothing ->
            let
                only_ixiy = case ixoriy of
                            IXIY_IX -> IX
                            IXIY_IY -> IY
            in
               case lt40_array |> Array.get c |> Maybe.withDefault Nothing of
                  Just f -> Just (z80 |> f only_ixiy rom48k)
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




lt40_array_lite: Array (Maybe (Z80ROM -> Z80 -> Z80Delta))
lt40_array_lite = makeLiteArray

list0255 = List.range 0 255

z80_to_delta: Maybe (Z80ROM -> Z80 -> Z80) -> Maybe (Z80ROM -> Z80 -> Z80Delta)
z80_to_delta z80func =
    case z80func of
        Just f ->  Just (\rom48k z80  -> Whole (z80 |> f rom48k))
        Nothing -> Nothing

--ixiyhl_z80_to_delta: Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80) -> Maybe (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
--ixiyhl_z80_to_delta z80func =
--    case z80func of
--        Just f ->  Just (\ixiyhl rom48k z80  -> Whole (z80 |> f ixiyhl rom48k))
--        Nothing -> Nothing

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

makeLt40Array: Array (Maybe ((IXIYHL -> Z80ROM -> Z80 -> Z80Delta)))
makeLt40Array =
    let
       delta_funcs = list0255 |> List.map (\index -> lt40_delta_dict |> Dict.get index)
    in
       delta_funcs |> Array.fromList

lt40_delta_dict_lite: Dict Int (Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict_lite = Dict.fromList
    [
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
    |> Dict.union delta_dict_lite_40
    |> Dict.union delta_dict_lite_50
    |> Dict.union delta_dict_lite_B0
    |> Dict.union delta_dict_lite_C0
    |> Dict.union delta_dict_lite_E0
    |> Dict.union delta_dict_lite_60
    |> Dict.union delta_dict_lite_70

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

xYDict: Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
xYDict = miniDict40
    |> Dict.union miniDict20

lt40_delta_dict: Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
lt40_delta_dict = delta_dict_00
    |> Dict.union delta_dict_80
    |> Dict.union delta_dict_90
    |> Dict.union delta_dict_A0
    |> Dict.union delta_dict_10
    |> Dict.union delta_dict_20
    |> Dict.union delta_dict_30
    |> Dict.union delta_dict_B0
    |> Dict.union delta_dict_40
    |> Dict.union delta_dict_50
    |> Dict.union delta_dict_60
    |> Dict.union delta_dict_70
    |> Dict.union delta_dict_C0
    |> Dict.union delta_dict_E0

execute_0xD0: Z80ROM -> Z80 -> Z80Delta
execute_0xD0 rom48k z80 =
  -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
  let
    z80_1_time = z80.env.time |> addCpuTimeTime 1
    --env = z80.env
  in
    if (and z80.flags.ff 0x100) == 0 then
      let
        popped = z80.env |> z80_pop rom48k
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
      v = z80.env |> z80_pop rom48k
      --env = z80.env
      --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
   in
      --z80_1 |> set_de v.value
      MainRegsWithSpAndTime (z80.main |> set_de_main v.value) v.sp v.time

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
    value = imm8 z80.pc z80.env.time rom48k z80.env.ram
    env_1 = z80.env
    env_2 = { env_1 | time = value.time }
    v = Bitwise.or value.value (shiftLeftBy8 z80.flags.a)
    env = out v z80.flags.a env_2 |> addCpuTimeEnv 4
  in
    EnvWithPc env value.pc

execute_0xD5: Z80ROM -> Z80 -> Z80Delta
execute_0xD5 _ z80 =
  -- case 0xD5: push(D<<8|E); break;
  --z80 |> push (z80 |> get_de)
  let
    de = z80.main |> get_de
    --pushed = z80.env |> z80_push de
  in
    --{ z80 | env = pushed }
    OnlyPush de

execute_0xD6: Z80ROM -> Z80 -> Z80Delta
execute_0xD6 rom48k z80 =
   -- case 0xD6: sub(imm8()); break;
  let
    v = imm8 z80.pc z80.env.time rom48k z80.env.ram
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
    z80_1_time = z80.env.time |> addCpuTimeTime 1
  in
    if and z80.flags.ff 0x100 /= 0 then
      let
        env = z80.env
        v = { env | time = z80_1_time } |> z80_pop rom48k
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
      imm8val = imm8 z80.pc z80.env.time rom48k z80.env.ram
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

execute_0xF1: Z80ROM -> Z80 -> Z80Delta
execute_0xF1 rom48k z80 =
    -- case 0xF1: af(pop()); break;
   let
      v = z80.env |> z80_pop rom48k
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
      a = z80.flags |> get_af
      --pushed = z80.env |> z80_push a
   in
      --{ z80 | env = pushed }
      OnlyPush a

execute_0xF6: Z80ROM -> Z80 -> Z80Delta
execute_0xF6 rom48k z80 =
   -- case 0xF6: or(imm8()); break;
   let
      a = imm8 z80.pc z80.env.time rom48k z80.env.ram
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
       z80_1_time = z80.env.time |> addCpuTimeTime 1
       z80_2 = if (and z80.flags.ff c_FS) /= 0 then
                   let
                       env = z80.env
                       popped = { env | time = z80_1_time } |> z80_pop rom48k
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
   { z80 | env = { env | sp = z80.main.hl } |> addCpuTimeEnv 2 }

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
      v = imm8 z80.pc z80.env.time rom48k z80.env.ram
      flags = z80.flags |> z80_cp v.value
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

--maybeMainRegister: CpuTimeAndValue -> Z80 -> Maybe DeltaWithChanges
--maybeMainRegister c_value z80 =
--    singleByteMainRegs |> Dict.get c_value.value |> Maybe.map (\mainRegFunc -> RegisterChangeDelta c_value.time (mainRegFunc z80.main))
--
--maybeFlagRegister: CpuTimeAndValue -> Z80 -> Maybe DeltaWithChanges
--maybeFlagRegister c_value z80 =
--    singleByteFlags |> Dict.get c_value.value |> Maybe.map (\f -> FlagDelta c_value.time (f z80.flags))
--
--maybeMainWithFlagsRegister: CpuTimeAndValue -> Z80 -> Maybe DeltaWithChanges
--maybeMainWithFlagsRegister c_value z80 =
--    singleByteMainAndFlagRegisters |> Dict.get c_value.value |> Maybe.map (\f -> PureDelta c_value.time (f z80.main z80.flags))

execute_delta: Z80ROM -> Z80 -> DeltaWithChanges
execute_delta rom48k tmp_z80 =
   --int v, c = env.m1(PC, IR|R++&0x7F);
   --PC = (char)(PC+1); time += 4;
   --switch(c) {
   let
      interrupts = tmp_z80.interrupts
      c = tmp_z80.env |> m1 tmp_z80.pc (Bitwise.or interrupts.ir (Bitwise.and interrupts.r 0x7F)) rom48k
   in
   case singleWith8BitParam |> Dict.get c.value of
       Just f ->
           let
              param = mem (Bitwise.and (tmp_z80.pc + 1) 0xFFFF) c.time rom48k tmp_z80.env.ram
           in
           -- duplicate of code in imm8 - add 3 to the cpu_time
           Simple8BitDelta (param.time |> addCpuTimeTime 3) (f param.value)
       Nothing ->
           case singleByteMainRegs  |> Dict.get c.value of
                Just mainRegFunc ->  RegisterChangeDelta c.time (mainRegFunc tmp_z80.main)
                Nothing ->
                    case singleByteFlags |> Dict.get c.value of
                        Just flagFunc -> FlagDelta c.time (flagFunc tmp_z80.flags)
                        Nothing ->
                          case singleByteMainAndFlagRegisters |> Dict.get c.value of
                              Just f -> PureDelta c.time (f tmp_z80.main tmp_z80.flags)
                              Nothing ->
                                  let
                                     env = tmp_z80.env
                                     old_z80 = { tmp_z80 | env = { env | time = c.time }, interrupts = { interrupts | r = interrupts.r + 1 } }
                                     new_pc = Bitwise.and (old_z80.pc + 1) 0xFFFF
                                     z80 = { old_z80 | pc = new_pc } |> add_cpu_time 4
                                     new_time = z80.env.time
                                  in
                                 case z80 |> execute_ltC0 c.value rom48k of
                                   Just z80delta -> OldDeltaWithChanges (DeltaWithChangesData z80delta interrupts new_pc new_time)
                                   Nothing ->
                                        --case c.value of
                                            --0xDD -> DeltaWithChanges (group_xy IXIY_IX z80) interrupts new_pc new_time
                                            --0xFD -> DeltaWithChanges (group_xy IXIY_IY z80) interrupts new_pc new_time
                                            --0xED -> DeltaWithChanges (Whole (group_ed z80)) interrupts new_pc new_time
                                            --0xCD -> DeltaWithChanges (execute_0xCD z80) interrupts new_pc new_time
                                            --_ ->
                                     let
                                       delta = debugTodo "execute" (c.value |> toHexString) z80  |> Whole
                                     in
                                       OldDeltaWithChanges (DeltaWithChangesData delta interrupts new_pc new_time)
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

    ltc0 = z80 |> execute_ltC0_xy c.value ixiy rom48k
   in
     case ltc0 of
       Just z_z80 -> z_z80
       Nothing ->
             --case c.value of
                --0xCB -> group_xy_cb ixiy z80 |> Whole
                --_ -> debug_todo "group_xy" (c.value |> toHexString) z80 |> Whole
         debugTodo "group_xy" (c.value |> toHexString) z80 |> Whole
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
            new_z80 = { z80_1 | env = pushed |> addCpuTimeEnv 6 }
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
                        { new_z80 | env = { env | time = env_and_pc.time } |> addCpuTimeEnv 6, pc = env_and_pc.value }
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

