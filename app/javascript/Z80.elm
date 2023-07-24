--
-- $Id$
--
module Z80 exposing (..)

import Bitwise exposing (and, complement, or, shiftLeftBy, shiftRightBy)
import Loop
import Utils exposing (byte, char, debug_log, debug_todo, shiftLeftBy8, shiftRightBy8, toHexString)
import Z80Env exposing (Z80Env, c_FRSTART, m1, mem, mem16, out, set_mem, set_mem16, z80_in, z80env_constructor)
import Z80Rom exposing (subName)

type alias MainRegisters =
   {
      b:   Int,
      c:   Int,
      d:   Int,
      e:   Int,
      hl:  Int
   }
type alias FlagRegisters =
   {
      a:   Int,
      ff:  Int,
      fr:  Int,
      fa:  Int,
      fb:  Int
   }
type alias RegisterSet =
   {
      main: MainRegisters,
      flags: FlagRegisters
   }
type alias IntWithFlags =
    {
       value: Int,
       flags: FlagRegisters
    }
type alias IntWithFlagsAndTime =
    {
       value: Int,
       flags: FlagRegisters,
       time: Int
    }
type alias IntWithZ80AndTime =
    {
        value: Int,
        z80: Z80,
        time: Int
    }
type alias IntWithZ80 =
    {
        value: Int,
        z80: Z80
    }
type alias InterruptRegisters =
   {
      ir:  Int,
      r:   Int,
      --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iff: Int,
      iM: Int,
      halted: Bool
   }
type alias Z80 =
   {
      env: Z80Env,
      pc:  Int,
      sp:  Int,
      main: MainRegisters,
      flags: FlagRegisters,
      alt_main: MainRegisters,
      alt_flags: FlagRegisters,
      ix: Int,
      iy: Int,
      interrupts: InterruptRegisters,
      time_limit: Int
   }

add_cpu_time: Int -> Z80 -> Z80
add_cpu_time value z80 =
   let
      env = z80.env
   in
      { z80 | env = { env | cpu_time = env.cpu_time + value } }

constructor: Z80
constructor =
    let
        main = MainRegisters 0 0 0 0 0
        alternate = MainRegisters 0 0 0 0 0
        main_flags = FlagRegisters 0 0 0 0 0
        alt_flags = FlagRegisters 0 0 0 0 0
        interrupts = InterruptRegisters 0 0 0 0 False
    in
        Z80 z80env_constructor 0 0 main main_flags alternate alt_flags 0 0 interrupts c_FRSTART
--/*
-- lazy flag evaluation:
--
-- state is stored in four variables: Ff, Fr, Fa, Fb
--
--   Z: Fr==0
--   P: parity of Fr&0xFF
--   V: (Fa.7^Fr.7) & (Fb.7^Fr.7)
--   X: Fa.8
-- P/V: X ? P : V
--   N: Fb.9
--   H: Fr.4 ^ Fa.4 ^ Fb.4 ^ Fb.12
--
--		FEDCBA98 76543210
--	Ff	.......C S.5.3...
--	Fr	........ V..H....
--	Fa	.......X V..H....
--	Fb	...H..N. V..H....
--*/
c_FC = 0x01
c_FN = 0x02
c_FP = 0x04
c_F3 = 0x08
c_FH = 0x10
c_F5 = 0x20
c_FZ = 0x40
c_FS = 0x80
c_F53 = 0x28


--private int flags() {
--	int f = Ff, a = Fa, b = Fb, r = Fr;
--	f = f&(FS|F53) | f>>>8&FC;		// S.5.3..C
--	int u = b >> 8;
--	if(r == 0) f |= FZ;			// .Z......
--	int ra = r ^ a;
--	f |= u & FN;				// ......N.
--	f |= (ra ^ b ^ u) & FH;			// ...H....
--	if((a&~0xFF)==0) {
--		a = ra & (b ^ r);
--		b = 5;				// .....V..
--	} else {
--		a = 0x9669*FP;
--		b = (r ^ r>>>4)&0xF;		// .....P..
--	}
--	return f | a>>>b & FP;
--}
get_flags: Z80 -> Int
get_flags z80 =
    let
        f1 = z80.flags.ff
        a1 = z80.flags.fa
        b1 = z80.flags.fb
        r = z80.flags.fr
        lhs_f = Bitwise.and f1 (Bitwise.or c_FS c_F53)
        rhs_f = Bitwise.and (shiftRightBy8 f1) c_FC
        f2 = Bitwise.or lhs_f rhs_f
        u = shiftRightBy8 b1
        f3 = if r == 0 then
                Bitwise.or f2 c_FZ
             else
                f2
        ra = Bitwise.xor r a1
        f4 = Bitwise.or f3 (Bitwise.and u c_FN)
        f5 = Bitwise.or f4 (Bitwise.and (Bitwise.xor (Bitwise.xor ra b1) u) c_FN)
        (a, b) = if Bitwise.and a1 0xFFFFFF00 == 0 then
                    (Bitwise.and ra (Bitwise.xor b1 r), 5)
                 else
                    (0x9669*c_FP, Bitwise.and (Bitwise.xor r (shiftRightBy 4 r)) 0xF)
    in
        Bitwise.or f5 (Bitwise.and (shiftRightBy b a) c_FP)
--	private void flags(int f) {
--		Fr = ~f&FZ;
--		Ff = (f |= f<<8);
--		Fa = 0xFF & (Fb = f&~0x80 | (f&FP)<<5);
--	}
set_flags: Int -> Int -> FlagRegisters
set_flags tmp_f a =
    let
        fr = Bitwise.and (Bitwise.complement tmp_f) c_FZ
        ff = Bitwise.or tmp_f (shiftLeftBy8 tmp_f)
        fb = Bitwise.or (Bitwise.and ff (Bitwise.complement 0x80)) (shiftLeftBy (Bitwise.and ff c_FP) 5)
        fa = Bitwise.and 0xFF fb
    in
        { a = a, ff = ff, fr = fr, fa = fa, fb = fb }
--
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
    or (shiftLeftBy8 z80.flags.a) (get_flags z80)
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
--	void de(int v) {E=v&0xFF; D=v>>>8;}
set_de: Int -> Z80 -> Z80
set_de v z80 =
    let
        z80_main = z80.main
    in
        { z80 | main = { z80_main | d = shiftRightBy8 v, e = and v 0xFF } }
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

inc_pc: Z80 -> Z80
inc_pc z80 =
   { z80 | pc = Bitwise.and (z80.pc + 1) 0xFFFF }

inc_pc2: Z80 -> Z80
inc_pc2 z80 =
   { z80 | pc = Bitwise.and (z80.pc + 2) 0xFFFF }

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
      t = 0
   in
    { z80 | pc = Bitwise.and pc 0xFFFF }
--	void sp(int v) {SP = v;}
set_sp: Int -> Z80 -> Z80
set_sp sp z80 =
   { z80 | sp = Bitwise.and sp 0xFFFF }
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
      y = debug_log "set_iff" (String.fromInt value) Nothing
      interrupts = z80.interrupts
   in
      { z80 | interrupts = { interrupts | iff = value } }
--	void ei(boolean v) {IFF = v ? 3 : 0;}
--
--	int time;
--	int time_limit;
--
--	void exx() {
--		int tmp;
--		tmp = B_; B_ = B; B = tmp;
--		tmp = C_; C_ = C; C = tmp;
--		tmp = D_; D_ = D; D = tmp;
--		tmp = E_; E_ = E; E = tmp;
--		tmp = HL_; HL_ = HL; HL = tmp;
--	}
exx: Z80 -> Z80
exx z80 =
   { z80 | main = z80.alt_main, alt_main = z80.main }
--
--	void ex_af() {
--		int tmp = A_; A_ = A; A = tmp;
--		tmp = Ff_; Ff_ = Ff; Ff = tmp;
--		tmp = Fr_; Fr_ = Fr; Fr = tmp;
--		tmp = Fa_; Fa_ = Fa; Fa = tmp;
--		tmp = Fb_; Fb_ = Fb; Fb = tmp;
--	}
ex_af: Z80 -> Z80
ex_af z80 =
    { z80 | flags = z80.alt_flags, alt_flags = z80.flags }
--	public void push(int v) {
--		int sp;
--		time++;
--		env.mem((char)((sp=SP)-1), v>>>8);
--		time += 3;
--		env.mem(SP = (char)(sp-2), v&0xFF);
--		time += 3;
--	}
push: Int -> Z80 -> Z80
push v z80 =
   let
      --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
      z80_1 = z80 |> add_cpu_time 1
      env = z80_1.env |> set_mem (char (z80.sp - 1)) (shiftRightBy8 v)
      z80_2 = { z80_1 | env = env } |> add_cpu_time 3
      new_sp = char (z80.sp - 2)
   in
      { z80_2 | env = z80_2.env |> set_mem new_sp (and v 0xFF) } |> set_sp new_sp |> add_cpu_time 3
--
--	int pop() {
--		int sp, v = env.mem16(sp=SP);
--		SP = (char)(sp+2);
--		time += 6;
--		return v;
--	}
pop: Z80 -> IntWithZ80
pop z80 =
   let
      v = z80.env |> mem16 z80.sp
   in
      IntWithZ80 v.value ({ z80 | env = v.env } |> set_sp (z80.sp + 2) |> add_cpu_time 6)
--
--	private void add(int b)
--	{
--		A = Fr = (Ff = (Fa = A) + (Fb = b)) & 0xFF;
--	}
z80_add: Int -> FlagRegisters -> FlagRegisters
z80_add b flags =
   let
      fa = flags.a
      fb = b
      ff = fa + fb
      fr = and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }
--
--	private void adc(int b)
--	{
--		A = Fr = (Ff = (Fa = A) + (Fb = b) + (Ff>>>8 & FC)) & 0xFF;
--	}
adc: Int -> FlagRegisters -> FlagRegisters
adc b flags =
   let
      fa = flags.a
      fb = b
      ff = fa + fb + (and (shiftRightBy8 flags.ff) c_FC)
      fr = and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }
--
--	private void sub(int b)
--	{
--		Fb = ~b;
--		A = Fr = (Ff = (Fa = A) - b) & 0xFF;
--	}
z80_sub: Int -> FlagRegisters -> FlagRegisters
z80_sub b flags =
   let
      fb = complement b
      fa = flags.a
      ff = fa - b
      fr = and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }
--
--	private void sbc(int b)
--	{
--		Fb = ~b;
--		A = Fr = (Ff = (Fa = A) - b - (Ff>>>8 & FC)) & 0xFF;
--	}
sbc: Int -> FlagRegisters -> FlagRegisters
sbc b flags =
   let
      fb = complement b
      fa = flags.a
      ff = fa - b - (and (shiftRightBy8 flags.ff) c_FC)
      fr = and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }
--
--	private void cp(int b)
--	{
--		int r = (Fa = A) - b;
--		Fb = ~b;
--		Ff = r&~F53 | b&F53;
--		Fr = r&0xFF;
--	}
cp: Int -> FlagRegisters -> FlagRegisters
cp b flags =
   let
      fa = flags.a
      r = fa - b
      fb = complement b
      ff = or (and r (complement c_F53)) (and b c_F53)
      fr = and r 0xFF
   in
     { flags | fr = fr, ff = ff, fb = fb, fa = fa }
--
--	private void and(int b) {Fa = ~(A = Ff = Fr = A & b); Fb = 0;}
z80_and: Int -> FlagRegisters -> FlagRegisters
z80_and b flags =
   let
      fr = and flags.a b
      ff = fr
      a = ff
      fa = complement a
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }
--	private void or(int b) {Fa = (A = Ff = Fr = A | b) | 0x100; Fb = 0;}
z80_or: Int -> FlagRegisters -> FlagRegisters
z80_or b flags =
   let
      fr = Bitwise.or flags.a b
      ff = fr
      a = ff
      fa = or a 0x100
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }
--	private void xor(int b) {Fa = (A = Ff = Fr = A ^ b) | 0x100; Fb = 0;}
z80_xor: Int -> FlagRegisters -> FlagRegisters
z80_xor b flags =
   let
      fr = Bitwise.xor flags.a b
      ff = fr
      a = ff
      fa = or a 0x100
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }
--	private void cpl()
--	{
--		Ff = Ff&~F53 | (A ^= 0xFF)&F53;
--		Fb |= ~0x80; Fa = Fa&~FH | ~Fr&FH; // set H, N
--	}
cpl: FlagRegisters -> FlagRegisters
cpl flags =
   let
      new_a = Bitwise.xor flags.a 0xFF
      ff = or (and flags.ff (complement c_F53)) (and new_a c_F53)
      fb = or flags.fb (complement 0x80)
      fa = or (and flags.fa (complement c_FH)) (and (complement flags.fr) c_FH)
   in
     { flags | a = new_a, ff= ff, fb = fb, fa = fa }
--
--	private int inc(int v)
--	{
--		Ff = Ff&0x100 | (Fr = v = (Fa=v)+(Fb=1) & 0xFF);
--		return v;
--	}
inc: Int -> FlagRegisters -> IntWithFlags
inc v flags =
   let
      ff = Bitwise.and flags.ff 0x100
      vv = Bitwise.and (v + 1) 0xFF
      new_flags = { flags | ff = (Bitwise.or ff vv), fb = 1, fa = v, fr = vv }
   in
      IntWithFlags vv new_flags
--
--	private int dec(int v)
--	{
--		Ff = Ff&0x100 | (Fr = v = (Fa=v)+(Fb=-1) & 0xFF);
--		return v;
--	}
dec: Int -> FlagRegisters -> IntWithFlags
dec v flags =
   let
      ff = Bitwise.and flags.ff 0x100
      vv = Bitwise.and (v - 1) 0xFF
   in
      IntWithFlags vv { flags | ff = (Bitwise.or ff vv), fb = -1, fa = v, fr = vv }
--
--	private void bit(int n, int v)
--	{
--		int m = v & 1<<n;
--		Ff = Ff&~0xFF | v&F53 | m;
--		Fa = ~(Fr = m); Fb = 0;
--	}
bit: Int -> Int -> FlagRegisters -> FlagRegisters
bit n v flags =
   let
     m = and v (shiftLeftBy n 1)
     ff = or (and flags.ff (complement 0xFF)) (or (and v c_F53) m)
     fr = m
   in
     { flags | ff = ff, fr = fr, fa = complement fr, fb = 0 }
--
--	private void f_szh0n0p(int r)
--	{
--		// SZ5H3PNC
--		// xxx0xP0.
--		Ff = Ff&~0xFF | (Fr = r);
--		Fa = r|0x100; Fb = 0;
--	}
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
--	private void rot(int a)
--	{
--		Ff = Ff&0xD7 | a&0x128;
--		Fb &= 0x80; Fa = Fa&~FH | Fr&FH; // reset H, N
--		A = a&0xFF;
--	}
rot: Int -> FlagRegisters -> FlagRegisters
rot a flags =
   let
      ff = Bitwise.or (Bitwise.and flags.ff 0x07) (Bitwise.and a 0x128)
      fb = Bitwise.and flags.fb 0x80
      fa = Bitwise.or (Bitwise.and flags.fa (Bitwise.complement c_FH)) (Bitwise.and flags.fr c_FH)
   in
      { flags | ff = ff, fb = fb, fa = fa, a = (Bitwise.and a 0xFF) }
--
--	private int shifter(int o, int v)
--	{
--		switch(o&7) {
--		case 0: v = v*0x101>>>7; break;
--		case 1: v = v*0x80800000>>24; break;
--		case 2: v = v<<1|Ff>>>8&1; break;
--		case 3: v = (v*0x201|Ff&0x100)>>>1; break;
--		case 4: v <<= 1; break;
--		case 5: v = v>>>1|v&0x80|v<<8; break;
--		case 6: v = v<<1|1; break;
--		case 7: v = v*0x201>>>1; break;
--		}
--		Fa = 0x100 | (Fr = v = 0xFF&(Ff = v));
--		Fb = 0;
--		return v;
--	}
shifter: Int -> Int -> FlagRegisters -> IntWithFlags
shifter o v flags =
   let
      ff = case (and o 7) of
               0 -> shiftRightBy 7 (v * 0x101)
               1 -> shiftRightBy 24 (v * 0x80800000)
               2 -> or (shiftLeftBy 1 v) (and (shiftRightBy8 flags.ff) 1)
               3 -> shiftRightBy 1 (or (v * 0x201) (and flags.ff 0x100))
               4 -> shiftLeftBy 1 v
               5 -> or (or (shiftRightBy 1 v) (and v 0x80)) (shiftLeftBy8 v)
               6 -> or (shiftLeftBy 1 v) 1
               _ -> shiftRightBy 1 (v * 0x201)
      fr = and 0xFF ff
   in
      IntWithFlags fr { flags | fr = fr, fb = 0, fa = (or 0x100 fr) }
--
--	private int add16(int a, int b)
--	{
--		int r = a + b;
--		Ff = Ff & FS | r>>>8 & 0x128;
--		Fa &= ~FH;
--		Fb = Fb&0x80 | ((r ^ a ^ b)>>>8 ^ Fr) & FH;
--		MP = a+1;
--		time += 7;
--		return (char)r;
--	}
add16: Int -> Int -> FlagRegisters -> IntWithFlagsAndTime
add16 a b main_flags =
    let
        r = a + b
        ff = or (and main_flags.ff c_FS) (and (shiftRightBy8 r) 0x128)
        fa = and main_flags.fa (complement c_FH)
        shiftright = shiftRightBy8 (Bitwise.xor (Bitwise.xor r a) b)
        shiftrightxor = Bitwise.xor shiftright main_flags.fr
        fb_rhs = and shiftrightxor c_FH
        fb = or (and main_flags.fb 0x80) fb_rhs
        new_flags = { main_flags | ff = ff, fa = fa, fb = fb }
    in
        IntWithFlagsAndTime (and r 0xFFFF) new_flags 7
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
--	private int getd(int xy)
--	{
--		int d = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 8;
--		return MP = (char)(xy + (byte)d);
--	}
getd: Int -> Z80 -> IntWithZ80
getd xy z80 =
   let
      d = z80.env |> mem z80.pc
   in
      IntWithZ80 (char (xy + byte d.value)) ({ z80 | pc = (char (z80.pc + 1)), env = d.env } |> add_cpu_time 8)
--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}
imm8: Z80 -> IntWithZ80
imm8 z80 =
    let
        v = mem z80.pc z80.env
    in
        IntWithZ80 v.value ({ z80 | env = v.env, pc = (and (z80.pc + 1) 0xFFFF) } |> add_cpu_time 3)
--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}
imm16: Z80 -> IntWithZ80
imm16 z80 =
    let
        v = mem16 z80.pc z80.env
    in
        IntWithZ80 v.value ({ z80 | env = v.env } |> inc_pc2 |> add_cpu_time 6)
--
--	/* instructions */
--
--	// may be wrong. see http://scratchpad.wikia.com/wiki/Z80
--	private void scf_ccf(int x)
--	{
--		Fa &= ~FH;
--		Fb = Fb&0x80 | (x>>>4 ^ Fr) & FH;
--		Ff = 0x100 ^ x | Ff&FS | A&F53;
--	}
scf_ccf: Int -> FlagRegisters -> FlagRegisters
scf_ccf x flags =
   let
      fa = and flags.fa (complement c_FH)
      fb = or (and flags.fb 0x80) (and (Bitwise.xor (shiftRightBy 4 x) flags.fr) c_FH)
      ff = or (or (Bitwise.xor 0x100 x) (and flags.ff c_FS)) (and flags.a c_F53)
   in
      { flags | fa = fa, fb = fb, ff = ff }
--
--	private void daa()
--	{
--		int h = (Fr ^ Fa ^ Fb ^ Fb>>8) & FH;
--
--		int d = 0;
--		if((A | Ff&0x100) > 0x99) d = 0x160;
--		if((A&0xF | h) > 9) d += 6;
--
--		Fa = A | 0x100; // parity
--		if((Fb & 0x200)==0)
--			A += (Fb = d);
--		else {
--			A -= d; Fb = ~d;
--		}
--		Ff = (Fr = A &= 0xFF) | d&0x100;
--	}
daa: FlagRegisters -> FlagRegisters
daa flags =
   let
      h = Bitwise.and (Bitwise.xor (Bitwise.xor (Bitwise.xor flags.fr flags.fa) flags.fb) (shiftRightBy8 flags.fb)) c_FH
      d0 = if Bitwise.or flags.a (Bitwise.and flags.ff 0x100) > 0x99 then
             0x160
           else
             0
      d = if Bitwise.or (Bitwise.and flags.a 0xF) h > 9 then
             d0 + 6
          else
             d0
      fa = or flags.a 0x100
      (a0, fb) = if and flags.fb 0x200 == 0 then
                  (flags.a + d, d)
                else
                  (flags.a - d, complement d)
      a = and a0 0xFF
      fr = a
      ff = or fr (and d 0x100)
   in
      { flags | fr = fr, a = a, fb = fb, fa = fa, ff = ff }
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
--
--	private void ld_a_ir(int v)
--	{
--		Ff = Ff&~0xFF | (A = v);
--		Fr = v==0 ? 0 : 1;
--		Fa = Fb = IFF<<6 & 0x80;
--		time++;
--	}
--
--	private void jp(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) PC = a;
--	}
jp: Bool -> Z80 -> Z80
jp y z80 =
   let
      a = imm16 z80
      z80_1 = a.z80
   in
      if y then
         z80_1 |> set_pc a.value
      else
         z80_1
--
--	private void jr()
--	{
--		int pc = PC;
--		byte d = (byte)env.mem(pc); time += 8;
--		MP = PC = (char)(pc+d+1);
--	}
jr: Z80 -> Z80
jr z80 =
   let
      mempc = mem z80.pc z80.env
      d = byte mempc.value
      --x = Debug.log "jr" ((String.fromInt d.value) ++ " " ++ (String.fromInt (byte d.value)))
   in
      z80 |> set_env mempc.env |> add_cpu_time 8 |> set_pc (z80.pc + d + 1)
--
--	private void call(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) {push(PC); PC = a;}
--	}
call: Bool -> Z80 -> Z80
call y z80 =
   let
      a = imm16 z80
   in
     if y then
      let
         --b = debug_log "call" (a.value |> subName) Nothing
         j = 1
      in
         a.z80 |> push a.z80.pc |> set_pc a.value
     else
       a.z80
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
halt: Z80 -> Z80
halt z80 =
   let
      interrupts = z80.interrupts
      n = shiftRightBy 2 (z80.time_limit - z80.env.cpu_time + 3)
      z80_1 = if n > 0 then
                 -- turns out env.halt(n, r) just returns n...?
                 { z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
              else
                 z80
    in
      { z80_1 | interrupts = { interrupts | halted = True } }
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
      z80_1 = { z80 | env = v1.env, main = { main | hl = char (a1 + i) } } |> add_cpu_time 3
      a2 = z80_1 |> get_de
      env_1 = z80_1.env |> set_mem a2 v1.value
      z80_2 = z80_1 |> set_env env_1 |> set_de (char (a2 + i)) |> add_cpu_time 5

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
                        (0x80, z80_3 |> set_pc (z80_3.pc - 2) |> add_cpu_time 5)
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

toString: IXIYHL -> String
toString ixiyhl =
   case ixiyhl of
      IX -> "IX"
      IY -> "IY"
      HL -> "HL"

get_xy: IXIYHL -> Z80 -> Int
get_xy ixiyhl z80 =
   case ixiyhl of
      IX ->
         z80.ix
      IY ->
         z80.iy
      HL ->
         z80.main.hl

set_xy: Int -> IXIYHL -> Z80 -> Z80
set_xy value ixiyhl z80 =
   case ixiyhl of
      IX ->
         { z80 | ix = value }
      IY ->
         { z80 | iy = value }
      HL ->
         let
            main = z80.main
         in
            { z80 | main = { main | hl = value } }

set_h: Int -> IXIYHL -> Z80 -> Z80
set_h value ixiyhl z80 =
   let
      xy = get_xy ixiyhl z80
   in
      set_xy (or (and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80

set_l: Int -> IXIYHL -> Z80 -> Z80
set_l value ixiyhl z80 =
   let
     xy = get_xy ixiyhl z80
   in
     set_xy (or (and xy 0xFF00) value) ixiyhl z80

execute_lt40: Int -> IXIYHL -> Z80 -> Z80
execute_lt40 c ixiyhl z80 =
  let
    z80_flags = z80.flags
    z80_main = z80.main
  in
    case c of
      0x08 -> ex_af z80
      --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
      --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
      --PC=(char)v;} break;
      0x10 ->
         let
            z80_1 = z80 |> add_cpu_time 1
            v = z80_1.pc
            mem_value = mem v z80_1.env
            d = byte mem_value.value
            v2 = v + 1
            z80_2 = { z80_1 | env = mem_value.env } |> add_cpu_time 3
            b = and (z80_2.main.b - 1) 0xFF
            (z80_3, v3) = if b /= 0 then
                           (z80_2 |> add_cpu_time 5, v2 + d)
                         else
                           (z80_2, v2)
         in
           { z80_3 | main = { z80_main | b = b } } |> set_pc v3
      -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
      -- This is just an inlined jr() call
      0x18 -> let
                  mem_value = mem z80.pc z80.env
                  dest = z80.pc + 1 + (byte mem_value.value)
                  --x = if (dest |> subName |> (String.startsWith "CALL-SUB")) then
                  --      -- HL still need to be indirected, so not a subroutine address yet
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
                  z80 |> set_pc dest |> add_cpu_time 8
      --case 0x09: HL=add16(HL,B<<8|C); break;
      --case 0x09: xy=add16(xy,B<<8|C); break;
      0x09 -> let
                  xy = get_xy ixiyhl z80
                  new_xy = add16 xy (get_bc z80) z80.flags
                  new_z80 = set_xy new_xy.value ixiyhl z80
              in
                  { new_z80 | flags = new_xy.flags } |> add_cpu_time new_xy.time
      -- case 0x19: HL=add16(HL,D<<8|E); break;
      -- case 0x19: xy=add16(xy,D<<8|E); break;
      0x19 -> let
                  xy = get_xy ixiyhl z80
                  new_xy = add16 xy (get_de z80) z80.flags
                  new_z80 = set_xy new_xy.value ixiyhl z80
              in
                  { new_z80 | flags = new_xy.flags} |> add_cpu_time new_xy.time
      -- case 0x29: HL=add16(HL,HL); break;
      -- case 0x29: xy=add16(xy,xy); break;
      0x29 -> let
                  xy = get_xy ixiyhl z80
                  new_xy = add16 xy xy z80.flags
                  new_z80 = set_xy new_xy.value ixiyhl z80
              in
                  { new_z80 | flags = new_xy.flags } |> add_cpu_time new_xy.time
      --case 0x39: HL=add16(HL,SP); break;
      --case 0x39: xy=add16(xy,SP); break;
      0x39 -> let
                  xy = get_xy ixiyhl z80
                  new_xy = add16 xy z80.sp z80.flags
                  new_z80 = set_xy new_xy.value ixiyhl z80
              in
                  { new_z80 | flags = new_xy.flags }  |> add_cpu_time new_xy.time
      -- case 0x01: v=imm16(); B=v>>>8; C=v&0xFF; break;
      0x01 -> let
                  v = imm16 z80
                  new_z80 = v.z80 |> set_bc v.value
                  --x = debug_log ("LD BC," ++ (new_z80 |> get_bc |> toHexString)) "" Nothing
              in
                  { new_z80 | env = new_z80.env }
      --case 0x11: v=imm16(); D=v>>>8; E=v&0xFF; break;
      0x11 -> let
                  v = imm16 z80
                  new_z80 = v.z80 |> set_de v.value
              in
                  { new_z80 | env = new_z80.env }
      -- case 0x21: HL=imm16(); break;
      -- case 0x21: xy=imm16(); break;
      0x21 -> let
                  new_xy = imm16 z80
                  --x = debug_log ("LD " ++ (ixiyhl |> toString) ++ "," ++ (new_xy.value |> toHexString)) ("pc = " ++ (z80.pc |> toHexString)) Nothing
               in
                  set_xy new_xy.value ixiyhl new_xy.z80
      -- case 0x23: HL=(char)(HL+1); time+=2; break;
      -- case 0x23: xy=(char)(xy+1); time+=2; break;
      0x23 -> let
                xy = z80 |> get_xy ixiyhl
                --x = if z80.pc /= 0x11E7 then
                --        debug_log "INC HL" (z80.pc |> toHexString) Nothing
                --    else
                --        Nothing
              in
                z80 |> set_xy (char (xy + 1)) ixiyhl |> add_cpu_time 2
      -- case 0x2B: HL=(char)(HL-1); time+=2; break;
      -- case 0x2B: xy=(char)(xy-1); time+=2; break;
      0x2B -> let
                xy = get_xy ixiyhl z80
                new_xy = and (xy - 1) 0xFFFF
                new_z80 = set_xy new_xy ixiyhl z80
              in
                  new_z80 |> add_cpu_time 2
      -- case 0x31: SP=imm16(); break;
      0x31 ->
          let
              v = imm16 z80
              newz80 = v.z80
          in
              { newz80 | sp = v.value }
      -- case 0x33: SP=(char)(SP+1); time+=2; break;
      0x33 -> z80 |> set_sp (z80.sp + 1) |> add_cpu_time 2
      -- case 0x3B: SP=(char)(SP-1); time+=2; break;
      0x3B -> z80 |> set_sp (z80.sp - 1) |> add_cpu_time 2
      -- case 0x03: if(++C==256) {B=B+1&0xFF;C=0;} time+=2; break;
      0x03 ->
          let
              tmp_c = z80_main.c + 1
              (reg_b, reg_c) = if tmp_c == 256 then
                                  ((and (z80_main.b + 1) 0xFF), 0)
                               else
                                  (z80_main.b, tmp_c)
          in
              { z80 | main = { z80_main | b = reg_b, c = reg_c } } |> add_cpu_time 2
      -- case 0x13: if(++E==256) {D=D+1&0xFF;E=0;} time+=2; break;
      0x13 ->
          let
              tmp_e = z80_main.e + 1
              (reg_d, reg_e) = if tmp_e == 256 then
                                  ((and (z80_main.d + 1) 0xFF), 0)
                               else
                                  (z80_main.d, tmp_e)
          in
              { z80 | main = { z80_main | d = reg_d, e = reg_e } } |> add_cpu_time 2
      -- case 0x0B: if(--C<0) B=B-1&(C=0xFF); time+=2; break;
      0x0B ->
          let
              tmp_c = z80_main.c - 1
              (reg_b, reg_c) = if tmp_c < 0 then
                                  ((and (z80_main.b - 1) 0xFF), 0xFF)
                               else
                                  (z80_main.b, tmp_c)
          in
              { z80 | main = { z80_main | b = reg_b, c = reg_c }} |> add_cpu_time 2
      -- case 0x1B: if(--E<0) D=D-1&(E=0xFF); time+=2; break;
      0x1B ->
          let
              tmp_e = z80_main.e - 1
              (reg_d, reg_e) = if tmp_e < 0 then
                                  ((and (z80_main.d - 1) 0xFF), 0xFF)
                               else
                                  (z80_main.d, tmp_e)
          in
              { z80 | main = { z80_main | d = reg_d, e = reg_e } } |> add_cpu_time 2
      -- case 0x02: MP=(v=B<<8|C)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
      0x02 ->
          let
              addr = (shiftLeftBy8 z80_main.b) + z80_main.c
          in
              { z80 | env = set_mem addr z80.flags.a z80.env } |> add_cpu_time 3
      -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
      0x0A ->
          let
              v = or (shiftLeftBy8 z80_main.b) z80_main.c
              new_a = mem v z80.env
              new_flags = { z80_flags | a = new_a.value }
          in
              { z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
      -- case 0x12: MP=(v=D<<8|E)+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
      0x12 ->
          let
              addr = (shiftLeftBy8 z80_main.d) + z80_main.e
              env = set_mem addr z80.flags.a z80.env
          in
            { z80 | env = { env | cpu_time = env.cpu_time + 3 } }
      -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
      0x1A ->
          let
              addr = or (shiftLeftBy8 z80_main.d) z80_main.e
              new_a = mem addr z80.env
              main_flags = z80.flags
              new_flags = { main_flags | a = new_a.value }
          in
              { z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
      -- case 0x22: MP=(v=imm16())+1; env.mem16(v,HL); time+=6; break;
      0x22 ->
          let
             v = imm16 z80
             new_z80 = v.z80
             env = new_z80.env |> set_mem16 v.value new_z80.main.hl
             --x = debug_log "LD nn, HL" ((z80.pc |> toHexString) ++ " addr " ++ (v.value |> toHexString) ++ " " ++ (new_z80.main.hl |> toHexString)) env
          in
             new_z80 |> set_env env |> add_cpu_time 6
      -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
      -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
      0x2A ->
          let
             v = imm16 z80
             z80_1 = v.z80
             new_xy = mem16 v.value z80_1.env
             z80_2 = { z80_1 | env = new_xy.env }
          in
             z80_2 |> set_xy new_xy.value ixiyhl |> add_cpu_time 6
      -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
      0x32 ->
         let
            v = imm16 z80
            new_z80 = v.z80
         in
           { new_z80 | env = set_mem v.value new_z80.flags.a new_z80.env } |> add_cpu_time 3
      -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
      0x3A ->
         let
            v = imm16 z80
            new_z80 = v.z80
            env = v.z80.env
            mem_value = mem v.value env
         in
           { new_z80 | flags = { z80_flags | a = mem_value.value }, env = mem_value.env } |> add_cpu_time 3
      -- case 0x04: B=inc(B); break;
      0x04 ->
         let
            new_b = inc z80.main.b z80.flags
         in
            { z80 | flags = new_b.flags, main = { z80_main | b = new_b.value } }
      -- case 0x05: B=dec(B); break;
      0x05 ->
         let
            new_b = dec z80.main.b z80.flags
         in
            { z80 | flags = new_b.flags, main = { z80_main | b = new_b.value } }
      -- case 0x06: B=imm8(); break;
      0x06 ->
         let
            new_b = imm8 z80
            new_z80 = new_b.z80
         in
            { new_z80 | main = { z80_main | b = new_b.value } }
      -- case 0x0C: C=inc(C); break;
      0x0C ->
         let
            new_c = inc z80.main.c z80.flags
         in
            { z80 | flags = new_c.flags, main = { z80_main | c = new_c.value } }
      -- case 0x0D: C=dec(C); break;
      0x0D ->
         let
            new_c = dec z80.main.c z80.flags
         in
            { z80 | flags = new_c.flags, main = { z80_main | c = new_c.value } }
      -- case 0x0E: C=imm8(); break;
      0x0E ->
         let
            new_c = imm8 z80
            new_z80 = new_c.z80
         in
            { new_z80 | main = { z80_main | c = new_c.value } }
      -- case 0x14: D=inc(D); break;
      0x14 ->
         let
            new_d = inc z80.main.d z80.flags
         in
            { z80 | flags = new_d.flags, main = { z80_main | d = new_d.value } }
      -- case 0x15: D=dec(D); break;
      0x15 ->
         let
            new_d = dec z80.main.d z80.flags
         in
            { z80 | flags = new_d.flags, main = { z80_main | d = new_d.value } }
      -- case 0x16: D=imm8(); break;
      0x16 ->
         let
            new_d = imm8 z80
            new_z80 = new_d.z80
         in
            { new_z80 | main = { z80_main | d = new_d.value } }
      -- case 0x1C: E=inc(E); break;
      0x1C ->
         let
            new_e = inc z80.main.e z80.flags
         in
            { z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
      -- case 0x1D: E=dec(E); break;
      0x1D ->
         let
            new_e = dec z80.main.c z80.flags
         in
            { z80 | flags = new_e.flags, main = { z80_main | e = new_e.value } }
      -- case 0x1E: E=imm8(); break;
      0x1E ->
         let
            new_e = imm8 z80
            new_z80 = new_e.z80
         in
            { new_z80 | main = { z80_main | e = new_e.value } }
      -- case 0x24: HL=HL&0xFF|inc(HL>>>8)<<8; break;
      -- case 0x24: xy=xy&0xFF|inc(xy>>>8)<<8; break;
      0x24 ->
         let
            xy = get_xy ixiyhl z80
            value = inc (shiftRightBy8 xy) z80.flags
            z80_1 = { z80 | flags = value.flags }
            new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
         in
            set_xy new_xy ixiyhl z80_1
      -- case 0x25: HL=HL&0xFF|dec(HL>>>8)<<8; break;
      -- case 0x25: xy=xy&0xFF|dec(xy>>>8)<<8; break;
      0x25 ->
         let
            xy = get_xy ixiyhl z80
            value = dec (shiftRightBy8 xy) z80.flags
            z80_1 = { z80 | flags = value.flags }
            new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
         in
            set_xy new_xy ixiyhl z80_1
      -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
      -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
      0x26 ->
         let
            value = imm8 z80
            new_z80 = value.z80
            xy = get_xy ixiyhl new_z80
            new_xy = or (and xy 0xFF) (shiftLeftBy8 value.value)
         in
            set_xy new_xy ixiyhl new_z80
      -- case 0x2C: HL=HL&0xFF00|inc(HL&0xFF); break;
      -- case 0x2C: xy=xy&0xFF00|inc(xy&0xFF); break;
      0x2C ->
         let
            xy = get_xy ixiyhl z80
            h = and xy 0xFF00
            l = inc (and xy 0xFF) z80_flags
            z80_1 = { z80 | flags = l.flags }
            new_xy = or h l.value
         in
            set_xy new_xy ixiyhl z80_1
      -- case 0x2D: HL=HL&0xFF00|dec(HL&0xFF); break;
      -- case 0x2D: xy=xy&0xFF00|dec(xy&0xFF); break;
      0x2D ->
         let
            xy = get_xy ixiyhl z80
            h = and xy 0xFF00
            l = dec (and xy 0xFF) z80_flags
            new_z80 = { z80 | flags = l.flags }
            new_xy = or h l.value
         in
            set_xy new_xy ixiyhl new_z80
      -- case 0x2E: HL=HL&0xFF00|imm8(); break;
      -- case 0x2E: xy=xy&0xFF00|imm8(); break;
      0x2E ->
         let
            xy = get_xy ixiyhl z80
            h = and xy 0xFF00
            l = imm8 z80
            new_z80 = l.z80
            new_xy = or h l.value
         in
            set_xy new_xy ixiyhl new_z80
      -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
      -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
      0x34 ->
         let
            xy = get_xy ixiyhl z80
            a = case ixiyhl of
               IX ->
                  getd xy z80
               IY ->
                  getd xy z80
               HL ->
                  IntWithZ80 z80.main.hl z80
            value = mem a.value z80.env
            v = z80_flags |> inc value.value
            z80_1 = a.z80 |> add_cpu_time 4
            new_env = z80_1.env |> set_mem a.value v.value
         in
            { z80_1 | env = new_env, flags = v.flags } |> add_cpu_time 3
      -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
      -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
      0x35 ->
         let
            xy = get_xy ixiyhl z80
            a = case ixiyhl of
               IX ->
                  getd xy z80
               IY ->
                  getd xy z80
               HL ->
                  IntWithZ80 z80.main.hl z80
            z80_1 = a.z80
            value = mem a.value z80_1.env
            env_1 = value.env
            v = z80_flags |> dec value.value
            new_env = { env_1 | cpu_time = env_1.cpu_time + 4 } |> set_mem a.value v.value
            env_2 = { new_env | cpu_time = new_env.cpu_time + 3 }
         in
            { z80_1 | env = env_2, flags = v.flags }
      -- case 0x36: env.mem(HL,imm8()); time+=3; break;
      -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
      --	v=env.mem((char)(PC+1)); time+=5;
      --	env.mem(a,v); PC=(char)(PC+2); time+=3;} break;
      0x36 ->
         case ixiyhl of
            HL ->let
                   v = imm8 z80
                   new_z80 = v.z80
                   new_env = set_mem z80.main.hl v.value new_z80.env
                 in
                   new_z80 |> set_env new_env
            _ -> let
                   xy = get_xy ixiyhl z80
                   mempc = mem z80.pc z80.env
                   a = char (xy + byte mempc.value)
                   z80_1 = z80 |> set_env mempc.env |> add_cpu_time 3
                   v = mem (char (z80_1.pc + 1)) z80_1.env
                   z80_2 = z80_1 |> set_env mempc.env |> add_cpu_time 5
                   x = set_mem a v.value z80_2.env
                 in
                   z80_2 |> set_env x |> inc_pc2 |> add_cpu_time 3
      -- case 0x3C: A=inc(A); break;
      0x3C ->
         let
            v = inc z80.flags.a z80.flags
            new_flags = v.flags
         in
            { z80 | flags = { new_flags | a = v.value } }
            -- case 0x3D: A=dec(A); break;
      0x3D ->
         let
            v = dec z80.flags.a z80.flags
            new_flags = v.flags
         in
            { z80 | flags = { new_flags | a = v.value } }
      -- case 0x3E: A=imm8(); break;
      0x3E ->
         let
            v = imm8 z80
            new_z80 = v.z80
         in
            { new_z80 | flags = { z80_flags | a = v.value } }
      -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
      0x20 ->
         if z80.flags.fr /= 0 then
            jr z80
         else
            (imm8 z80).z80
      -- case 0x28: if(Fr==0) jr(); else imm8(); break;
      0x28 ->
         if z80.flags.fr == 0 then
            jr z80
         else
            (imm8 z80).z80
      -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
      0x30 ->
         if (and z80.flags.ff 0x100) == 0 then
            jr z80
         else
            let
               v = imm8 z80
            in
               v.z80
      -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
      0x38 ->
         if (and z80.flags.ff 0x100) /= 0 then
            jr z80
         else
            let
               v = imm8 z80
            in
               v.z80
      -- case 0x07: rot(A*0x101>>>7); break;
      0x07 ->
         { z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 7 (z80.flags.a * 0x101)) }
      -- case 0x0F: rot(A*0x80800000>>24); break;
      0x0F ->
         { z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 24 (z80.flags.a * 0x80800000)) }
      -- case 0x17: rot(A<<1|Ff>>>8&1); break;
      0x17 ->
         { z80 | flags = z80.flags |> rot (Bitwise.or (Bitwise.shiftLeftBy 1 z80.flags.a)
                                                                              (Bitwise.and (shiftRightBy8 z80.flags.ff) 1)) }
      -- case 0x1F: rot((A*0x201|Ff&0x100)>>>1); break;
      0x1F ->
         { z80 | flags = z80.flags |> rot (Bitwise.shiftRightBy 1 (Bitwise.or (z80.flags.a * 0x201)
                                                                              (Bitwise.and z80.flags.ff 0x100))) }
      -- case 0x27: daa(); break;
      0x27 ->
         { z80 | flags = daa z80.flags }
      -- case 0x2F: cpl(); break;
      0x2F ->
         { z80 | flags = cpl z80.flags }
      -- case 0x37: scf_ccf(0); break;
      0x37 ->
         let
            x = debug_log "scf" "1" Nothing
         in
            { z80 | flags = scf_ccf 0 z80.flags }
      -- case 0x3F: scf_ccf(Ff&0x100); break;
      0x3F ->
         { z80 | flags = scf_ccf (and z80.flags.ff 0x100) z80.flags }
      _ -> z80

executegt40ltC0: Int -> IXIYHL -> Z80 -> Z80
executegt40ltC0 c ixiyhl z80 =
   let
      shiftRight3 =  shiftRightBy 3 (c - 0x40)
   in
      --// case 0x40: break;
      -- case 0x41: B=C; break;
      -- case 0x42: B=D; break;
      -- case 0x43: B=E; break;
      -- case 0x44: B=HL>>>8; break;
      -- case 0x44: B=xy>>>8; break;
      -- case 0x45: B=HL&0xFF; break;
      -- case 0x45: B=xy&0xFF; break;
      -- case 0x46: B=env.mem(HL); time+=3; break;
      -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
      -- case 0x47: B=A; break;
      -- case 0x48: C=B; break;
      --// case 0x49: break;
      -- case 0x4A: C=D; break;
      -- case 0x4B: C=E; break;
      -- case 0x4C: C=HL>>>8; break;
      -- case 0x4D: C=HL&0xFF; break;
      -- case 0x4E: C=env.mem(HL); time+=3; break;
      -- case 0x4F: C=A; break;
      -- case 0x50: D=B; break;
      -- case 0x51: D=C; break;
      --// case 0x52: break;
      -- case 0x53: D=E; break;
      -- case 0x54: D=HL>>>8; break;
      -- case 0x55: D=HL&0xFF; break;
      -- case 0x56: D=env.mem(HL); time+=3; break;
      -- case 0x57: D=A; break;
      -- case 0x58: E=B; break;
      -- case 0x59: E=C; break;
      -- case 0x5A: E=D; break;
      --// case 0x5B: break;
      -- case 0x5C: E=HL>>>8; break;
      -- case 0x5D: E=HL&0xFF; break;
      -- case 0x5E: E=env.mem(HL); time+=3; break;
      -- case 0x5F: E=A; break;
      case shiftRight3 of
         0x00 ->
            let
                value = load408bit c ixiyhl z80
            in
                set408bit shiftRight3 value.value ixiyhl value.z80
         0x01 ->
            let
                value = load408bit c ixiyhl z80
            in
                set408bit shiftRight3 value.value ixiyhl value.z80
         0x02 ->
            let
                value = load408bit c ixiyhl z80
            in
                set408bit shiftRight3 value.value ixiyhl value.z80
         0x03 ->
            let
                value = load408bit c ixiyhl z80
            in
                set408bit shiftRight3 value.value ixiyhl value.z80
         -- case 0x60: HL=HL&0xFF|B<<8; break;
         -- case 0x60: xy=xy&0xFF|B<<8; break;
         -- case 0x61: HL=HL&0xFF|C<<8; break;
         -- case 0x61: xy=xy&0xFF|C<<8; break;
         -- case 0x62: HL=HL&0xFF|D<<8; break;
         -- case 0x62: xy=xy&0xFF|D<<8; break;
         -- case 0x63: HL=HL&0xFF|E<<8; break;
         -- case 0x63: xy=xy&0xFF|E<<8; break;
         --// case 0x64: break;
         -- case 0x65: HL=HL&0xFF|(HL&0xFF)<<8; break;
         -- case 0x65: xy=xy&0xFF|(xy&0xFF)<<8; break;
         -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
         -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
         -- case 0x67: HL=HL&0xFF|A<<8; break;
         -- case 0x67: xy=xy&0xFF|A<<8; break;
         0x04 ->
            let
              value = load408bit c ixiyhl z80
              new_z80 = value.z80
            in
              if c == 0x66 || ixiyhl == HL then
                set_h value.value HL new_z80
              else
                set_h value.value ixiyhl new_z80
         -- case 0x68: HL=HL&0xFF00|B; break;
         -- case 0x68: xy=xy&0xFF00|B; break;
         -- case 0x69: HL=HL&0xFF00|C; break;
         -- case 0x69: xy=xy&0xFF00|C; break;
         -- case 0x6A: HL=HL&0xFF00|D; break;
         -- case 0x6A: xy=xy&0xFF00|D; break;
         -- case 0x6B: HL=HL&0xFF00|E; break;
         -- case 0x6B: xy=xy&0xFF00|E; break;
         -- case 0x6C: HL=HL&0xFF00|HL>>>8; break;
         -- case 0x6C: xy=xy&0xFF00|xy>>>8; break;
         --// case 0x6D: break;
         -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
         -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
         -- case 0x6F: HL=HL&0xFF00|A; break;
         -- case 0x6F: xy=xy&0xFF00|A; break;
         0x05 ->
            let
                value = load408bit c ixiyhl z80
                new_z80 = value.z80
            in
                if c == 0x6E || ixiyhl == HL then
                  set_l value.value HL new_z80
                else
                  set_l value.value ixiyhl new_z80
         0x06 ->
         -- case 0x70: env.mem(HL,B); time+=3; break;
         -- case 0x70: env.mem(getd(xy),B); time+=3; break;
         -- case 0x71: env.mem(HL,C); time+=3; break;
         -- case 0x71: env.mem(getd(xy),C); time+=3; break;
         -- case 0x72: env.mem(HL,D); time+=3; break;
         -- case 0x72: env.mem(getd(xy),D); time+=3; break;
         -- case 0x73: env.mem(HL,E); time+=3; break;
         -- case 0x73: env.mem(getd(xy),E); time+=3; break;
         -- case 0x74: env.mem(HL,HL>>>8); time+=3; break;
         -- case 0x74: env.mem(getd(xy),HL>>>8); time+=3; break;
         -- case 0x75: env.mem(HL,HL&0xFF); time+=3; break;
         -- case 0x75: env.mem(getd(xy),HL&0xFF); time+=3; break;
         -- case 0x76: halt(); break;
         -- case 0x77: env.mem(HL,A); time+=3; break;
         -- case 0x77: env.mem(getd(xy),A); time+=3; break;
            if c == 0x76 then
               halt z80
            else
               let
                   value = load408bit c HL z80
                   z80_1 = value.z80
                   xy = get_xy ixiyhl z80_1
                   mem_target = if ixiyhl == HL then
                                  IntWithZ80 z80_1.main.hl z80_1
                                else
                                  getd xy z80_1
                   new_z80 = mem_target.z80
                   new_env = set_mem mem_target.value value.value new_z80.env
               in
                   { new_z80 | env = new_env } |> add_cpu_time 3
         -- case 0x78: A=B; break;
         -- case 0x79: A=C; break;
         -- case 0x7A: A=D; break;
         -- case 0x7B: A=E; break;
         -- case 0x7C: A=HL>>>8; break;
         -- case 0x7C: A=xy>>>8; break;
         -- case 0x7D: A=HL&0xFF; break;
         -- case 0x7D: A=xy&0xFF; break;
         -- case 0x7E: A=env.mem(HL); time+=3; break;
         -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
         --// case 0x7F: break;
         0x07 ->
            let
                value = load408bit c ixiyhl z80
                new_z80 = value.z80
            in
                set408bit shiftRight3 value.value ixiyhl new_z80
         -- case 0x80: add(B); break;
         -- case 0x81: add(C); break;
         -- case 0x82: add(D); break;
         -- case 0x83: add(E); break;
         -- case 0x84: add(HL>>>8); break;
         -- case 0x84: add(xy>>>8); break;
         -- case 0x85: add(HL&0xFF); break;
         -- case 0x85: add(xy&0xFF); break;
         -- case 0x86: add(env.mem(HL)); time+=3; break;
         -- case 0x86: add(env.mem(getd(xy))); time+=3; break;
         -- case 0x87: add(A); break;
         0x08 ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
                new_flags = z80_add value.value newish_z80.flags
            in
                { newish_z80 | flags = new_flags }
         -- case 0x88: adc(B); break;
         -- case 0x89: adc(C); break;
         -- case 0x8A: adc(D); break;
         -- case 0x8B: adc(E); break;
         -- case 0x8C: adc(HL>>>8); break;
         -- case 0x8C: adc(xy>>>8); break;
         -- case 0x8D: adc(HL&0xFF); break;
         -- case 0x8D: adc(xy&0xFF); break;
         -- case 0x8E: adc(env.mem(HL)); time+=3; break;
         -- case 0x8E: adc(env.mem(getd(xy))); time+=3; break;
         -- case 0x8F: adc(A); break;
         0x09 ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
                new_flags = adc value.value newish_z80.flags
            in
                { newish_z80 | flags = new_flags }
         -- case 0x90: sub(B); break;
         -- case 0x91: sub(C); break;
         -- case 0x92: sub(D); break;
         -- case 0x93: sub(E); break;
         -- case 0x94: sub(HL>>>8); break;
         -- case 0x94: sub(xy>>>8); break;
         -- case 0x95: sub(HL&0xFF); break;
         -- case 0x95: sub(xy&0xFF); break;
         -- case 0x96: sub(env.mem(HL)); time+=3; break;
         -- case 0x96: sub(env.mem(getd(xy))); time+=3; break;
         -- case 0x97: sub(A); break;
         0x0A ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
                new_flags = z80_sub value.value newish_z80.flags
            in
                { newish_z80 | flags = new_flags }
         -- case 0x98: sbc(B); break;
         -- case 0x99: sbc(C); break;
         -- case 0x9A: sbc(D); break;
         -- case 0x9B: sbc(E); break;
         -- case 0x9C: sbc(HL>>>8); break;
         -- case 0x9C: sbc(xy>>>8); break;
         -- case 0x9D: sbc(HL&0xFF); break;
         -- case 0x9D: sbc(xy&0xFF); break;
         -- case 0x9E: sbc(env.mem(HL)); time+=3; break;
         -- case 0x9E: sbc(env.mem(getd(xy))); time+=3; break;
         -- case 0x9F: sbc(A); break;
         0x0B ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
            in
                { newish_z80 | flags = newish_z80.flags |> sbc value.value }
         -- case 0xA0: and(B); break;
         -- case 0xA1: and(C); break;
         -- case 0xA2: and(D); break;
         -- case 0xA3: and(E); break;
         -- case 0xA4: and(HL>>>8); break;
         -- case 0xA4: and(xy>>>8); break;
         -- case 0xA5: and(HL&0xFF); break;
         -- case 0xA5: and(xy&0xFF); break;
         -- case 0xA6: and(env.mem(HL)); time+=3; break;
         -- case 0xA6: and(env.mem(getd(xy))); time+=3; break;
         -- case 0xA7: Fa=~(Ff=Fr=A); Fb=0; break;
         0x0C ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
                new_flags = z80_and value.value newish_z80.flags
            in
                { newish_z80 | flags = new_flags }
         -- case 0xA8: xor(B); break;
         -- case 0xA9: xor(C); break;
         -- case 0xAA: xor(D); break;
         -- case 0xAB: xor(E); break;
         -- case 0xAC: xor(HL>>>8); break;
         -- case 0xAC: xor(xy>>>8); break;
         -- case 0xAD: xor(HL&0xFF); break;
         -- case 0xAD: xor(xy&0xFF); break;
         -- case 0xAE: xor(env.mem(HL)); time+=3; break;
         -- case 0xAE: xor(env.mem(getd(xy))); time+=3; break;
         -- case 0xAF: A=Ff=Fr=Fb=0; Fa=0x100; break;
         0x0D ->
            let
                value = load408bit c ixiyhl z80
                newish_z80 = value.z80
                new_flags = z80_xor value.value newish_z80.flags
            in
                { newish_z80 | flags = new_flags }
         -- case 0xB0: or(B); break;
         -- case 0xB1: or(C); break;
         -- case 0xB2: or(D); break;
         -- case 0xB3: or(E); break;
         -- case 0xB4: or(HL>>>8); break;
         -- case 0xB4: or(xy>>>8); break;
         -- case 0xB5: or(HL&0xFF); break;
         -- case 0xB5: or(xy&0xFF); break;
         -- case 0xB6: or(env.mem(HL)); time+=3; break;
         -- case 0xB6: or(env.mem(getd(xy))); time+=3; break;
         -- case 0xB7: or(A); break;
         0x0E ->
            let
                value = load408bit c ixiyhl z80
                z80_1 = value.z80
            in
                { z80_1 | flags = z80_1.flags |> z80_or value.value }
         -- case 0xB8: cp(B); break;
         -- case 0xB9: cp(C); break;
         -- case 0xBA: cp(D); break;
         -- case 0xBB: cp(E); break;
         -- case 0xBC: cp(HL>>>8); break;
         -- case 0xBC: cp(xy>>>8); break;
         -- case 0xBD: cp(HL&0xFF); break;
         -- case 0xBD: cp(xy&0xFF); break;
         -- case 0xBE: cp(env.mem(HL)); time+=3; break;
         -- case 0xBE: cp(env.mem(getd(xy))); time+=3; break;
         -- case 0xBF: cp(A); break;
         _ ->
            let
                value = load408bit c ixiyhl z80
                z80_1 = value.z80
            in
                { z80_1 | flags = z80_1.flags |> cp value.value }

load408bit: Int -> IXIYHL -> Z80 -> IntWithZ80
load408bit c_value ixiyhl z80 =
   case (and c_value 0x07) of
      0 -> IntWithZ80 z80.main.b z80
      1 -> IntWithZ80 z80.main.c z80
      2 -> IntWithZ80 z80.main.d z80
      3 -> IntWithZ80 z80.main.e z80
      4 -> IntWithZ80 (shiftRightBy8 (get_xy ixiyhl z80)) z80
      5 -> IntWithZ80 (and (get_xy ixiyhl z80) 0xFF) z80
      6 -> let
            xy = get_xy ixiyhl z80
            a = case ixiyhl of
               HL ->
                  IntWithZ80 z80.main.hl z80
               _ ->
                  z80 |> getd xy
            new_z80 = a.z80
            new_b = mem a.value new_z80.env
           in
               IntWithZ80 new_b.value { new_z80 | env = new_b.env }
      -- 7 is the last (and only) possibility after and-ing with 0x07
      _ -> IntWithZ80 z80.flags.a z80

set408bit: Int -> Int -> IXIYHL -> Z80 -> Z80
set408bit c value ixiyhl z80 =
   let
      z80_main = z80.main
      z80_flags = z80.flags
   in
      case (and c 0x07) of
         0 -> { z80 | main = { z80_main | b = value } }
         1 -> { z80 | main = { z80_main | c = value } }
         2 -> { z80 | main = { z80_main | d = value } }
         3 -> { z80 | main = { z80_main | e = value } }
         4 -> set_h value ixiyhl z80
         5 -> set_l value ixiyhl z80
         6 -> { z80 | env = set_mem z80_main.hl value z80.env }
         _ -> { z80 | flags = { z80_flags | a = value } }

execute_gtc0: Int -> IXIYHL -> Z80 -> Z80
execute_gtc0 c ixiyhl z80 =
   case c of
   -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
      0xC0 ->
         let
            z80_1 =  z80 |> add_cpu_time 1
         in
            if z80_1.flags.fr /= 0 then
               let
                  result = z80_1 |> pop
                  x = debug_log "ret nz" (result.value |> subName) Nothing
               in
                  result.z80 |> set_pc result.value
            else
               z80_1
      -- case 0xC2: jp(Fr!=0); break;
      0xC2 -> jp (z80.flags.fr /= 0) z80
      -- case 0xC4: call(Fr!=0); break;
      0xC4 -> call (z80.flags.fr /= 0) z80
      -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
      0xC8 ->
         let
            z80_1 = z80 |> add_cpu_time 1
         in
            if z80_1.flags.fr == 0 then
               let
                  popped = z80_1 |> pop
               in
                  popped.z80 |> set_pc popped.value
            else
               z80_1
      -- case 0xCA: jp(Fr==0); break;
      0xCA -> jp (z80.flags.fr == 0) z80
      -- case 0xCC: call(Fr==0); break;
      0xCC -> call (z80.flags.fr == 0) z80
      -- case 0xD0: time++; if((Ff&0x100)==0) MP=PC=pop(); break;
      0xD0 -> let
                 z80_1 = z80 |> add_cpu_time 1
              in
                 if (and z80.flags.ff 0x100) == 0 then
                   let
                     popped = z80_1 |> pop
                     x = debug_log "ret nc" (popped.value |> subName) Nothing
                   in
                     popped.z80 |> set_pc popped.value
                 else
                   z80_1
      -- case 0xF3: IFF=0; break;
      0xF3 -> z80 |> set_iff 0
      -- case 0xC3: MP=PC=imm16(); break;
      0xC3 -> let
                  v = imm16 z80
                  --y = debug_log "jp" (v.value |> subName) Nothing
              in
                  v.z80 |> set_pc v.value
      -- case 0xD3: env.out(v=imm8()|A<<8,A); MP=v+1&0xFF|v&0xFF00; time+=4; break;
      0xD3 -> let
                  value = imm8 z80
                  z80_1 = value.z80
                  v = or value.value (shiftLeftBy8 z80.flags.a)
                  env = out v z80.flags.a z80_1.env
              in
                  { z80_1 | env = env } |> add_cpu_time 4
      -- case 0xD9: exx(); break;
      0xD9 -> z80 |> exx
      -- case 0xEB: v=HL; HL=D<<8|E; D=v>>>8; E=v&0xFF; break;
      0xEB -> let
                 v = z80.main.hl
                 de = z80 |> get_de
                 --x = debug_log "EX DE,HL" ("DE " ++ (v |> toHexString) ++ " HL " ++ (de |> toHexString)) Nothing
              in
                 z80 |> set_de v |> set_hl de
      -- case 0xF9: SP=HL; time+=2; break;
      0xF9 -> z80 |> set_sp z80.main.hl |> add_cpu_time 2
      -- case 0xFB: IFF=3; break;
      0xFB -> z80 |> set_iff 3
      -- case 0xC6: add(imm8()); break;
      0xC6 -> let
                 v = imm8 z80
                 z80_1 = v.z80
              in
                 { z80_1 | flags = z80_1.flags |> z80_add v.value }
      -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
      0xCD -> let
                v = z80 |> imm16
                --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
              in
                v.z80 |> push v.z80.pc |> set_pc v.value
      -- case 0xC5: push(B<<8|C); break;
      0xC5 -> z80 |> push (z80 |> get_bc)
      -- case 0xE6: and(imm8()); break;
      0xE6 -> let
                 a = z80 |> imm8
                 z80_1 = a.z80
                 flags = z80_1.flags |> z80_and a.value
              in
                 { z80_1 | flags = flags }
      -- case 0xF6: or(imm8()); break;
      0xF6 -> let
                 a = z80 |> imm8
                 z80_1 = a.z80
                 flags = z80_1.flags |> z80_or a.value
              in
                 { z80_1 | flags = flags }
      -- case 0xC9: MP=PC=pop(); break;
      0xC9 -> let
                a = z80 |> pop
                --b = debug_log "ret" (a.value |> subName) Nothing
              in
                a.z80 |> set_pc a.value
      -- case 0xF5: push(A<<8|flags()); break;
      0xF5 -> let
                a = z80 |> get_af
              in
                z80 |> push a
      -- case 0xC7:
      -- case 0xCF:
      -- case 0xD7:
      -- case 0xDF:
      -- case 0xE7:
      -- case 0xEF:
      -- case 0xF7:
      -- case 0xFF: push(PC); PC=c-199; break;
      0xC7 -> z80 |> push z80.pc |> set_pc (c - 199)
      0xCF -> z80 |> push z80.pc |> set_pc (c - 199)
      0xD7 -> z80 |> push z80.pc |> set_pc (c - 199)
      0xDF -> z80 |> push z80.pc |> set_pc (c - 199)
      0xE7 -> z80 |> push z80.pc |> set_pc (c - 199)
      0xEF -> z80 |> push z80.pc |> set_pc (c - 199)
      0xF7 -> z80 |> push z80.pc |> set_pc (c - 199)
      0xFF -> z80 |> push z80.pc |> set_pc (c - 199)
      -- case 0xE1: HL=pop(); break;
      0xE1 -> let
                  hl = z80 |> pop
              in
                  hl.z80 |> set_hl hl.value
      -- case 0xE5: push(HL); break;
      0xE5 -> z80 |> push z80.main.hl
      -- case 0xC1: v=pop(); B=v>>>8; C=v&0xFF; break;
      0xC1 -> let
                 v = z80 |> pop
                 --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
              in
                 v.z80 |> set_bc v.value
      -- case 0xE3: v=pop(); push(HL); MP=HL=v; time+=2; break;
      0xE3 -> let
                 v = z80 |> pop
              in
                 v.z80 |> push v.z80.main.hl |> set_hl v.value |> add_cpu_time 2
      -- case 0xF1: af(pop()); break;
      0xF1 -> let
                  v = z80 |> pop
              in
                  v.z80 |> set_af v.value
      -- case 0xFE: cp(imm8()); break;
      0xFE -> let
                 v = z80 |> imm8
                 flags = v.z80.flags |> cp v.value
              in
                 v.z80 |> set_flag_regs flags
      -- case 0xD8: time++; if((Ff&0x100)!=0) MP=PC=pop(); break;
      0xD8 -> let
                 z80_1 = z80 |> add_cpu_time 1
                 z80_2 = if and z80_1.flags.ff 0x100 /= 0 then
                           let
                              v = z80_1 |> pop
                           in
                              debug_log "ret c" (v.value |> subName) (v.z80 |> set_pc v.value)
                         else
                           z80_1
              in
                 z80_2
      -- case 0xD5: push(D<<8|E); break;
      0xD5 -> z80 |> push (z80 |> get_de)
      -- case 0xE9: PC=HL; break;
      -- case 0xE9: PC=xy; break;
      0xE9 -> let
                  xy = z80 |> get_xy ixiyhl
                  --a = if Dict.member xy Z80Rom.c_COMMON_NAMES then
                  --      Nothing
                  --    else
                  --      debug_log ("JP (" ++ (toString ixiyhl) ++ ")") (xy |> subName) Nothing
              in
                 z80 |> set_pc xy
      -- case 0xD2: jp((Ff&0x100)==0); break;
      0xD2 -> z80 |> jp ((Bitwise.and z80.flags.ff 0x100) == 0)
      -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
      0xD1 -> let
                 v = z80 |> pop
              in
                 v.z80 |> set_de v.value
      -- case 0xDB: MP=(v=imm8()|A<<8)+1; A=env.in(v); time+=4; break;
      0xDB -> let
                 imm8val = z80 |> imm8
                 z80_1 = imm8val.z80
                 v = or imm8val.value (shiftLeftBy8 z80_1.flags.a)
                 a = z80_1.env |> z80_in v
                 flags = z80_1.flags
              in
                 { z80_1 | env = a.env, flags = { flags | a = a.value } }
      -- case 0xF8: time++; if((Ff&FS)!=0) MP=PC=pop(); break;
      0xF8 -> let
                 z80_1 = z80 |> add_cpu_time 1
                 z80_2 = if (and z80_1.flags.ff c_FS) /= 0 then
                           let
                              popped = z80_1 |> pop
                           in
                              popped.z80 |> set_pc popped.value
                         else
                           z80_1
              in
                 z80_2
      -- case 0xEE: xor(imm8()); break;
      0xEE -> let
                 v = z80 |> imm8
                 z80_1 = v.z80
                 flags = z80_1.flags |> z80_xor v.value
              in
                 { z80_1 | flags = flags }
      -- case 0xD6: sub(imm8()); break;
      0xD6 -> let
                v = z80 |> imm8
                flags = v.z80.flags |> z80_sub v.value
              in
                 v.z80 |> set_flag_regs flags
      ---- case 0xDC: call((Ff&0x100)!=0); break;
      --0xDC -> z80 |> call (Bitwise.and z80.flags.ff 0x100 /= 0)
      ---- case 0xF2: jp((Ff&FS)==0); break;
      --0xF2 -> z80 |> jp (Bitwise.and z80.flags.ff c_FS == 0)
      ---- case 0xFA: jp((Ff&FS)!=0); break;
      --0xFA -> z80 |> jp (Bitwise.and z80.flags.ff c_FS /= 0)
      ---- case 0xCE: adc(imm8()); break;
      --0xCE -> let
      --           v = z80 |> imm8
      --           flags = v.z80.flags |> adc v.value
      --        in
      --           v.z80 |> set_flag_regs flags
      _ -> debug_todo "execute" (c |> toHexString) z80

execute_instruction: Z80 -> Z80
execute_instruction tmp_z80 =
      --int v, c = env.m1(PC, IR|R++&0x7F);
      --PC = (char)(PC+1); time += 4;
      --switch(c) {
    let
       interrupts = tmp_z80.interrupts
       c = tmp_z80.env |> m1 tmp_z80.pc (or interrupts.ir (and interrupts.r 0x7F))
       z80 = { tmp_z80 | env = c.env, interrupts = { interrupts | r = interrupts.r + 1 } } |> inc_pc |> add_cpu_time 4
    in
      if c.value < 0x40 then
        execute_lt40 c.value HL z80
      else if c.value < 0xC0 then
        executegt40ltC0 c.value HL z80
      else
         case c.value of
            0xDD -> group_xy IX z80
            0xFD -> group_xy IY z80
            0xCB -> group_cb z80
            0xED -> group_ed z80
            _ -> execute_gtc0 c.value HL z80
-- case 0xD4: call((Ff&0x100)==0); break;
-- case 0xDA: jp((Ff&0x100)!=0); break;
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

execute: Z80 -> Z80
execute z80 =
    if z80.interrupts.halted then
        halt z80
    else
        Loop.while (\x -> x.time_limit - x.env.cpu_time > 0) execute_instruction z80
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
group_xy: IXIYHL -> Z80 -> Z80
group_xy ixiy old_z80 =
   let
      --xy = if c0 == 0xDD then
      --       old_z80.ix
      --     else
      --       old_z80.iy
      c = m1 old_z80.pc (or old_z80.interrupts.ir (and old_z80.interrupts.r 0x7F)) old_z80.env
      intr = old_z80.interrupts
      z80 = { old_z80 | env = c.env, interrupts = { intr | r = intr.r + 1 } } |> inc_pc |> add_cpu_time 4
   in
      if c.value < 0x40 then
         execute_lt40 c.value ixiy z80
      else if c.value < 0xC0 then
         executegt40ltC0 c.value ixiy z80
      -- case 0xDD:
      -- case 0xFD: c0=c; continue;
      else if c.value == 0xDD then
         group_xy IX z80
      else if c.value == 0xFD then
         group_xy IY z80
      else if c.value == 0xCB then
         group_xy_cb ixiy z80
      else if c.value == 0xED then
         group_ed z80
      else
         execute_gtc0 c.value ixiy z80
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
group_ed: Z80 -> Z80
group_ed z80_0 =
   let
      ints = z80_0.interrupts
      c = m1 z80_0.pc (or z80_0.interrupts.ir (and z80_0.interrupts.r 0x7F)) z80.env
      new_r = z80_0.interrupts.r + 1
      z80 = { z80_0 | interrupts = { ints | r = new_r } } |> inc_pc |> add_cpu_time 4
   in
      --// -------------- >8 ed
      case c.value of
      -- case 0x47: i(A); time++; break;
      0x47 -> z80 |> set_i z80.flags.a |> add_cpu_time 1
      -- case 0x52: sbc_hl(D<<8|E); break;
      0x52 -> z80 |> sbc_hl (or (shiftLeftBy8 z80.main.d) z80.main.e)
      -- case 0x43: MP=(v=imm16())+1; env.mem16(v,B<<8|C); time+=6; break;
      0x43 -> let
                 v = imm16 z80
                 z80_1 = v.z80
                 env = set_mem16 v.value (or (shiftLeftBy8 z80.main.b) z80.main.c) z80_1.env
              in
                 { z80_1 | env = env } |> add_cpu_time 6
      -- case 0x53: MP=(v=imm16())+1; env.mem16(v,D<<8|E); time+=6; break;
      0x53 -> let
                 v = imm16 z80
                 z80_1 = v.z80
                 env = set_mem16 v.value (or (shiftLeftBy8 z80.main.d) z80.main.e) z80_1.env
              in
                 { z80_1 | env = env } |> add_cpu_time 6
      -- case 0xB8: ldir(-1,true); break;
      0xB8 -> z80 |> ldir -1 True
      -- case 0xB0: ldir(1,true); break;
      0xB0 -> z80 |> ldir 1 True
      --0xB0 -> debug_log "LDIR" ("HL " ++ (z80.main.hl |> toHexString) ++ " BC " ++ (z80 |> get_bc |> toHexString2)) (z80 |> ldir 1 True)
      -- case 0x7B: MP=(v=imm16())+1; SP=env.mem16(v); time+=6; break;
      0x7B -> let
                 v = z80 |> imm16
                 z80_1 = v.z80
                 env = z80_1.env
                 sp = env |> mem16 v.value
              in
                 { z80_1 | env = sp.env, sp = sp.value } |> add_cpu_time 6
      -- case 0x4B: MP=(v=imm16())+1; v=env.mem16(v); B=v>>>8; C=v&0xFF; time+=6; break;
      0x4B -> let
                 v1 = z80 |> imm16
                 v2 = v1.z80.env |> mem16 v1.value
                 --x = debug_log "LD BC,(nnnn)" (v2.value |> toHexString) Nothing
              in
                 v1.z80 |> set_env v2.env |> set_bc v2.value |> add_cpu_time 6
      -- case 0x73: MP=(v=imm16())+1; env.mem16(v,SP); time+=6; break;
      0x73 -> let
                 v = z80 |> imm16
                 env = v.z80.env |> set_mem16 v.value v.z80.sp
              in
                 z80 |> set_env env |> add_cpu_time 6
      -- case 0x5B: MP=(v=imm16())+1; v=env.mem16(v); D=v>>>8; E=v&0xFF; time+=6; break;
      0x5B -> let
                 v1 = z80 |> imm16
                 z80_1 = v1.z80
                 v2 = z80_1.env |> mem16 v1.value
              in
                 z80_1 |> set_env v2.env |> set_de v2.value |> add_cpu_time 6
      -- case 0x42: sbc_hl(B<<8|C); break;
      0x42 -> let
                 bc = z80 |> get_bc
              in
                 z80 |> sbc_hl bc
      -- case 0x72: sbc_hl(SP); break;
      0x72 -> z80 |> sbc_hl z80.sp
      ---- case 0x6A: adc_hl(HL); break;
      --0x6A -> z80 |> adc_hl z80.main.hl
      ---- case 0x5A: adc_hl(D<<8|E); break;
      --0x5A -> z80 |> adc_hl (z80 |> get_de)
      ---- case 0x4A: adc_hl(B<<8|C); break;
      --0x4A -> z80 |> adc_hl (z80 |> get_bc)
      _ ->
         -- case 0x46:
         -- case 0x4E:
         -- case 0x56:
         -- case 0x5E:
         -- case 0x66:
         -- case 0x6E:
         -- case 0x76:
         -- case 0x7E: IM = c>>3&3; break;
         if List.member c.value [0x46, 0x4E, 0x56, 0x5E, 0x66, 0x6E, 0x76, 0x7E] then
            z80 |> set_im_direct (and (shiftRightBy 3 c.value) 3)
         else if List.member c.value [0x40, 0x48, 0x50, 0x58, 0x60, 0x68, 0x70] then
            -- case 0x40: f_szh0n0p(B=env.in(B<<8|C)); time+=4; break;
            -- case 0x48: f_szh0n0p(C=env.in(B<<8|C)); time+=4; break;
            -- case 0x50: f_szh0n0p(D=env.in(B<<8|C)); time+=4; break;
            -- case 0x58: f_szh0n0p(E=env.in(B<<8|C)); time+=4; break;
            -- case 0x60: f_szh0n0p(v=env.in(B<<8|C)); HL=HL&0xFF|v<<8; time+=4; break;
            -- case 0x68: f_szh0n0p(v=env.in(B<<8|C)); HL=HL&0xFF00|v; time+=4; break;
            -- case 0x70: f_szh0n0p(env.in(B<<8|C)); time+=4; break;
            let
              bc = z80 |> get_bc
              inval = z80.env |> z80_in bc
              z80_1 = z80 |> set408bit (shiftRightBy 3 (c.value - 0x40)) inval.value HL
            in
              z80_1 |> f_szh0n0p inval.value |> add_cpu_time 4
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
         else
            debug_todo "group_ed" (c.value |> toHexString) z80
-- case 0x4F: r(A); time++; break;
-- case 0x57: ld_a_ir(IR>>>8); break;
-- case 0x5F: ld_a_ir(r()); break;
-- case 0x67: rrd(); break;
-- case 0x6F: rld(); break;
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
      z80 = { tmp_z80 | env = c.env } |> inc_pc |> add_cpu_time 4
      o = and (shiftRightBy 3 c.value) 7
      caseval = and c.value 0xC7
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
           value = shifter o raw.value raw.z80.flags
         in
           raw.z80 |> set408bit caseval value.value HL
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
           flags = bit o raw.value raw.z80.flags
         in
           raw.z80 |> set_flag_regs flags
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
         in
             raw.z80 |> set408bit caseval result HL
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
         in
             raw.z80 |> set408bit caseval result HL
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
group_xy_cb: IXIYHL -> Z80 -> Z80
group_xy_cb ixiyhl z80 =
   let
      xy = get_xy ixiyhl z80
      offset = mem z80.pc z80.env
      a = char (xy + (byte offset.value))
      z80_1 = { z80 | env = offset.env } |> add_cpu_time 3
      c = z80_1.env |> mem (char (z80.pc + 1))
      z80_2 = { z80_1 | env = c.env } |> inc_pc2 |> add_cpu_time 5
      v1 = z80_2.env |> mem a
      z80_3 = { z80_2 | env = v1.env } |> add_cpu_time 4
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
      z80_4 = z80_3 |> set_flag_regs v2.flags |> set_env new_env |> add_cpu_time 3
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
--	}
--
--	/* interrupts */
--
--	private int IFF, IM;
--	private boolean halted;
--
--	boolean interrupt(int bus)
--	{
--		if((IFF&1)==0)
--			return false;
--		IFF = 0;
--		halted = false;
--		time += 6;
--		push(PC);
--		switch(IM) {
--		case 0:	// IM 0
--		case 1:	// IM 0
--			if((bus|0x38)==0xFF) {PC=bus-199; break;}
--			/* not emulated */
--		case 2:	// IM 1
--			PC = 0x38; break;
--		case 3:	// IM 2
--			PC = env.mem16(IR&0xFF00 | bus);
--			time += 6;
--			break;
--		}
--		MP=PC;
--		return true;
--	}
interrupt: Int -> Z80 -> Z80
interrupt bus z80 =
   let
      ints = z80.interrupts
   in
      if (and ints.iff 1) == 0 then
         z80
      else
         { z80 | interrupts = { ints | halted = False } }

set_im_direct: Int -> Z80 -> Z80
set_im_direct value z80 =
   let
      ints = debug_log "set_im" (String.fromInt value) z80.interrupts
   in
      { z80 | interrupts = { ints | iM = value } }

set_env: Z80Env -> Z80 -> Z80
set_env z80env z80 =
   { z80 | env = z80env }

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
