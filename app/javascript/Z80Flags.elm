module Z80Flags exposing (..)

import Bitwise exposing (complement, shiftLeftBy, shiftRightBy)
import Utils exposing (shiftLeftBy1, shiftLeftBy8, shiftRightBy1, shiftRightBy8)
type alias FlagRegisters =
   {
      a:   Int,
      ff:  Int,
      fr:  Int,
      fa:  Int,
      fb:  Int
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
get_flags: FlagRegisters -> Int
get_flags flags =
   let
       f1 = flags.ff
       a1 = flags.fa
       b1 = flags.fb
       r = flags.fr
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

z80_add: Int -> FlagRegisters -> FlagRegisters
z80_add b flags =
   let
      fa = flags.a
      fb = b
      ff = fa + fb
      fr = Bitwise.and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }

adc: Int -> FlagRegisters -> FlagRegisters
adc b flags =
   let
      fa = flags.a
      fb = b
      ff = fa + fb + (Bitwise.and (shiftRightBy8 flags.ff) c_FC)
      fr = Bitwise.and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }

z80_sub: Int -> FlagRegisters -> FlagRegisters
z80_sub b flags =
   let
      fb = complement b
      fa = flags.a
      ff = fa - b
      fr = Bitwise.and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }

sbc: Int -> FlagRegisters -> FlagRegisters
sbc b flags =
   let
      fb = complement b
      fa = flags.a
      ff = fa - b - (Bitwise.and (shiftRightBy8 flags.ff) c_FC)
      fr = Bitwise.and ff 0xFF
   in
      { flags | fa = fa, fb = fb, ff = ff, fr = fr, a = fr }

cp: Int -> FlagRegisters -> FlagRegisters
cp b flags =
   let
      fa = flags.a
      r = fa - b
      fb = complement b
      ff = Bitwise.or (Bitwise.and r (complement c_F53)) (Bitwise.and b c_F53)
      fr = Bitwise.and r 0xFF
   in
     { flags | fr = fr, ff = ff, fb = fb, fa = fa }

z80_and: Int -> FlagRegisters -> FlagRegisters
z80_and b flags =
   let
      fr = Bitwise.and flags.a b
      ff = fr
      a = ff
      fa = complement a
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }

z80_or: Int -> FlagRegisters -> FlagRegisters
z80_or b flags =
   let
      fr = Bitwise.or flags.a b
      ff = fr
      a = ff
      fa = Bitwise.or a 0x100
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }

z80_xor: Int -> FlagRegisters -> FlagRegisters
z80_xor b flags =
   let
      fr = Bitwise.xor flags.a b
      ff = fr
      a = ff
      fa = Bitwise.or a 0x100
   in
      { flags | fa = fa, fb = 0, ff = ff, fr = fr, a = a }

cpl: FlagRegisters -> FlagRegisters
cpl flags =
   let
      new_a = Bitwise.xor flags.a 0xFF
      ff = Bitwise.or (Bitwise.and flags.ff (complement c_F53)) (Bitwise.and new_a c_F53)
      fb = Bitwise.or flags.fb (complement 0x80)
      fa = Bitwise.or (Bitwise.and flags.fa (complement c_FH)) (Bitwise.and (complement flags.fr) c_FH)
   in
     { flags | a = new_a, ff= ff, fb = fb, fa = fa }

inc: Int -> FlagRegisters -> IntWithFlags
inc v flags =
   let
      ff = Bitwise.and flags.ff 0x100
      vv = Bitwise.and (v + 1) 0xFF
      new_flags = { flags | ff = (Bitwise.or ff vv), fb = 1, fa = v, fr = vv }
   in
      IntWithFlags vv new_flags

dec: Int -> FlagRegisters -> IntWithFlags
dec v flags =
   let
      ff = Bitwise.and flags.ff 0x100
      vv = Bitwise.and (v - 1) 0xFF
   in
      IntWithFlags vv { flags | ff = (Bitwise.or ff vv), fb = -1, fa = v, fr = vv }

bit: Int -> Int -> FlagRegisters -> FlagRegisters
bit n v flags =
   let
     m = Bitwise.and v (shiftLeftBy n 1)
     ff = Bitwise.or (Bitwise.and flags.ff (complement 0xFF)) (Bitwise.or (Bitwise.and v c_F53) m)
     fr = m
   in
     { flags | ff = ff, fr = fr, fa = complement fr, fb = 0 }

rot: Int -> FlagRegisters -> FlagRegisters
rot a flags =
   let
      ff = Bitwise.or (Bitwise.and flags.ff 0x07) (Bitwise.and a 0x128)
      fb = Bitwise.and flags.fb 0x80
      fa = Bitwise.or (Bitwise.and flags.fa (Bitwise.complement c_FH)) (Bitwise.and flags.fr c_FH)
   in
      { flags | ff = ff, fb = fb, fa = fa, a = (Bitwise.and a 0xFF) }

shifter: Int -> Int -> FlagRegisters -> IntWithFlags
shifter o v_in flags =
   let
      v = case (Bitwise.and o 7) of
               0 -> shiftRightBy 7 (v_in * 0x101)
               1 -> shiftRightBy 24 (v_in * 0x80800000)
               2 -> Bitwise.or (shiftLeftBy1 v_in) (Bitwise.and (shiftRightBy8 flags.ff) 1)
               3 -> shiftRightBy1 (Bitwise.or (v_in * 0x201) (Bitwise.and flags.ff 0x100))
               4 -> shiftLeftBy1 v_in
               5 -> Bitwise.or (Bitwise.or (shiftRightBy1 v_in) (Bitwise.and v_in 0x80)) (shiftLeftBy8 v_in)
               6 -> Bitwise.or (shiftLeftBy1 v_in) 1
               _ -> shiftRightBy1 (v_in * 0x201)
      fr = Bitwise.and 0xFF v
   in
      IntWithFlags fr { flags | ff = v, fr = fr, fb = 0, fa = (Bitwise.or 0x100 fr) }

shifter_v: Int -> FlagRegisters -> IntWithFlags
shifter_v v flags =
   let
      fr = Bitwise.and 0xFF v
   in
      IntWithFlags fr { flags | ff = v, fr = fr, fb = 0, fa = (Bitwise.or 0x100 fr) }

shifter0: Int -> FlagRegisters -> IntWithFlags
shifter0 v_in flags =
    flags |> shifter_v (shiftRightBy 7 (v_in * 0x101))

shifter1: Int -> FlagRegisters -> IntWithFlags
shifter1 v_in flags =
    flags |> shifter_v (shiftRightBy 24 (v_in * 0x80800000))

shifter2: Int -> FlagRegisters -> IntWithFlags
shifter2 v_in flags =
    flags |> shifter_v (Bitwise.or (shiftLeftBy1 v_in) (Bitwise.and (shiftRightBy8 flags.ff) 1))

shifter3: Int -> FlagRegisters -> IntWithFlags
shifter3 v_in flags =
    flags |> shifter_v (shiftRightBy1 (Bitwise.or (v_in * 0x201) (Bitwise.and flags.ff 0x100)))

shifter4: Int -> FlagRegisters -> IntWithFlags
shifter4 v_in flags =
    flags |> shifter_v (shiftLeftBy1 v_in)

shifter5: Int -> FlagRegisters -> IntWithFlags
shifter5 v_in flags =
    flags |> shifter_v (Bitwise.or (Bitwise.or (shiftRightBy1 v_in) (Bitwise.and v_in 0x80)) (shiftLeftBy8 v_in))

shifter6: Int -> FlagRegisters -> IntWithFlags
shifter6 v_in flags =
    flags |> shifter_v (Bitwise.or (shiftLeftBy1 v_in) 1)

shifter7: Int -> FlagRegisters -> IntWithFlags
shifter7 v_in flags =
    flags |> shifter_v (shiftRightBy1 (v_in * 0x201))

add16: Int -> Int -> FlagRegisters -> IntWithFlagsAndTime
add16 a b main_flags =
    let
        r = a + b
        ff = Bitwise.or (Bitwise.and main_flags.ff c_FS) (Bitwise.and (shiftRightBy8 r) 0x128)
        fa = Bitwise.and main_flags.fa (complement c_FH)
        shiftright = shiftRightBy8 (Bitwise.xor (Bitwise.xor r a) b)
        shiftrightxor = Bitwise.xor shiftright main_flags.fr
        fb_rhs = Bitwise.and shiftrightxor c_FH
        fb = Bitwise.or (Bitwise.and main_flags.fb 0x80) fb_rhs
        new_flags = { main_flags | ff = ff, fa = fa, fb = fb }
    in
        IntWithFlagsAndTime (Bitwise.and r 0xFFFF) new_flags 7

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
      fa = Bitwise.and flags.fa (complement c_FH)
      fb = Bitwise.or (Bitwise.and flags.fb 0x80) (Bitwise.and (Bitwise.xor (shiftRightBy 4 x) flags.fr) c_FH)
      ff = Bitwise.or (Bitwise.or (Bitwise.xor 0x100 x) (Bitwise.and flags.ff c_FS)) (Bitwise.and flags.a c_F53)
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
      fa = Bitwise.or flags.a 0x100
      (a0, fb) = if Bitwise.and flags.fb 0x200 == 0 then
                  (flags.a + d, d)
                else
                  (flags.a - d, complement d)
      a = Bitwise.and a0 0xFF
      fr = a
      ff = Bitwise.or fr (Bitwise.and d 0x100)
   in
      { flags | fr = fr, a = a, fb = fb, fa = fa, ff = ff }
