module Keyboard exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict
import Utils exposing (toHexString, toHexString2)
import Z80Debug exposing (debug_log)

type Kempston = JoystickLeft | JoystickRight | JoystickUp | JoystickDown | JoystickControl
--type ControlKey = Shift | Control | Enter | Alt | AltGraph | Backspace | Delete | Home | Escape |
--                  End | PageUp | PageDown | NumLock | ArrowDown | ArrowUp | ArrowLeft | ArrowRight |
--                  Insert | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12
type ControlKey = Shift | Control | Enter | Alt | Backspace | Escape |
                  ArrowLeft | ArrowDown | ArrowUp | ArrowRight
--| Delete | Home  |
--                  End | PageUp | PageDown | NumLock | ArrowDown | ArrowUp | ArrowLeft | ArrowRight |
--                  Insert | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12

-- actual supported control keys
c_CONTROL_KEY_MAP = Dict.fromList [
      ("Shift", Shift), ("Control", Control), ("Enter", Enter), ("Escape", Escape),
      ("Alt", Alt), ("Backspace", Backspace),
      ("ArrowLeft", ArrowLeft), ("ArrowDown", ArrowDown), ("ArrowUp", ArrowUp), ("ArrowRight", ArrowRight)]

type alias Keyboard =
    {
        keyboard: List Int,
        kempston: List Kempston
    }

type KeyEvent
   = KeyDownEvent Char
   | ControlKeyDownEvent ControlKey

constructor: Keyboard
constructor =
    Keyboard (List.repeat 8 0xFF) []

--	private int ear = 0x1BBA4; // EAR noise
--
--	public int in(int port)
--	{
--		cont_port(port);
--
--		if((port&0x00E0)==0)
--			return kempston;
--		if((port&0xC002)==0xC000 && ay_enabled) {
--			if(ay_idx>=14 && (ay_reg[7]>>ay_idx-8 & 1) == 0)
--				return 0xFF;
--			return ay_reg[ay_idx];
--		}
--		int v = 0xFF;
--		if((port&0x0001)==0) {
--			for(int i=0; i<8; i++)
--				if((port&0x100<<i) == 0)
--					v &= keyboard[i];
--			int e = 0;
--			// Apply tape noise only when SPK==0 and MIC==1.
--			// thanks Jose Luis for bug report
--			if((ula28&0x18)==8) {
--				e = ear - 0x100000;
--				if((e&0xFFF00000)==0)
--					e = e<<2 | e>>18;
--				ear = e;
--			}
--			v &= ula28<<2 | e | 0xBF;
--		} else if(cpu.time>=0) {
--			int t = cpu.time;
--			int y = t/224;
--			t %= 224;
--			if(y<192 && t<124 && (t&4)==0) {
--				int x = t>>1 & 1 | t>>2;
--				if((t&1)==0)
--					x += y & 0x1800 | y<<2 & 0xE0 | y<<8 & 0x700;
--				else
--					x += 6144 | y<<2 & 0x3E0;
--				v = ram[x];
--			}
--		}
--		return v;
--	}

--Kempston JoyLeft/Right/Up/Down uses ^ (xor) so (0123)->(1032)->(0x02,0x01,0x08,0x04) (1<<(i^1))
kempstonMapping: Kempston -> Int
kempstonMapping kempston =
   case kempston of
      JoystickLeft -> 0x02
      JoystickRight -> 0x01
      JoystickUp -> 0x08
      JoystickDown -> 0x04
      JoystickControl -> 0x10

z80_keyboard_input: Int -> Keyboard -> Int
z80_keyboard_input portnum keyboard =
   if Bitwise.and portnum 0x00E0 == 0 then
     let
        val = keyboard.kempston |> List.foldl (\kemp total -> Bitwise.or (kempstonMapping kemp) total) 0
     in
        debug_log "kempston" val val
   else
      if Bitwise.and portnum 0x0001 == 0 then
         let
            list1 = keyboard.keyboard |> List.indexedMap Tuple.pair
            debug_flag = (keyboard.keyboard |> List.sum) /= 255 * 8
            x = if debug_flag then
                  debug_log ("keyboard poll " ++ (portnum |> toHexString)) keyboard.keyboard Nothing
                else
                  Nothing
            list2 = list1 |> List.filter (\(index, value_) -> Bitwise.and portnum (0x100 |> shiftLeftBy index) == 0)
            list3 = list2 |> List.map (\(bitmask, listval) -> listval)
            y = if debug_flag then
                  debug_log "list3" list3 Nothing
                else
                  Nothing
            v = List.foldl (\num total ->  Bitwise.and num total) 0xFF list3
         in
            if v /= 0xFF then
               debug_log "keyboard v" (toHexString2 v) v
            else
               v
      else
         0xFF

--	/* keyboard & joystick */
--
--	public final int keyboard[] = new int[8];
--	public int kempston = 0;
--	public final KeyEvent keys[] = new KeyEvent[8];
--	static final int arrowsDefault[] = {0143, 0124, 0134, 0144};
c_ARROWS = [0x63, 0x54, 0x5C, 0x64] |> Array.fromList
--	int arrows[] = arrowsDefault;
--
--	void update_keyboard() {
--		for(int i=0; i<8; i++) keyboard[i] = 0xFF;
--		kempston = 0;
--
--		int m[] = new int[] {-1,-1,-1,-1,-1};
--		int s = 0;
--		synchronized(keys) {
--			for(int i=0; i<keys.length; i++) if(keys[i]!=null) {
--				int k = key(keys[i]);
--				if(k<0) continue;
--				// .......xxx row
--				// ....xxx... column
--				// ...x...... caps shift
--				// ..x....... symbol shift
--				// .x........ caps shift alone
--				// x......... symbol shift alone
--				s |= k;
--				if(k<01000)
--					pressed(k,m);
--			}
--		}
--		if((s&0300)==0) s |= s>>>3 & 0300;
--		if((s&0100)!=0) pressed(000,m);
--		if((s&0200)!=0) pressed(017,m);
--	}
--
update_keyboard: List KeyEvent -> Keyboard
update_keyboard keys =
   let
      initial_keyboard = Keyboard (List.repeat 8 0xFF) []
      m_initial = List.repeat 5 -1
      k_list = keys |> List.map key |> List.filter (\(v, kemp) -> v >= 0)
      s1 = k_list |> List.map Tuple.first |> List.foldl (\k total -> Bitwise.or k total) 0
      (keyboard, mlist) = k_list |> List.filter (\(v, kemp) -> v < 0x200)
                                 |> List.foldl (\(k, kemp) (keyb, m_list) -> pressed k { keyb | kempston = keyb.kempston ++ kemp } m_list) (initial_keyboard, m_initial)
      s2 = if Bitwise.and s1 0xC0 == 0 then
               Bitwise.or s1 (Bitwise.and (s1 |> shiftRightBy 3) 0xC0)
           else
               s1
      (keyb2, mlist2) = if Bitwise.and s2 0x40 /= 0 then
                           pressed 0 keyboard mlist
                         else
                           (keyboard, mlist)
      (keyb3, _) = if Bitwise.and s2 0x80 /= 0 then
                           pressed 0x17 keyb2 mlist2
                        else
                           (keyb2, mlist2)
   in
      keyb3

--	private final void pressed(int k, int m[])
--	{
--		int a = k&7, b = k>>>3 & 7;
--		int v = keyboard[a] & ~(1<<b);
--		int n = m[b];
--		keyboard[a] = v; m[b] = a;
--		if(n>=0) v |= keyboard[n];
--		for(n=0; n<8; n++)
--			if((keyboard[n]|v) != 0xFF)
--				keyboard[n] = v;
--	}

pressed: Int -> Keyboard ->  List Int -> (Keyboard, List Int)
pressed k keyboard mlist =
   let
      a = Bitwise.and k 7
      b = Bitwise.and (k |> shiftRightBy 3) 7
      v1 = case Array.get a (keyboard.keyboard |> Array.fromList) of
         Just value -> Bitwise.and value (1 |> shiftLeftBy b |> Bitwise.complement)
         Nothing -> debug_log "pressed" ("v is impossible " ++ String.fromInt a) 0
      n = case Array.get b (mlist |> Array.fromList) of
         Just value -> value
         Nothing -> debug_log "pressed" ("n is wong mlist size " ++ String.fromInt (mlist |> List.length) ++ " b = " ++ String.fromInt b) 0
      keyboard1 = (keyboard.keyboard |> List.take a) ++ List.singleton(v1) ++ (keyboard.keyboard |> List.reverse |> (List.take (7 - a)) |> List.reverse)
      new_mlist = (mlist |> List.take b) ++ List.singleton(a) ++ (mlist |> List.reverse |> (List.take (4 - b)) |> List.reverse)
      new_v = if n >= 0 then
                let
                   keyboard_n = case Array.get n (keyboard.keyboard |> Array.fromList) of
                                   Just value -> value
                                   Nothing -> debug_log "pressed" ("keyboard_n impossible" ++ String.fromInt n) 0
                in
                   Bitwise.or v1 keyboard_n
              else
                  v1
      keyboard2 = keyboard1 |> List.map (\kb -> if Bitwise.or kb new_v /= 0xFF then new_v else kb)
   in
      ({ keyboard | keyboard = keyboard2}, new_mlist)
--
--	private int key(KeyEvent e)
--	{
--		int c = e.getKeyCode();
--		int a = e.getKeyChar();
--		int i = "[AQ10P\n ZSW29OL]XDE38IKMCFR47UJNVGT56YHB".indexOf((char)c);
--		if(i>=0) simple: {
--			int s = 0;
--			if(c>=KeyEvent.VK_0 && c<=KeyEvent.VK_9) {
--				if(c!=(int)a) break simple;
--				if(e.isAltDown()) s = 0100;
--			}
--			return i | s;
--		}
--		if(a != '\0') {
--			i = "\t\0\0!_\"\0\0:\0\0@);=\0\0\0\0#(\0+.?\0<$'\0-,/\0>%&\0^*".indexOf(a);
--			if(i>=0)
--				return i | 0200;
--		}
--		switch(c) {
--			case KeyEvent.VK_INSERT:
--			case KeyEvent.VK_ESCAPE: return 0103;
--			case KeyEvent.VK_KP_LEFT:
--			case KeyEvent.VK_LEFT: i=0; break;
--			case KeyEvent.VK_KP_DOWN:
--			case KeyEvent.VK_DOWN: i=3; break;
--			case KeyEvent.VK_KP_UP:
--			case KeyEvent.VK_UP: i=2; break;
--			case KeyEvent.VK_KP_RIGHT:
--			case KeyEvent.VK_RIGHT: i=1; break;
--			case KeyEvent.VK_BACK_SPACE: return 0104;
--			case KeyEvent.VK_SHIFT: return 01000;
--			case KeyEvent.VK_CONTROL: kempston |= 0x10; /* fall */
--			case KeyEvent.VK_ALT: return 02000;
--			default: return -1;
--		}
--		kempston |= 1<<(i^1);
--		return e.isAltDown() ? arrowsDefault[i] : arrows[i];
--	}
--c_SPECCY_KEYBOARD_CHARS = Dict.fromList [('[', 0), ('A', 1), etc
c_SPECCY_KEYBOARD_CHARS = "[AQ10P\n ZSW29OL]XDE38IKMCFR47UJNVGT56YHB" |> String.toList |> List.indexedMap (\index char -> (char, index)) |> Dict.fromList

key: KeyEvent -> (Int, List Kempston)
key event  =
   case event of
      KeyDownEvent char ->
         (Maybe.withDefault -1 (Dict.get char c_SPECCY_KEYBOARD_CHARS), [])
      ControlKeyDownEvent controlKey ->
         case controlKey of
            Escape -> (0x43, [])
            ArrowLeft -> (c_ARROWS |> Array.get 0 |> Maybe.withDefault(-1), [JoystickLeft])
            ArrowDown -> (c_ARROWS |> Array.get 3 |> Maybe.withDefault(-1), [JoystickDown])
            ArrowUp ->  (c_ARROWS |> Array.get 2 |> Maybe.withDefault(-1), [JoystickUp])
            ArrowRight -> (c_ARROWS |> Array.get 1 |> Maybe.withDefault(-1), [JoystickRight])
            Backspace -> (0x44, [])
            Shift -> (0x200, [])
            Control -> (0x0400, [JoystickControl])
            Alt -> (0x400, [])
            -- index 6 in c_SPECCY_KEYBOARD_CHARS
            Enter -> (6, [])

--	public void setArrows(String s) {
--		arrows = new int[4];
--		for(int i=0; i<4; i++) {
--			int c = -1;
--			if(i<s.length())
--				c = "Caq10pE_zsw29olSxde38ikmcfr47ujnvgt56yhb"
--					.indexOf(s.charAt(i));
--			if(c<0) c = arrowsDefault[i];
--			arrows[i] = c;
--		}
--	}
