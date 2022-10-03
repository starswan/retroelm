module Keyboard exposing (..)

import Bitwise exposing (shiftLeftBy)
import Utils exposing (debug_log, toHexString2)

type alias Keyboard =
    {
        keyboard: List Int,
        kempston: Int
        --keys: List Int
    }

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

z80_keyboard_input: Int -> Keyboard -> Int
z80_keyboard_input portnum keyboard =
   if Bitwise.and portnum 0x00E0 == 0 then
     let
        val = keyboard.kempston
     in
        debug_log "kempston" val val
   else
      if Bitwise.and portnum 0x0001 == 0 then
         let
            list1 = keyboard.keyboard |> List.indexedMap Tuple.pair
            list2 = list1 |> List.filter (\(index, value_) -> Bitwise.and portnum (0x100 |> shiftLeftBy index) == 0)
            list3 = list2 |> List.map (\(bitmask, listval) -> listval)
            v = List.foldl (\num total ->  Bitwise.or num total) 0xFF list3
         in
            debug_log "keyboard" (toHexString2 v) v
      else
         0xFF

--	/* keyboard & joystick */
--
--	public final int keyboard[] = new int[8];
--	public int kempston = 0;
--	public final KeyEvent keys[] = new KeyEvent[8];
--	static final int arrowsDefault[] = {0143, 0124, 0134, 0144};
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
update_keyboard: Keyboard -> Keyboard
update_keyboard k =
   let
      keyboard = List.repeat 0xFF 8
      kempston = 0
      m = List.repeat -1 5
      s = 0
      --new_keys = List.map key k.keys
   in
      { k | keyboard = keyboard, kempston = kempston }
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
key: Int -> Int
key event =
   event
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
