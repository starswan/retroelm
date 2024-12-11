module Spectrum exposing (..)

import Array exposing (Array)
import Bitwise exposing (complement, shiftRightBy)
import Dict
import Keyboard exposing (KeyEvent, Keyboard, update_keyboard)
import SingleNoParams exposing (ex_af)
import Tapfile exposing (Tapfile)
import Utils exposing (char, shiftRightBy8)
import Vector8
import Z80 exposing (execute, get_ei, interrupt)
import Z80Address exposing (incrementBy2, toInt)
import Z80Debug exposing (debugLog)
import Z80Env exposing (mem, mem16, reset_cpu_time)
import Z80Flags exposing (c_FC, c_FZ, get_flags, set_flags)
import Z80Rom exposing (Z80ROM, make_spectrum_rom)
import Z80Tape exposing (Z80Tape)
import Z80Types exposing (IXIYHL(..), Z80, get_de, get_l)


type alias Audio =
    {}


type alias Tape =
    { stop_loading : Bool
    , -- tried to convert this to a List Char, but blew up the stack whilst doing so
      tape : List Int
    , tape_blk : Int
    , tape_pos : Int
    , tape_changed : Bool
    , tape_ready : Bool
    }



--	private static final int REFRESH_END = 99999;
--	final int scrchg[] = new int[24];	// where the picture changed
--	private int refrs_t, refrs_a, refrs_b, refrs_s;


type alias ScreenRefresh =
    { scrchg : List Int
    , refrs_t : Int
    , refrs_a : Int
    , refrs_b : Int
    , refrs_s : Int
    }


type alias BorderRefresh =
    { bordchg : Int
    , refrb_t : Int
    , refrb_x : Int
    , refrb_y : Int
    }


new_screen_refresh : ScreenRefresh
new_screen_refresh =
    let
        scrchg =
            List.repeat 24 0
    in
    ScreenRefresh scrchg 0 0 0 0


new_border_refresh : BorderRefresh
new_border_refresh =
    BorderRefresh 0 0 0 0


new_tape : List Tapfile -> Spectrum -> Spectrum
new_tape tapfileList spectrum =
    let
        --x = debug_log "new_tape list" (tapfile_list |> List.length) Nothing
        tapedict =
            tapfileList |> List.indexedMap (\index tap -> ( index, tap )) |> Dict.fromList |> Z80Tape Z80Tape.newPosition
    in
    --  w/o this change, the code crashes with a recursion error
    --{ spectrum | tape = Just (Z80Tape False tape_string 0 0 True True) }
    -- spectrum
    -- This is weird - it only works if the tape is a dict (rather than list or Array)
    -- otherwise we just an infinite recursion error
    { spectrum | tape = Just tapedict }



-- let
--    z80 = spectrum.cpu
-- in
--    { spectrum | cpu = { z80 | env = z80.env |> Z80Env.set_tape tape_string } }


set_rom : Array Int -> Spectrum -> Spectrum
set_rom romdata spectrum =
    let
        z80 =
            spectrum.cpu

        rommy =
            make_spectrum_rom romdata
    in
    --{ spectrum | cpu = { z80 | env = z80.env |> Z80Env.set_rom romdata } }
    { spectrum | rom48k = rommy }


loadTapfile : Tapfile -> Spectrum -> Spectrum
loadTapfile tapFile spectrum =
    let
        cpu =
            spectrum.cpu

        env =
            cpu.env

        --env  = case tapFile.header.start.headerType of
        --    PROGRAM ->
        --        let
        --            y = tapFile.block.data
        --        in
        --        cpu.env
        --    NUMBER_ARRAY -> cpu.env
        --    Tapfile.CHAR_ARRAY -> cpu.env
        --    Tapfile.CODE -> cpu.env
    in
    { spectrum | cpu = { cpu | env = env } }



--c_Mh = 6 -- margin
--c_Mv = 5
--c_W = 256 + 8*c_Mh*2 -- 352
--c_H = 192 + 8*c_Mv*2 -- 272
--c_REFRESH_END = 99999
--c_BORDER_START = -224*8*c_Mv - 4*c_Mh + 4
--c_CHANNEL_VOLUME = 26000
--c_SPEAKER_VOLUME = 49000
-- Spectrum only contained a reference to Qaop
-- so that it could call new_pixels() on it in 2 places


type alias Spectrum =
    { cpu : Z80
    , rom48k : Z80ROM
    , paused : Bool
    , loading : Bool
    , want_pause : Int
    , tape : Maybe Z80Tape
    , --audio: Audio,
      screen_refresh : ScreenRefresh
    , border_refresh : BorderRefresh
    }


constructor : Spectrum
constructor =
    --Spectrum Z80.constructor True 1 Nothing Audio new_screen_refresh new_border_refresh
    Spectrum Z80.constructor Z80Rom.constructor True False 1 Nothing new_screen_refresh new_border_refresh



--
--	public void run()
--	{
--		try {
--			frames();
--		} catch(InterruptedException e) {}
--		audio.close();
--	}
--
--	private void end_frame() {
--		refresh_screen();
--		if(border != border_solid) {
--			int t = refrb_t;
--			refresh_border();
--			if(t == BORDER_START)
--				border_solid = border;
--		}
--
--		for(int i=0; i<consumers.size();) {
--			ImageConsumer c = (ImageConsumer)
--				consumers.elementAt(consumers.size() - ++i);
--			update_screen(c);
--		}
--		send_change();
--
--		cpu.time -= FRTIME;
--		if(--flash_count <= 0) {
--			flash ^= 0xFF;
--			flash_count = 16;
--		}
--		audio.level -= audio.level>>8;
--	}
--
--	private long time;
--	private int timet;
--
--	private void frames() throws InterruptedException
--	{
--		time = System.currentTimeMillis();
--		cpu.time = FRSTART;
--		cpu.time_limit = FRSTART+FRTIME;
--		au_time = cpu.time;
--		do {
--			byte[] tap = null;
--			boolean tend = false;
--			synchronized(this) {
--				int w = want_scale;
--				if(w != scale) {
--					if(w<0) break; // quit
--					scale = w;
--					width=w*W; height=w*H;
--					cm = w==0 ? cm1 : cm2;
--					notifyAll();
--					abort_consumers();
--				}
--				w = want_pause;
--				if((w!=0) ^ paused) {
--					paused = w!=0;
--					notifyAll();
--				}
--				if(stop_loading) {
--					loading = stop_loading = false;
--					notifyAll();
--				}
--				if(!paused) {
--					tap = tape;
--					tend = tape_ready;
--					if(!loading && tap!=null)
--						loading = check_load();
--				}
--			}
--
--			update_keyboard();
--			refresh_new();
--
--			if(paused) {
--				int t = cpu.time = cpu.time_limit;
--				audio.step(t-au_time, 0);
--				au_time = t;
--			} else {
--				if(loading) {
--					loading = do_load(tap, tend);
--					cpu.time = cpu.time_limit;
--				} else {
--					cpu.interrupt(0xFF);
--					cpu.execute();
--				}
--				au_update();
--			}
--			au_time -= FRTIME;
--			end_frame();
--
--			/* sync */
--
--			timet += 121;
--			if(timet >= 125) {timet -= 125; time++;}
--			time += 19;
--
--			long t = System.currentTimeMillis();
--			if(t < time) {
--				t = time-t;
--				sleep(t);
--			} else {
--				yield();
--				t -= 100;
--				if(t > time)
--					time = t;
--			}
--		} while(!interrupted());
--	}


frames : List KeyEvent -> Spectrum -> Spectrum
frames keys speccy =
    let
        --tap = 0
        --tend = False
        sz80 =
            speccy.cpu

        env =
            sz80.env |> reset_cpu_time

        cpu1 =
            { sz80
              -- time_limit is a constant
              --| time_limit = c_FRSTART + c_FRTIME
                | env = { env | keyboard = keys |> update_keyboard }
            }

        loading_z80 =
            if speccy.loading then
                speccy |> checkLoad

            else
                Nothing

        ( new_loading, cpu ) =
            case loading_z80 of
                Just z80 ->
                    let
                        --cpu2 = cpu1 |> doLoad
                        a =
                            1
                    in
                    ( True, cpu1 )

                Nothing ->
                    ( False
                    , cpu1
                        |> interrupt 0xFF speccy.rom48k
                        |> execute speccy.rom48k
                    )
    in
    { speccy | loading = new_loading, cpu = cpu }



--x = if spectrum.paused then
--       1
--   else
--       1
--(scr, bord) = refresh_new spectrum.screen_refresh spectrum.border_refresh
--z80 = if spectrum.paused then
--           spectrum.cpu
--         else
--           case spectrum.tape of
--               Just a ->
--                   let
--                       loaded = do_load tap tend spectrum.cpu
--                   in
--                       loaded
--               Nothing ->
--                   execute spectrum.cpu
--refresh_new: ScreenRefresh -> BorderRefresh -> (ScreenRefresh, BorderRefresh)
--refresh_new screen_refresh border_refresh =
--    let
--        new_border = { border_refresh | refrb_x = -c_Mh, refrb_y = -8*c_Mv, refrb_t = c_BORDER_START }
--    in
--        ({ screen_refresh | refrs_t = 0, refrs_b = 0, refrs_s = c_Mv*c_W + c_Mh, refrs_a = 0x1800}, new_border)
--
--	boolean paused = true;
--	int want_pause = 1;
--
--	public synchronized int pause(int m) throws InterruptedException
--	{
--		if((want_pause = want_pause&~m>>3 ^ m&7)!=0 && !paused) do
--			wait();
--		while(!paused);
--		return want_pause;
--	}
--w = want_pause;
--if((w!=0) ^ paused) {
--   paused = w!=0;
--   notifyAll();
--}


pause : Int -> Spectrum -> Spectrum
pause m spectrum =
    let
        w =
            Bitwise.xor (shiftRightBy 3 (Bitwise.and spectrum.want_pause (complement m))) (Bitwise.and m 7)

        paused =
            if xor spectrum.paused (w /= 0) then
                debugLog "pause" (w /= 0) w /= 0

            else
                debugLog "pause" Nothing spectrum.paused
    in
    { spectrum | want_pause = w, paused = paused }



--
--	public synchronized void reset()
--	{
--		stop_loading();
--		cpu.reset();
--		au_reset();
--		rom = rom48k;
--	}
--
--	protected byte ay_idx;
--	private byte ula28;
--
--	public void out(int port, int v)
--	{
--		cont_port(port);
--
--		if((port&0x0001)==0) {
--			ula28 = (byte)v;
--			int n = v&7;
--			if(n != border) {
--				refresh_border();
--				border = (byte)n;
--			}
--			n = sp_volt[v>>3 & 3];
--			if(n != speaker) {
--				au_update();
--				speaker = n;
--			}
--		}
--		if((port&0x8002)==0x8000 && ay_enabled) {
--			if((port&0x4000)!=0)
--				ay_idx = (byte)(v&15);
--			else {
--				au_update();
--				ay_write(ay_idx, v);
--			}
--		}
--	}
--
--
--	public int halt(int n, int ir)
--	{
--		return n;
--	}
--
--	/* screen */
--
--	private static final int SCRENDT = 191*224+126;
--	private static final int Mh = 6; // margin
--	private static final int Mv = 5;
--	static final int W = 256 + 8*Mh*2; // 352
--	static final int H = 192 + 8*Mv*2; // 272
--	int width=W, height=H, scale=0, want_scale=0;
--
--	public synchronized void scale(int m)
--	{
--		want_scale = m;
--		if(m>=0) try {
--			while(scale != m) wait();
--		} catch(InterruptedException e) {
--			currentThread().interrupt();
--		}
--	}
--
--	public int scale() {return scale;}
--
--	private void force_refresh()
--	{
--		bordchg = (1L<<Mv+24+Mv) - 1;
--		for(int r=0; r<24; r++)
--			scrchg[r] = ~0;
--	}
--
--	final int screen[] = new int[W/8*H];	// canonicalized scr. content
--	int flash_count = 16;
--	int flash = 0x8000;
--
--	/* screen refresh */
--
--	private static final int REFRESH_END = 99999;
--	final int scrchg[] = new int[24];	// where the picture changed
--	private int refrs_t, refrs_a, refrs_b, refrs_s;
--
--	private final void refresh_new() {
--		refrs_t = refrs_b = 0;
--		refrs_s = Mv*W + Mh;
--		refrs_a = 0x1800;
--
--		refrb_x = -Mh;
--		refrb_y = -8*Mv;
--		refrb_t = BORDER_START;
--	}
--
--	private final void refresh_screen() {
--		int ft = cpu.time;
--		if(ft < refrs_t)
--			return;
--		final int flash = this.flash;
--		int a = refrs_a, b = refrs_b;
--		int t = refrs_t, s = refrs_s;
--		do {
--			int sch = 0;
--
--			int v = ram[a]<<8 | ram[b++];
--			if(v>=0x8000) v ^= flash;
--			v = canonic[v];
--			if(v!=screen[s]) {
--				screen[s] = v;
--				sch = 1;
--			}
--
--			v = ram[a+1]<<8 | ram[b++];
--			if(v>=0x8000) v ^= flash;
--			v = canonic[v];
--			if(v!=screen[++s]) {
--				screen[s] = v;
--				sch += 2;
--			}
--
--			if(sch!=0)
--				scrchg[a-0x1800>>5] |= sch<<(a&31);
--
--			a+=2; t+=8; s++;
--			if((a&31)!=0) continue;
--			// next line
--			t+=96; s+=2*Mh;
--			a-=32; b+=256-32;
--			if((b&0x700)!=0) continue;
--			// next row
--			a+=32; b+=32-0x800;
--			if((b&0xE0)!=0) continue;
--			// next segment
--			b+=0x800-256;
--			if(b>=6144) {
--				t = REFRESH_END;
--				break;
--			}
--		} while(ft >= t);
--		refrs_a = a; refrs_b = b;
--		refrs_t = t; refrs_s = s;
--	}
--
--	/* border refresh */
--
--	private static final int BORDER_START = -224*8*Mv - 4*Mh + 4;
--	private long bordchg;
--	private int refrb_t, refrb_x, refrb_y;
--
--	private final void refresh_border()
--	{
--		int ft = cpu.time;
--		int t = refrb_t;
--//		if(t == BORDER_END) XXX only if within screen
--		if(ft < t)
--			return;
--		border_solid = -1;
--
--		int c = canonic[border<<11];
--		int x = refrb_x;
--		int y = refrb_y;
--		int p = Mh+(Mh+32+Mh)*8*Mv + x + (Mh+32+Mh)*y;
--		long m = 1L << (y>>>3)+Mv;
--		boolean chg = false;
--
--		do {
--			if(screen[p]!=c) {
--				screen[p]=c;
--				chg = true;
--			}
--			p++; t+=4;
--			if(++x==0 && (char)y<192) {
--				p+=(x=32); t+=128;
--				continue;
--			}
--			if(x<32+Mh)
--				continue;
--			x=-Mh; t+=224-4*(Mh+32+Mh);
--			if((++y&7)!=0)
--				continue;
--			if(y==8*(24+Mv)) {
--				t = REFRESH_END;
--				break;
--			}
--			if(chg) {
--				bordchg |= m;
--				chg = false;
--			}
--			m<<=1;
--		} while(t <= ft);
--
--		if(chg)
--			bordchg |= m;
--
--		refrb_x = x;
--		refrb_y = y;
--		refrb_t = t;
--	}
--
--	/* image */
--
--	private final void update_box(ImageConsumer ic,
--			int y, int x, int w, byte buf[])
--	{
--		int si = y*W + x;
--		int p = 0;
--
--		x <<= 3; y <<= 3;
--		int h, s;
--
--		if(scale==1) {
--			s = w*8;
--			for(int n=0; n<8; n++) {
--				for(int k=0; k<w; k++) {
--					int m = screen[si++];
--					byte c0 = (byte)(m>>>8 & 0xF);
--					byte c1 = (byte)(m>>>12);
--					m &= 0xFF;
--					do
--						buf[p++] = (m&1)==0 ? c0 : c1;
--					while((m >>>= 1)!=0);
--				}
--				si += (W/8)-w;
--			}
--			h = 8;
--		} else {
--			h = scale<<3;
--			s = w*h;
--			for(int n=0; n<8; n++) {
--				for(int k=0; k<w; k++) {
--					int m = screen[si++];
--					byte c0 = (byte)(m>>>8 & 0xF);
--					byte c1 = (byte)(m>>>12);
--					m &= 0xFF;
--					do {
--						buf[p+s] = buf[p+s+1] = (byte)(
--						(buf[p] = buf[p+1]
--						 = (m&1)==0 ? c0 : c1) + 16);
--						p += 2;
--					} while((m >>>= 1)!=0);
--				}
--				p += s;
--				si += (W/8)-w;
--			}
--			x *= scale; y *= scale;
--		}
--		ic.setPixels(x, y, s, h, cm, buf, 0, s);
--	}
--
--	private byte[] update_buf;
--
--	private void update_screen(ImageConsumer ic)
--	{
--		long bm = bordchg;
--		boolean chg = false;
--		byte buf[] = update_buf;
--		for(int r=-Mv; r<24+Mv; r++, bm>>>=1) {
--			if((bm&1)!=0) {
--				update_box(ic, r+Mv,0,Mh+32+Mh, buf);
--				chg = true;
--				continue;
--			}
--			if((char)r >= 24)
--				continue;
--			int v = scrchg[r];
--			if(v!=0) {
--				int x=max_bit(v^v-1);
--				update_box(ic, Mv+r,Mh+x, max_bit(v)+1-x, buf);
--				chg = true;
--			}
--		}
--		if(chg)
--			ic.imageComplete(ImageConsumer.SINGLEFRAMEDONE);
--	}
--
--	private void send_change()
--	{
--		int y1,y2,s;
--		long vv;
--loop:
--		for(int i=0;;) {
--			s = scrchg[i];
--			if(s!=0) {
--				y1 = i;
--				for(;;) {
--					scrchg[y2=i]=0;
--					do if(++i == 24)
--						break loop;
--					while(scrchg[i]==0);
--					s |= scrchg[i];
--				}
--			}
--			if(++i<24)
--				continue;
--			vv = bordchg;
--			if(vv==0)
--				return;
--			bordchg = 0;
--			y1 = max_bit(vv^vv-1);
--			y2 = max_bit(vv);
--			int sc8 = scale*8;
--			qaop.new_pixels(0, y1*sc8, (Mh+32+Mh)*sc8, (y2-y1+1)*sc8);
--			return;
--		}
--		int x,w;
--		y1+=Mv; y2+=Mv;
--		if((vv = bordchg)!=0) {
--			bordchg = 0;
--			int v = max_bit(vv^vv-1);
--			if(v<y1) y1=v;
--			v = max_bit(vv);
--			if(v>y2) y2=v;
--			x=0; w=Mh+32+Mh;
--		} else {
--			x = max_bit(s^s-1)+Mh;
--			w = max_bit(s)+(Mh+1)-x;
--		}
--		int sc8 = scale*8;
--		qaop.new_pixels(x*sc8, y1*sc8, w*sc8, (y2-y1+1)*sc8);
--	}
--
--	static final int max_bit(long vv)
--	{
--		int v = (int)(vv>>>32);
--		return (v!=0?32:0) + max_bit(v!=0?v:(int)vv);
--	}
--
--	static final int max_bit(int v)
--	{
--		int b = 0;
--		if((char)v!=v) v>>>=(b=16);
--		if(v>0xFF) {v>>>=8;b+=8;}
--		if(v>0xF) {v>>>=4;b+=4;}
--		return b + (-0x55B0>>>2*v & 3);
--	}
--
--	static final int canonic[] = new int[32768];
--	static {
--		// .bpppiii 76543210 -> bppp biii 01234567
--		for(int a=0; a<0x8000; a+=0x100) {
--			int b = a>>3 & 0x0800;
--			int p = a>>3 & 0x0700;
--			int i = a & 0x0700;
--			if(p!=0) p |= b;
--			if(i!=0) i |= b;
--			canonic[a] = p<<4 | 0xFF;
--			canonic[a|0xFF] = i<<4 | 0xFF;
--			for(int m=1; m<255; m+=2) {
--				if(i!=p) {
--					int xm = m>>>4 | m<<4;
--					xm = xm>>>2&0x33 | xm<<2&0xCC;
--					xm = xm>>>1&0x55 | xm<<1&0xAA;
--					canonic[a|m] = i<<4 | p | xm;
--					canonic[a|m^0xFF] =  p<<4 | i | xm;
--				} else
--					canonic[a|m] = canonic[a|m^0xFF]
--						= p<<4 | 0xFF;
--			}
--		}
--	}
--
--	byte border = (byte)7;		// border color
--	byte border_solid = -1;		// nonnegative: solid border color
--
--	/* audio */
--
--	static final int CHANNEL_VOLUME = 26000;
--	static final int SPEAKER_VOLUME = 49000;
--
--	boolean ay_enabled;
--
--	void ay(boolean y) // enable
--	{
--		if(!y) ay_mix = 0;
--		ay_enabled = y;
--	}
--
--	private int speaker;
--	private static final int sp_volt[];
--
--	protected final byte ay_reg[] = new byte[16];
--
--	private int ay_aper, ay_bper, ay_cper, ay_nper, ay_eper;
--	private int ay_acnt, ay_bcnt, ay_ccnt, ay_ncnt, ay_ecnt;
--	private int ay_gen, ay_mix, ay_ech, ay_dis;
--	private int ay_avol, ay_bvol, ay_cvol;
--	private int ay_noise = 1;
--	private int ay_ekeep; // >=0:hold, ==0:stop
--	private boolean ay_div16;
--	private int ay_eattack, ay_ealt, ay_estep;
--
--	private static final int ay_volt[];
--
--	void ay_write(int n, int v) {
--		switch(n) {
--		case  0: ay_aper = ay_aper&0xF00 | v; break;
--		case  1: ay_aper = ay_aper&0x0FF | (v&=15)<<8; break;
--		case  2: ay_bper = ay_bper&0xF00 | v; break;
--		case  3: ay_bper = ay_bper&0x0FF | (v&=15)<<8; break;
--		case  4: ay_cper = ay_cper&0xF00 | v; break;
--		case  5: ay_cper = ay_cper&0x0FF | (v&=15)<<8; break;
--		case  6: ay_nper = v&=31; break;
--		case  7: ay_mix = ~(v|ay_dis); break;
--		case  8:
--		case  9:
--		case 10:
--			int a=v&=31, x=011<<(n-8);
--			if(v==0) {
--				ay_dis |= x;
--				ay_ech &= ~x;
--			} else if(v<16) {
--				ay_dis &= (x = ~x);
--				ay_ech &= x;
--			} else {
--				ay_dis &= ~x;
--				ay_ech |= x;
--				a = ay_estep^ay_eattack;
--			}
--			ay_mix = ~(ay_reg[7]|ay_dis);
--			a = ay_volt[a];
--			switch(n) {
--			case 8: ay_avol = a; break;
--			case 9: ay_bvol = a; break;
--			case 10: ay_cvol = a; break;
--			}
--			break;
--		case 11: ay_eper = ay_eper&0xFF00 | v; break;
--		case 12: ay_eper = ay_eper&0xFF | v<<8; break;
--		case 13: ay_eshape(v&=15); break;
--		}
--		ay_reg[n] = (byte)v;
--	}
--
--	private void ay_eshape(int v) {
--		if(v<8)
--			v = v<4 ? 1 : 7;
--
--		ay_ekeep = (v&1)!=0 ? 1 : -1;
--		ay_ealt = (v+1&2)!=0 ? 15 : 0;
--		ay_eattack = (v&4)!=0 ? 15 : 0;
--		ay_estep = 15;
--
--		ay_ecnt = -1; // ?
--		ay_echanged();
--	}
--
--	private void ay_echanged()
--	{
--		int v = ay_volt[ay_estep ^ ay_eattack];
--		int x = ay_ech;
--		if((x&1)!=0) ay_avol = v;
--		if((x&2)!=0) ay_bvol = v;
--		if((x&4)!=0) ay_cvol = v;
--	}
--
--	private int ay_tick()
--	{
--		int x = 0;
--		if((--ay_acnt & ay_aper)==0) {
--			ay_acnt = -1;
--			x ^= 1;
--		}
--		if((--ay_bcnt & ay_bper)==0) {
--			ay_bcnt = -1;
--			x ^= 2;
--		}
--		if((--ay_ccnt & ay_cper)==0) {
--			ay_ccnt = -1;
--			x ^= 4;
--		}
--
--		if(ay_div16 ^= true) {
--			ay_gen ^= x;
--			return x & ay_mix;
--		}
--
--		if((--ay_ncnt & ay_nper)==0) {
--			ay_ncnt = -1;
--			if((ay_noise&1)!=0) {
--				x ^= 070;
--				ay_noise ^= 0x28000;
--			}
--			ay_noise >>= 1;
--		}
--
--		if((--ay_ecnt & ay_eper)==0) {
--			ay_ecnt = -1;
--			if(ay_ekeep!=0) {
--				if(ay_estep==0) {
--					ay_eattack ^= ay_ealt;
--					ay_ekeep >>= 1;
--					ay_estep = 16;
--				}
--				ay_estep--;
--				if(ay_ech!=0) {
--					ay_echanged();
--					x |= 0x100;
--				}
--			}
--		}
--		ay_gen ^= x;
--		return x & ay_mix;
--	}
--
--	private int au_value()
--	{
--		int g = ay_mix & ay_gen;
--		int v = speaker;
--		if((g&011)==0) v += ay_avol;
--		if((g&022)==0) v += ay_bvol;
--		if((g&044)==0) v += ay_cvol;
--		return v;
--	}
--
--	private int au_time;
--	private int au_val, au_dt;
--
--	private void au_update() {
--		int t = cpu.time;
--		au_time += (t -= au_time);
--
--		int dv = au_value() - au_val;
--		if(dv != 0) {
--			au_val += dv;
--			audio.step(0, dv);
--		}
--		int dt = au_dt;
--		for(; t>=dt; dt+=16) {
--			if(ay_tick() == 0)
--				continue;
--			dv = au_value() - au_val;
--			if(dv == 0)
--				continue;
--			au_val += dv;
--			audio.step(dt, dv);
--			t -= dt; dt = 0;
--		}
--		au_dt = dt - t;
--		audio.step(t, 0);
--	}
--
--	void au_reset()
--	{
--		/* XXX */
--		speaker = 0;
--		ay_mix = ay_gen = 0;
--		ay_avol = ay_bvol = ay_cvol = 0;
--		ay_ekeep = 0;
--		ay_dis = 077;
--	}
--
--	static boolean muted = false;
--	static int volume = 40; // %
--
--	void mute(boolean v) {
--		muted = v;
--		setvol();
--	}
--
--	int volume(int v) {
--		if(v<0) v=0; else if(v>100) v=100;
--		volume = v;
--		setvol();
--		return v;
--	}
--
--	int volumeChg(int chg) {
--		return volume(volume + chg);
--	}
--
--	static {
--		sp_volt = new int[4];
--		ay_volt = new int[16];
--		setvol();
--	}
--
--	static void setvol()
--	{
--		double a = muted ? 0 : volume/100.;
--		a *= a;
--
--		sp_volt[2] = (int)(SPEAKER_VOLUME*a);
--		sp_volt[3] = (int)(SPEAKER_VOLUME*1.06*a);
--
--		a *= CHANNEL_VOLUME;
--		int n;
--		ay_volt[n=15] = (int)a;
--		do {
--			ay_volt[--n] = (int)(a *= 0.7071);
--		} while(n>1);
--	}
--
--
--	/* tape */
--
--	private boolean check_load()
--	{
--		int pc = cpu.pc();
--		if(cpu.ei() || pc<0x56B || pc>0x604)
--			return false;
--		int sp = cpu.sp();
--		if(pc>=0x5E3) {
--			pc = mem16(sp); sp=(char)(sp+2);
--			if(pc == 0x5E6) {
--				pc = mem16(sp); sp=(char)(sp+2);
--			}
--		}
--		if(pc<0x56B || pc>0x58E)
--			return false;
--		cpu.sp(sp);
--		cpu.ex_af();
--
--		if(tape_changed || tape_ready && tape.length <= tape_blk) {
--			tape_changed = false;
--			tape_blk = 0;
--		}
--		tape_pos = tape_blk;
--		return true;
--	}
-- our version returns the new Z80 value (ish) in a Just, or Nothing if it would have returned false


checkLoad : Spectrum -> Maybe Z80
checkLoad spectrum =
    let
        cpu =
            spectrum.cpu

        pc1 =
            cpu.pc |> toInt
    in
    if (cpu |> get_ei) || pc1 < 0x056B || pc1 > 0x0604 then
        Nothing

    else
        let
            sp1 =
                cpu.env.sp

            ( pc, sp ) =
                if pc1 >= 0x05E3 then
                    let
                        ( pc2, sp2 ) =
                            --( cpu.env |> mem16 sp1 spectrum.rom48k |> .value, char sp1 + 2 )
                            ( cpu.env |> mem16 (sp1 |> toInt) spectrum.rom48k |> .value, sp1 |> incrementBy2 )
                    in
                    if pc2 == 0x05E6 then
                        --( cpu.env |> mem16 sp2 spectrum.rom48k |> .value, char sp2 + 2 )
                        ( cpu.env |> mem16 (sp2 |> toInt) spectrum.rom48k |> .value, sp2 |> incrementBy2 )

                    else
                        ( pc2, sp2 )

                else
                    ( pc1, sp1 )
        in
        if pc < 0x056B || pc > 0x058E then
            Nothing

        else
            let
                env =
                    cpu.env

                new_env =
                    { env | sp = sp }

                new_cpu =
                    { cpu | env = new_env }
            in
            Just (new_cpu |> ex_af)



--
--	private boolean loading, stop_loading;
--	private byte[] tape;
--	private int tape_blk;
--	private int tape_pos;
--	private boolean tape_changed = false;
--	private boolean tape_ready = false;
--
--	public synchronized void stop_loading()
--	{
--		stop_loading = true;
--		try {
--			while(loading) wait();
--		} catch(InterruptedException e) {
--			currentThread().interrupt();
--		}
--	}
--
--	public synchronized void tape(byte[] tape, boolean end)
--	{
--		if(tape==null)
--			tape_changed = true;
--		tape_ready = end;
--		this.tape = tape;
--	}
--
--	private final boolean do_load(byte[] tape, boolean ready)
--	{


type alias TapeState =
    { p : Int
    , ix : Int
    , de : Int
    , h : Int
    , l : Int
    , a : Int
    , f : Int
    , rf : Int
    , data : List Int
    , break : Bool
    }


doLoad : Z80 -> Z80ROM -> Z80Tape -> ( Z80, Bool )
doLoad cpu z80rom tape =
    --		if(tape_changed || (keyboard[7]&1)==0) {
    --			cpu.f(0);
    --			return false;
    --		}
    if (cpu.env.keyboard.keyboard |> Vector8.get Vector8.Index7 |> Bitwise.and 1) == 0 then
        let
            flags =
                set_flags 0 cpu.flags.a
        in
        ( { cpu | flags = flags }, False )

    else
        --		int p = tape_pos;
        --
        --		int ix = cpu.ix();
        --		int de = cpu.de();
        --		int h, l = cpu.hl(); h = l>>8 & 0xFF; l &= 0xFF;
        --		int a = cpu.a();
        --		int f = cpu.f();
        --		int rf = -1;
        --		if(p == tape_blk) {
        --			p += 2;
        --			if(tape.length < p) {
        --				if(ready) {
        --					cpu.pc(cpu.pop());
        --					cpu.f(cpu.FZ);
        --				}
        --				return !ready;
        --			}
        --			tape_blk = p + (tape[p-2]&0xFF | tape[p-1]<<8&0xFF00);
        --			h = 0;
        --		}
        let
            p =
                tape.tapePos

            h0 =
                cpu.main.hl |> toInt |> shiftRightBy8

            h1 =
                if p.position == 0 then
                    0

                else
                    h0

            bool =
                case tape.tapfiles |> Dict.get p.tapfileNumber of
                    Just aTapfile ->
                        let
                            tapeBlk =
                                aTapfile.block.dataLength

                            startState =
                                { p = tape.tapePos.position
                                , ix = cpu.main.ix |> toInt
                                , de = cpu.main |> get_de
                                , h = h1
                                , l = cpu.main |> get_l HL
                                , a = cpu.flags.a
                                , f = cpu.flags |> get_flags
                                , rf = -1
                                , data = Dict.empty
                                , break = False
                                }

                            new_data =
                                --		for(;;) {
                                aTapfile.block.data
                                    |> List.foldl
                                        (\item state ->
                                            let
                                                --	if(p == tape_blk) {
                                                --	   rf = cpu.FZ;
                                                --	   break;
                                                --	}
                                                --	if(p == tape.length) {
                                                --		if(ready)
                                                --			rf = cpu.FZ;
                                                --		break;
                                                --	}
                                                ( break1, rf1 ) =
                                                    if not state.break && state.p == tapeBlk then
                                                        ( True, c_FZ )

                                                    else
                                                        ( state.break, state.rf )

                                                --	l = tape[p++]&0xFF;
                                                l =
                                                    if not break1 then
                                                        Bitwise.and item 0xFF

                                                    else
                                                        state.l

                                                --	h ^= l;
                                                h =
                                                    if not break1 then
                                                        Bitwise.xor state.h l

                                                    else
                                                        state.h

                                                --	if(de == 0) {
                                                --		a = h;
                                                --		rf = 0;
                                                --		if(a<1)
                                                --			rf = cpu.FC;
                                                --		break;
                                                --	}
                                                ( a, rf2, break ) =
                                                    if not break1 && state.de == 0 then
                                                        if h < 1 then
                                                            ( h, c_FC, True )

                                                        else
                                                            ( h, 0, True )

                                                    else
                                                        ( state.a, rf1, break1 )

                                                --		if((f&cpu.FZ)==0) {
                                                --			a ^= l;
                                                --			if(a != 0) {
                                                --				rf = 0;
                                                --				break;
                                                --			}
                                                --			f |= cpu.FZ;
                                                --			continue;
                                                --		}
                                                a_rf_f_break_1 =
                                                    if not break && Bitwise.and state.f c_FZ == 0 then
                                                        let
                                                            new_a =
                                                                Bitwise.xor a l
                                                        in
                                                        if new_a /= 0 then
                                                            { data = state.data, a = new_a, rf = 0, break = True, f = state.f, continue = False }

                                                        else
                                                            { data = state.data, a = new_a, rf = rf2, break = break, f = Bitwise.or state.f c_FZ, continue = True }

                                                    else
                                                        { data = state.data, a = a, rf = rf2, break = break, f = state.f, continue = False }

                                                --		if((f&cpu.FC)!=0)
                                                --			mem(ix, l);
                                                --		else {
                                                --			a = mem(ix) ^ l;
                                                --			if(a != 0) {
                                                --				rf = 0;
                                                --				break;
                                                --			}
                                                --		}
                                                data_a_rf_break =
                                                    if not a_rf_f_break_1.break && not a_rf_f_break_1.continue then
                                                        if Bitwise.and a_rf_f_break_1.f c_FC /= 0 then
                                                            { a_rf_f_break_1 | data = a_rf_f_break_1.data |> Dict.insert state.ix l }

                                                        else
                                                            let
                                                                new_a =
                                                                    Bitwise.xor (mem state.ix cpu.env.time z80rom cpu.env.ram |> .value) l
                                                            in
                                                            if new_a /= 0 then
                                                                { a_rf_f_break_1 | a = new_a, rf = 0, break = True }

                                                            else
                                                                { a_rf_f_break_1 | a = new_a }

                                                    else
                                                        a_rf_f_break_1

                                                --			ix = (char)(ix+1);
                                                --			de--;
                                                --		}
                                            in
                                            { state
                                                | ix = char (state.ix + 1)
                                                , de = state.de - 1
                                                , data = data_a_rf_break.data
                                                , h = h
                                                , l = l
                                                , a = data_a_rf_break.a
                                                , f = data_a_rf_break.f
                                                , break = data_a_rf_break.break
                                                , rf = data_a_rf_break.rf
                                            }
                                        )
                                        startState
                        in
                        True

                    Nothing ->
                        False
        in
        ( cpu, bool )



--
--		cpu.ix(ix);
--		cpu.de(de);
--		cpu.hl(h<<8|l);
--		cpu.a(a);
--		if(rf>=0) {
--			f = rf;
--			cpu.pc(cpu.pop());
--		}
--		cpu.f(f);
--		tape_pos = p;
--		return rf<0;
--	}
--
--	/* LOAD "" */
--
--	public final void basic_exec(String cmd)
--	{
--		rom = rom48k;
--
--		cpu.i(0x3F);
--		int p=16384;
--		do mem(p++, 0); while(p<22528);
--		do mem(p++, 070); while(p<23296);
--		do mem(p++, 0); while(p<65536);
--		mem16(23732, --p); // P-RAMT
--		p -= 0xA7;
--		System.arraycopy(rom48k, 0x3E08, ram, p-16384, 0xA8);
--		mem16(23675, p--); // UDG
--		mem(23608, 0x40); // RASP
--		mem16(23730, p); // RAMTOP
--		mem16(23606, 0x3C00); // CHARS
--		mem(p--, 0x3E);
--		cpu.sp(p);
--		mem16(23613, p-2); // ERR-SP
--		cpu.iy(0x5C3A);
--		cpu.im(1);
--		cpu.ei(true);
--
--		mem16(23631, 0x5CB6); // CHANS
--		System.arraycopy(rom48k, 0x15AF, ram, 0x1CB6, 0x15);
--		p = 0x5CB6+0x14;
--		mem16(23639, p++); // DATAADD
--		mem16(23635, p); // PROG
--		mem16(23627, p); // VARS
--		mem(p++, 0x80);
--		mem16(23641, p); // E-LINE
--		for(int i=0;i<cmd.length();i++)
--			mem(p++, cmd.charAt(i));
--		mem16(p, 0x800D); p+=2;
--		mem16(23649, p); // WORKSP
--		mem16(23651, p); // STKBOT
--		mem16(23653, p); // STKEND
--
--		mem(23693, 070); mem(23695, 070); mem(23624, 070);
--		mem16(23561, 0x0523);
--
--		mem(23552, 0xFF); mem(23556, 0xFF); // KSTATE
--
--		System.arraycopy(rom48k, 0x15C6, ram, 0x1C10, 14);
--
--		mem16(23688, 0x1821); // S-POSN
--		mem(23659, 2); // DF-SZ
--		mem16(23656, 0x5C92); // MEM
--		mem(23611, 0x0C); // FLAGS
--
--/*		int r = (int)Math.floor(Math.random() * 128);
--		cpu.r(r);
--		mem(23672, r); // FRAMES
--*/
--		cpu.pc(4788);
--		au_reset();
--	}
--}
