--
-- $Id$
--
module Z80Env exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (listToDict, shiftLeftBy8, shiftRightBy8)
import Z80Memory exposing (Z80Memory, getValue)
import Z80Rom exposing (Z80ROM, getROMValue)

-- changing this to an array results in a recursion error in the browser :-(
type alias Z80Env =
    {
            rom48k: Z80ROM,
            ram: Z80Memory,
            keyboard: Keyboard,
            ctime: Int,
            cpu_time: Int
    }

type alias Z80EnvWithValue =
    {
        env: Z80Env,
        value: Int
    }

type alias ValueWithTime =
    {
        value: Int,
        cpu_time: Int
    }

c_NOCONT = 99999
c_SCRENDT = 191*224+126

c_FRSTART = -14335
c_FRTIME = 69888
--c_FRTIME = 14350

z80env_constructor =
    let
        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        startrange = List.repeat 6144 0
        whiterange = List.repeat (6912 - 6144) 0x38 -- white
        endrange = List.repeat (49152 - 6912) 0

        ram = List.concat [startrange, whiterange, endrange]

        keyboard = Keyboard (List.repeat 8 0xFF) []
    in
        Z80Env Z80Rom.constructor (Z80Memory.constructor ram) keyboard 0 c_FRSTART

set_rom: Array Int -> Z80Env -> Z80Env
set_rom romdata z80env =
   let
      romDict = listToDict (Array.toList romdata)
   in
      { z80env | rom48k = romDict }

m1: Int -> Int -> Z80Env -> Z80EnvWithValue
m1 tmp_addr ir z80env_ =
    let
       n = z80env_.cpu_time - z80env_.ctime
       z80env = if n > 0 then
                  z80env_ |> cont n
                else
                  z80env_
       addr = tmp_addr - 0x4000
       z80env_1 = if ((and addr 0xC000) == 0) then
                       z80env |> cont1 0
                     else
                       z80env
       ctime = if and ir 0xC000 == 0x4000 then
                  z80env_1.cpu_time + 4
               else
                  c_NOCONT
       value = if addr >= 0 then
                  z80env_1.ram |> getValue addr
               else
                  -- not implementing IF1 switching for now
                  z80env_1.rom48k |> getROMValue tmp_addr
    in
        Z80EnvWithValue { z80env_1 | ctime = ctime } value

mem: Int -> Z80Env -> Z80EnvWithValue
mem base_addr z80env_ctime =
    let
       n = z80env_ctime.cpu_time - z80env_ctime.ctime
       z80env = if n > 0 then
                  z80env_ctime |> cont n
                else
                  z80env_ctime
       addr = base_addr - 0x4000
       (new_env, ctime, value) = if addr >= 0 then
                                   if addr < 0x4000 then
                                     let
                                        new_z80 = z80env |> cont1 0
                                     in
                                        (new_z80, new_z80.cpu_time + 3, z80env.ram |> getValue addr)
                                   else
                                     (z80env_ctime, c_NOCONT, z80env.ram |> getValue addr)
                                 else
                                   (z80env_ctime, c_NOCONT, z80env.rom48k |> getROMValue base_addr)
    in
        Z80EnvWithValue { new_env | ctime = ctime } value
--	public final int mem16(int addr) {
--		int n = cpu.time - ctime;
--		if(n>0) cont(n);
--		ctime = NOCONT;
--
--		int addr1 = addr-0x3FFF;
--		if((addr1&0x3FFF)!=0) {
--			if(addr1<0)
--				return rom[addr] | rom[addr1+0x4000]<<8;
--			if(addr1<0x4000) {
--				cont1(0); cont1(3);
--				ctime = cpu.time + 6;
--			}
--			return ram[addr-0x4000] | ram[addr1]<<8;
--		}
--		switch(addr1>>>14) {
--		case 0:
--			cont1(3);
--			ctime = cpu.time + 6;
--			return rom[addr] | ram[0]<<8;
--		case 1:
--			cont1(0);
--		case 2:
--			return ram[addr-0x4000] | ram[addr1]<<8;
--		default:
--			return ram[0xBFFF] | rom[0]<<8;
--		}
--	}
mem16impl: Int -> Int -> Z80Env -> Z80EnvWithValue
mem16impl addr addr1 z80env =
   if and addr1 0x3FFF /= 0 then
      if addr1 < 0 then
        let
            low = getROMValue addr z80env.rom48k
            high = getROMValue (addr1 + 0x4000) z80env.rom48k
        in
            Z80EnvWithValue { z80env | ctime = c_NOCONT } (Bitwise.or low (shiftLeftBy8 high))
      else
        let
            low = getValue (addr - 0x4000) z80env.ram
            high = getValue addr1 z80env.ram
            z80env_1 = if addr1 < 0x4000 then
                          let
                             x = z80env
                                 |> cont1 0
                                 |> cont1 3
                          in
                             { x | cpu_time = x.cpu_time + 6 }
                       else
                          { z80env | ctime = c_NOCONT }
        in
            Z80EnvWithValue z80env_1 (Bitwise.or low (shiftLeftBy8 high))
   else
      let
        addr1shift14 = shiftRightBy 14 addr1
      in
        if addr1shift14 == 0 then
            let
                new_z80 = cont1 3 z80env
                low = new_z80.rom48k |> getROMValue addr
                high = getValue 0 new_z80.ram
            in
                Z80EnvWithValue new_z80 (or low (shiftLeftBy8 high))
        else if addr1shift14 == 1 then
            let
                new_env = z80env |> cont1 0
                low = getValue (addr - 0x4000) new_env.ram
                high = getValue addr1 new_env.ram
            in
                Z80EnvWithValue new_env (or low (shiftLeftBy8 high))
        else if addr1shift14 == 2 then
            let
                low = getValue (addr - 0x4000) z80env.ram
                high = getValue addr1 z80env.ram
            in
                Z80EnvWithValue { z80env | ctime = c_NOCONT } (or low (shiftLeftBy8 high))
        else
            let
                low = getValue 0xBFFF z80env.ram
                high = getValue 0 z80env.ram
            in
                Z80EnvWithValue { z80env | ctime = c_NOCONT } (or low (shiftLeftBy8 high))

mem16: Int -> Z80Env -> Z80EnvWithValue
mem16 addr z80env_ctime =
    let
       n = z80env_ctime.cpu_time - z80env_ctime.ctime
       z80env = if n > 0 then
                  z80env_ctime |> cont n
                else
                  z80env_ctime
    in
       z80env |> mem16impl addr (addr - 0x3FFF)
--
--public final void mem(int addr, int v) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--	ctime = NOCONT;
--
--	addr -= 0x4000;
--	if(addr < 0x4000) {
--		if(addr < 0) return;
--		cont1(0);
--		ctime = cpu.time + 3;
--		if(ram[addr]==v)
--			return;
--		if(addr<6912)
--			refresh_screen();
--	}
--	ram[addr] = v;
--}
set_ram: Int -> Int -> Z80Env -> Z80Env
set_ram addr value z80env =
   --let
      --ram_value = getValue addr z80env.ram
      --n = if addr == 0x1CB6 || addr == 0x1CB7 then
      --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
      --    else
      --       Nothing
   --in
   { z80env | ram = z80env.ram |> Z80Memory.set_value addr value }

set_mem: Int -> Int -> Z80Env -> Z80Env
set_mem z80_addr value old_z80env =
   let
      n = old_z80env.cpu_time - old_z80env.ctime
      z80env = if n > 0 then
                old_z80env |> cont n
             else
                { old_z80env | ctime = c_NOCONT }
      addr = z80_addr - 0x4000
      (new_env, ctime) = if addr < 0x4000 then
                            if addr < 0 then
                               (z80env, c_NOCONT)
                            else
                               let
                                   z80env_1 = z80env |> cont1 0
                                   new_time = z80env.cpu_time + 3
                                   ram_value = getValue addr z80env.ram
                               in
                                  if ram_value == value then
                                     (z80env_1 , new_time)
                                  else
                                     if addr < 6912 then
                                        (z80env_1 |> refresh_screen |> set_ram addr value, new_time)
                                     else
                                        (z80env_1 |> set_ram addr value, new_time)
                         else
                            (z80env |> set_ram addr value, c_NOCONT)
   in
      { new_env | ctime = ctime }
--public final void mem16(int addr, int v) {
--
--	int addr1 = addr-0x3FFF;
--	if((addr1&0x3FFF)!=0) {
--		int n = cpu.time - ctime;
--		if(n>0) cont(n);
--		ctime = NOCONT;
--
--		if(addr1<0) return;
--		if(addr1>=0x4000) {
--			ram[addr1-1] = v&0xFF;
--			ram[addr1] = v>>>8;
--			return;
--		}
--	}
--	mem(addr, v&0xFF);
--	cpu.time += 3;
--	mem((char)(addr+1), v>>>8);
--	cpu.time -= 3;
--}
set_mem16: Int -> Int -> Z80Env -> Z80Env
set_mem16 addr value z80env =
   let
      addr1 = addr - 0x3FFF
      new_env = if (Bitwise.and addr1 0x3FFF) /= 0 then
                    let
                       n = z80env.cpu_time - z80env.ctime
                       env = if (n > 0) then
                                cont n z80env
                             else
                                z80env
                       env_1 = { env | ctime = c_NOCONT }
                       nenv = if addr1 < 0 then
                                 env_1
                              else
                                 if addr1 >= 0x4000 then
                                    env_1
                                    |> set_ram (addr - 1) (Bitwise.and value 0xFF)
                                    |> set_ram addr (shiftRightBy8 value)
                                 else
                                    env_1
                                    |> set_mem addr (Bitwise.and value 0xFF)
                                    |> set_mem (addr + 1) (shiftRightBy8 value)
                    in
                       nenv
                else
                    let
                       nenv = z80env
                              |> set_mem addr (Bitwise.and value 0xFF)
                              |> set_mem (addr + 1) (shiftRightBy8 value)
                    in
                      nenv
    in
       new_env

--	private final void cont1(int t) {
--		t += cpu.time;
--		if(t<0 || t>=SCRENDT) return;
--		if((t&7) >= 6) return;
--		if(t%224 < 126)
--			cpu.time += 6 - (t&7);
--	}
cont1: Int -> Z80Env -> Z80Env
cont1 tmp_t z80  =
    let
        t = tmp_t + z80.cpu_time
    in
        if (t<0) || (t>=c_SCRENDT) then
           z80
        else
           if (Bitwise.and t 7) >= 6 then
               z80
           else
               if (modBy 224 t) < 126 then
                  { z80 | cpu_time = z80.cpu_time + 6 - Bitwise.and t 7 }
               else
                   z80
--
--	private final void cont(int n) {
--		int s, k;
--		int t = ctime;
--		if(t+n <= 0) return;
--		s = SCRENDT - t;
--		if(s < 0) return;
--		s %= 224;
--		if(s > 126) {
--			n -= s-126;
--			if(n <= 0) return;
--			t = 6; k = 15;
--		} else {
--			k = s>>>3;
--			s &= 7;
--			if(s == 7) {
--				s--;
--				if(--n == 0) return;
--			}
--			t = s;
--		}
--		n = n-1 >> 1;
--		if(k<n) n = k;
--		cpu.time += t + 6*n;
--	}

-- Helper implementation function for cont
contimpl: Z80Env -> Int -> Int -> Z80Env
contimpl z80env tmp_n tmp_s =
    let
        s = modBy 224 tmp_s
        (ntk) = if s > 126 then
                   { n = s - 126, t = 6, k = 15, o = False }
                else
                    let
                        k2 = shiftRightBy 3 s
                        s2 = Bitwise.and s 7
                        (s3, n2, override) = if s2 == 7 then
                        -- in (only) this branch we need to bale if n == 1
                                                 (s2 - 1, tmp_n - 1, tmp_n == 1)
                                              else
                                                 (s2, tmp_n, False)
                     in
                        { n = n2, t = s3, k = k2, o = override }
        n3 = shiftRightBy 1 (ntk.n - 1)
        n4 = if ntk.k<n3 then
                ntk.k
             else
                n3
    in
        if ntk.o then
            z80env
        else
            { z80env | cpu_time = z80env.cpu_time + ntk.t + 6*n4 }

cont: Int -> Z80Env-> Z80Env
cont tmp_n z80env =
    let
        tmp_t = z80env.ctime
    in
        if tmp_t + tmp_n <= 0 then
            z80env
        else
            let
                tmp_s = c_SCRENDT - tmp_t
            in
                if tmp_s < 0 then
                    z80env
                else
                    contimpl z80env tmp_n tmp_s

--private void cont_port(int port)
--{
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--
--	if((port&0xC000) != 0x4000) {
--		if((port&0x0001)==0)
--			cont1(1);
--		ctime = NOCONT;
--	} else {
--		ctime = cpu.time;
--		cont(2 + ((port&1)<<1));
--		ctime = cpu.time+4;
--	}
--}
cont_port: Int -> Z80Env -> Z80Env
cont_port portn z80env =
   let
      n = z80env.cpu_time - z80env.ctime
      env1 = if n > 0 then
                z80env |> cont n
             else
                z80env
      env2 = if and portn 0xC000 /= 0x4000 then
               let
                  env3 = if and portn 0x0001 == 0 then
                            env1 |> cont1 1
                         else
                            env1
               in
                  { env3 | ctime = c_NOCONT }
             else
               let
                   env3 = { env1 | ctime = env1.cpu_time }
                   contval = and portn 1 |> shiftLeftBy 1
                   env4 = env3 |> cont (2 + contval)
               in
                  { env4 | ctime = env4.cpu_time + 4 }
   in
      env2

--private final void refresh_screen() {
--	int ft = cpu.time;
--	if(ft < refrs_t)
--		return;
--	final int flash = this.flash;
--	int a = refrs_a, b = refrs_b;
--	int t = refrs_t, s = refrs_s;
--	do {
--		int sch = 0;
--
--		int v = ram[a]<<8 | ram[b++];
--		if(v>=0x8000) v ^= flash;
--		v = canonic[v];
--		if(v!=screen[s]) {
--			screen[s] = v;
--			sch = 1;
--		}
--
--		v = ram[a+1]<<8 | ram[b++];
--		if(v>=0x8000) v ^= flash;
--		v = canonic[v];
--		if(v!=screen[++s]) {
--			screen[s] = v;
--			sch += 2;
--		}
--
--		if(sch!=0)
--			scrchg[a-0x1800>>5] |= sch<<(a&31);
--
--		a+=2; t+=8; s++;
--		if((a&31)!=0) continue;
--		// next line
--		t+=96; s+=2*Mh;
--		a-=32; b+=256-32;
--		if((b&0x700)!=0) continue;
--		// next row
--		a+=32; b+=32-0x800;
--		if((b&0xE0)!=0) continue;
--		// next segment
--		b+=0x800-256;
--		if(b>=6144) {
--			t = REFRESH_END;
--			break;
--		}
--	} while(ft >= t);
--	refrs_a = a; refrs_b = b;
--	refrs_t = t; refrs_s = s;
--}
refresh_screen: Z80Env -> Z80Env
refresh_screen z80env =
   z80env

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
out: Int -> Int -> Z80Env -> Z80Env
out portnum value env_in =
   let
      env = env_in |> cont_port portnum
   in
      env

z80_in: Int -> Z80Env -> Z80EnvWithValue
z80_in portnum env_in =
   let
      env = env_in |> cont_port portnum
      value = env.keyboard |> z80_keyboard_input portnum
   in
      Z80EnvWithValue env value