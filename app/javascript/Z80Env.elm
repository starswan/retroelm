--
-- $Id$
--
module Z80Env exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (shiftLeftBy8, shiftRightBy8, toHexString)
import Z80Ram exposing (Z80Ram, c_FRSTART, getRamValue)
import Z80Rom exposing (Z80ROM, getROMValue, set_spectrum_rom)


type alias CpuTimeCTime =
    {
            cpu_time: Int,
            ctime: Int
    }

-- changing this to an array results in a recursion error in the browser :-(
type alias Z80Env =
    {
            rom48k: Z80ROM,
            ram: Z80Ram,
            keyboard: Keyboard,
            time: CpuTimeCTime
    }

type alias Z80EnvWithValue =
    {
        env: Z80Env,
        value: Int
    }

type alias CpuTimeWithValue =
    {
        time: CpuTimeCTime,
        value: Int
    }

type alias ValueWithTime =
    {
        value: Int,
        cpu_time: Int
    }

c_NOCONT = 99999
c_SCRENDT = 191*224+126

z80env_constructor =
    Z80Env Z80Rom.constructor Z80Ram.constructor Keyboard.constructor (CpuTimeCTime c_FRSTART 0)

set_rom: Array Int -> Z80Env -> Z80Env
set_rom romdata z80env =
   let
       rommy = z80env.rom48k |> set_spectrum_rom romdata
   in
      { z80env | rom48k = rommy }

--public final int m1(int addr, int ir) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--
--	addr -= 0x4000;
--	if((addr&0xC000) == 0)
--		cont1(0);
--	ctime = NOCONT;
--	if((ir&0xC000) == 0x4000)
--		ctime = cpu.time + 4;
--	if(addr >= 0)
--		return ram[addr];
--	n = rom[addr+=0x4000];
--	if(if1rom!=null && (addr&0xE8F7)==0) {
--		if(addr==0x0008 || addr==0x1708) {
--			if(rom==rom48k) rom = if1rom;
--		} else if(addr==0x0700) {
--			if(rom==if1rom) rom = rom48k;
--		}
--	}
--	return n;
--}
m1: Int -> Int -> Z80Env -> CpuTimeWithValue
m1 tmp_addr ir z80env =
    let
       n = z80env.time.cpu_time - z80env.time.ctime
       time_1 = if n > 0 then
                  z80env.time |> cont n
                else
                  z80env.time
       addr = tmp_addr - 0x4000
       time_2 = if ((and addr 0xC000) == 0) then
                       time_1 |> cont1 0
                     else
                       time_1
       ctime = if and ir 0xC000 == 0x4000 then
                  time_2.cpu_time + 4
               else
                  c_NOCONT
       value = if addr >= 0 then
                  z80env.ram |> getRamValue addr
               else
                  -- not implementing IF1 switching for now
                  z80env.rom48k |> getROMValue tmp_addr
    in
        CpuTimeWithValue { time_2 | ctime = ctime } value

m1_env: Int -> Int -> Z80Env -> Z80EnvWithValue
m1_env tmp_addr ir z80env =
    let
        time_value = z80env |> m1 tmp_addr ir
    in
       Z80EnvWithValue { z80env | time = time_value.time } time_value.value
--public final int mem(int addr) {
--	int n = cpu.time - ctime;
--	if(n>0) cont(n);
--	ctime = NOCONT;
--
--	addr -= 0x4000;
--	if(addr>=0) {
--		if(addr<0x4000) {
--			cont1(0);
--			ctime = cpu.time + 3;
--		}
--		return ram[addr];
--	}
--	return rom[addr+0x4000];
--}
mem: Int -> Z80Env -> Z80EnvWithValue
mem base_addr z80env_ctime =
    let
       old_time = z80env_ctime.time
       n = old_time.cpu_time - old_time.ctime
       z80env = if n > 0 then
                   z80env_ctime |> cont_env n
                else
                   z80env_ctime
       addr = base_addr - 0x4000
       (new_time, ctime, value) = if addr >= 0 then
                                    if addr < 0x4000 then
                                       let
                                          new_z80_time = z80env.time |> cont1 0
                                       in
                                          (new_z80_time, new_z80_time.cpu_time + 3, z80env.ram |> getRamValue addr)
                                    else
                                       (z80env.time, c_NOCONT, z80env.ram |> getRamValue addr)
                                 else
                                    (z80env.time, c_NOCONT, z80env.rom48k |> getROMValue base_addr)
    in
        Z80EnvWithValue { z80env | time = { new_time | ctime = ctime } } value
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
mem16: Int -> Z80Env -> CpuTimeWithValue
mem16 addr z80env =
    let
       n = z80env.time.cpu_time - z80env.time.ctime
       time_0 = if n > 0 then
                  z80env.time |> cont n
                else
                  z80env.time
       addr1 = addr - 0x3FFF
    in
      if and addr1 0x3FFF /= 0 then
         if addr1 < 0 then
           let
              low0 = getROMValue addr z80env.rom48k
              high0 = getROMValue (addr1 + 0x4000) z80env.rom48k
           in
              CpuTimeWithValue { time_0 | ctime = c_NOCONT } (Bitwise.or low0 (shiftLeftBy8 high0))
         else
           let
              low1 = getRamValue (addr - 0x4000) z80env.ram
              high1 = getRamValue addr1 z80env.ram
              time_1 = if addr1 < 0x4000 then
                            time_0 |> cont1 0 |> cont1 3 |> add_cpu_time_ctime 6
                         else
                            { time_0 | ctime = c_NOCONT }
        in
            CpuTimeWithValue time_1 (Bitwise.or low1 (shiftLeftBy8 high1))
      else
       let
         addr1shift14 = shiftRightBy 14 addr1
       in
         case addr1shift14 of
          0 ->
            let
                new_time_s0 = time_0 |> cont1 3
                low_s0 = z80env.rom48k |> getROMValue addr
                high_s0 = getRamValue 0 z80env.ram
            in
                CpuTimeWithValue new_time_s0  (or low_s0 (shiftLeftBy8 high_s0))
          1 ->
            let
                new_time_s1 = time_0 |> cont1 0
                low_s1 = getRamValue (addr - 0x4000) z80env.ram
                high_s1 = getRamValue addr1 z80env.ram
            in
                CpuTimeWithValue new_time_s1 (or low_s1 (shiftLeftBy8 high_s1))
          2 ->
            let
                low_s2 = getRamValue (addr - 0x4000) z80env.ram
                high_s2 = getRamValue addr1 z80env.ram
            in
                CpuTimeWithValue { time_0 | ctime = c_NOCONT } (or low_s2 (shiftLeftBy8 high_s2))
          _ ->
            let
                low = getRamValue 0xBFFF z80env.ram
                high = getRamValue 0 z80env.ram
            in
                CpuTimeWithValue { time_0 | ctime = c_NOCONT } (or low (shiftLeftBy8 high))

mem16_env: Int -> Z80Env -> Z80EnvWithValue
mem16_env addr z80env =
    let
       timer = z80env |> mem16 addr
    in
       Z80EnvWithValue { z80env | time = timer.time } timer.value

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
   { z80env | ram = z80env.ram |> Z80Ram.setRamValue addr value }

set_mem: Int -> Int -> Z80Env -> Z80Env
set_mem z80_addr value old_z80env =
   let
      time_0 = old_z80env.time
      n = time_0.cpu_time - time_0.ctime
      z80env = if n > 0 then
                old_z80env |> cont_env n
             else
                { old_z80env | time = { time_0 | ctime = c_NOCONT } }
      addr = z80_addr - 0x4000
      (new_env, ctime) = if addr < 0x4000 then
                            if addr < 0 then
                               (z80env, c_NOCONT)
                            else
                               let
                                   time_1 = z80env.time |> cont1 0
                                   new_ctime = time_1.cpu_time + 3
                                   ram_value = getRamValue addr z80env.ram
                                   z80env_1 = { z80env | time = time_1 }
                               in
                                  if ram_value == value then
                                     (z80env_1 , new_ctime)
                                  else
                                     (z80env_1 |> set_ram addr value, new_ctime)
                         else
                            (z80env |> set_ram addr value, c_NOCONT)
      time = new_env.time
   in
      { new_env | time = { time | ctime = ctime } }
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
                       n = z80env.time.cpu_time - z80env.time.ctime
                       env = if (n > 0) then
                                cont_env n z80env
                             else
                                z80env
                       env_1 = { env | time = CpuTimeCTime z80env.time.cpu_time c_NOCONT }
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
cont1_env: Int -> Z80Env -> Z80Env
cont1_env tmp_t z80env  =
    { z80env | time = z80env.time |> cont1 tmp_t }

cont1: Int -> CpuTimeCTime -> CpuTimeCTime
cont1 tmp_t z80_time  =
    let
        t = tmp_t + z80_time.cpu_time
    in
        if (t<0) || (t>=c_SCRENDT) then
           z80_time
        else
           if (Bitwise.and t 7) >= 6 then
               z80_time
           else
               if (modBy 224 t) < 126 then
                  z80_time |> add_cpu_time_ctime (6 - (Bitwise.and t 7))
               else
                   z80_time
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
contimpl: Int -> Int -> CpuTimeCTime -> CpuTimeCTime
contimpl tmp_n tmp_s z80env =
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
            { z80env | cpu_time = z80env.cpu_time + (ntk.t + 6*n4) }

cont_env: Int -> Z80Env-> Z80Env
cont_env tmp_n z80env =
    let
       x = z80env.time |> cont tmp_n
    in
       { z80env | time = x }

cont: Int -> CpuTimeCTime-> CpuTimeCTime
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
                    z80env |> contimpl tmp_n tmp_s

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
      n = z80env.time.cpu_time - z80env.time.ctime
      env1 = if n > 0 then
                z80env |> cont_env n
             else
                z80env
      env2 = if and portn 0xC000 /= 0x4000 then
               let
                  env3 = if and portn 0x0001 == 0 then
                            env1 |> cont1_env 1
                         else
                            env1
               in
                  { env3 | time = CpuTimeCTime z80env.time.cpu_time c_NOCONT }
             else
               let
                   env3 = { env1 | time = CpuTimeCTime z80env.time.cpu_time env1.time.cpu_time }
                   contval = and portn 1 |> shiftLeftBy 1
                   env4 = env3 |> cont_env (2 + contval)
               in
                  { env4 | time = CpuTimeCTime z80env.time.cpu_time (env4.time.cpu_time + 4) }
   in
      env2

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
out portnum _ env_in =
   let
      env = env_in |> cont_port portnum
   in
      env

z80_in: Int -> Z80Env -> Z80EnvWithValue
z80_in portnum env_in =
   let
      env = env_in |> cont_port portnum
      --x = debug_log "z80_in" (portnum |> toHexString) Nothing
      value = env.keyboard |> z80_keyboard_input portnum
   in
      Z80EnvWithValue env value

add_cpu_time_env: Int -> Z80Env -> Z80Env
add_cpu_time_env value z80env =
   { z80env | time = CpuTimeCTime (z80env.time.cpu_time + value) z80env.time.ctime }

add_cpu_time_ctime: Int -> CpuTimeCTime -> CpuTimeCTime
add_cpu_time_ctime value time =
   { time | cpu_time = time.cpu_time + value }

reset_cpu_time: Z80Env -> Z80Env
reset_cpu_time z80env =
   { z80env | time = CpuTimeCTime c_FRSTART z80env.time.ctime }