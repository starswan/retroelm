--
-- $Id$
--
module Z80Env exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (listToDict, shiftLeftBy8, shiftRightBy8)
import Z80Ram exposing (Z80Ram, add_cpu_time_ram, c_FRSTART, getRam16Value, getRamValue)
import Z80Rom exposing (Z80ROM, getROMValue)

-- changing this to an array results in a recursion error in the browser :-(
type alias Z80Env =
    {
            rom48k: Z80ROM,
            ram: Z80Ram,
            keyboard: Keyboard,
            ctime: Int
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

z80env_constructor =
    Z80Env Z80Rom.constructor Z80Ram.constructor Keyboard.constructor 0

set_rom: Array Int -> Z80Env -> Z80Env
set_rom romdata z80env =
   let
      romDict = listToDict (Array.toList romdata)
   in
      { z80env | rom48k = romDict }

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
m1: Int -> Int -> Z80Env -> Z80EnvWithValue
m1 tmp_addr ir z80env_ =
    let
       n = z80env_.ram.cpu_time - z80env_.ctime
       cont_value_0 = if n > 0 then
                         z80env_ |> cont n
                      else
                         0
       z80env = z80env_ |> add_cpu_time_env cont_value_0
       addr = tmp_addr - 0x4000
       cont_value = if ((and addr 0xC000) == 0) then
                       z80env |> cont1 0
                    else
                       0
       z80env_1 = z80env |> add_cpu_time_env cont_value
       ctime = if and ir 0xC000 == 0x4000 then
                  z80env_1.ram.cpu_time + 4
               else
                  c_NOCONT
       value = if addr >= 0 then
                  z80env_1.ram |> getRamValue addr
               else
                  -- not implementing IF1 switching for now
                  z80env_1.rom48k |> getROMValue tmp_addr
    in
        Z80EnvWithValue { z80env_1 | ctime = ctime } value

mem: Int -> Z80Env -> Z80EnvWithValue
mem base_addr z80env_ctime =
    let
       n = z80env_ctime.ram.cpu_time - z80env_ctime.ctime
       cont_value_0 = if n > 0 then
                         z80env_ctime |> cont n
                      else
                         0
       z80env = z80env_ctime |> add_cpu_time_env cont_value_0
       addr = base_addr - 0x4000
       (new_env, ctime, value) = if addr >= 0 then
                                   if addr < 0x4000 then
                                     let
                                        cont_value = z80env |> cont1 0
                                        new_z80 = z80env |> add_cpu_time_env cont_value
                                     in
                                        (new_z80, new_z80.ram.cpu_time + 3, z80env.ram |> getRamValue addr)
                                   else
                                     (z80env_ctime, c_NOCONT, z80env.ram |> getRamValue addr)
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
mem16: Int -> Z80Env -> Z80EnvWithValue
mem16 addr z80env_ctime =
    let
       n = z80env_ctime.ram.cpu_time - z80env_ctime.ctime
       cont0_value = if n > 0 then
                        z80env_ctime |> cont n
                     else
                        0
       z80env = z80env_ctime |> add_cpu_time_env cont0_value
       addr1 = addr - 0x3FFF
    in
       if and addr1 0x3FFF /= 0 then
         if addr1 < 0 then
           let
              low = getROMValue addr z80env.rom48k
              high = getROMValue (addr1 + 0x4000) z80env.rom48k
           in
              Z80EnvWithValue { z80env | ctime = c_NOCONT } (Bitwise.or low (shiftLeftBy8 high))
         else
           let
              value = getRam16Value (addr - 0x4000) z80env.ram
              z80env_1 = if addr1 < 0x4000 then
                            let
                                cont1_value = z80env |> cont1 0
                                z80env_2 = z80env |> add_cpu_time_env cont1_value
                                cont3_value = z80env_2 |> cont1 3
                            in
                                z80env_2 |> add_cpu_time_env (cont3_value + 6)
                         else
                            { z80env | ctime = c_NOCONT }
        in
            Z80EnvWithValue z80env_1 value
       else
         case (shiftRightBy 14 addr1) of
           0 ->
            let
                cont1_value = cont1 3 z80env
                low = z80env.rom48k |> getROMValue addr
                high = getRamValue 0 z80env.ram
                new_z80 = z80env |> add_cpu_time_env cont1_value
            in
                Z80EnvWithValue new_z80 (or low (shiftLeftBy8 high))
           1 ->
            let
                cont1_value = z80env |> cont1 0
                low = getRamValue (addr - 0x4000) z80env.ram
                high = getRamValue addr1 z80env.ram
                new_env = z80env |> add_cpu_time_env cont1_value
            in
                Z80EnvWithValue new_env (or low (shiftLeftBy8 high))
           2 ->
            let
                low = getRamValue (addr - 0x4000) z80env.ram
                high = getRamValue addr1 z80env.ram
            in
                Z80EnvWithValue { z80env | ctime = c_NOCONT } (or low (shiftLeftBy8 high))
           _ ->
            let
                low = getRamValue 0xBFFF z80env.ram
                high = getRamValue 0 z80env.ram
            in
                Z80EnvWithValue { z80env | ctime = c_NOCONT } (or low (shiftLeftBy8 high))
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

set_mem: Int -> Int -> Z80Env -> Z80Env
set_mem z80_addr value old_z80env =
   let
      n = old_z80env.ram.cpu_time - old_z80env.ctime
      z80env = if n > 0 then
                  let
                     cont0_value = old_z80env |> cont n
                  in
                     old_z80env |> add_cpu_time_env cont0_value
               else
                { old_z80env | ctime = c_NOCONT }
      addr = z80_addr - 0x4000
      (new_env, ctime) = if addr < 0x4000 then
                            if addr < 0 then
                               (z80env, c_NOCONT)
                            else
                               let
                                   cont1_value = z80env |> cont1 0
                                   z80env_1 = z80env |> add_cpu_time_env cont1_value
                                   new_time = z80env_1.ram.cpu_time + 3
                                   ram_value = getRamValue addr z80env_1.ram
                               in
                                  if ram_value == value then
                                     (z80env_1 , new_time)
                                  else
                                     ({ z80env_1 | ram = z80env.ram |> Z80Ram.setRamValue addr value }, new_time)
                         else
                            ({ z80env | ram = z80env.ram |> Z80Ram.setRamValue addr value }, c_NOCONT)
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
                       n = z80env.ram.cpu_time - z80env.ctime
                       cont0_value = if (n > 0) then
                                        cont n z80env
                                     else
                                        0
                       env = z80env |> add_cpu_time_env cont0_value
                       env_1 = { env | ctime = c_NOCONT }
                       nenv = if addr1 < 0 then
                                 env_1
                              else
                                 if addr1 >= 0x4000 then
                                    let
                                        ram = z80env.ram
                                           |> Z80Ram.setRamValue (addr - 1) (Bitwise.and value 0xFF)
                                           |> Z80Ram.setRamValue addr (shiftRightBy8 value)
                                    in
                                       { env_1 | ram = ram }
                                    --env_1
                                    --|> set_ram (addr - 1) (Bitwise.and value 0xFF)
                                    --|> set_ram addr (shiftRightBy8 value)
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
cont1: Int -> Z80Env -> Int
cont1 tmp_t z80  =
    let
        t = tmp_t + z80.ram.cpu_time
    in
        if (t<0) || (t>=c_SCRENDT) then
           0
        else
           if (Bitwise.and t 7) >= 6 then
               0
           else
               if (modBy 224 t) < 126 then
                   6 - (Bitwise.and t 7)
               else
                   0
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
contimpl: Int -> Int -> Int
contimpl tmp_n tmp_s =
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
            0
        else
            ntk.t + 6*n4

cont: Int -> Z80Env-> Int
cont tmp_n z80env =
    let
        tmp_t = z80env.ctime
    in
        if tmp_t + tmp_n <= 0 then
            0
        else
            let
                tmp_s = c_SCRENDT - tmp_t
            in
                if tmp_s < 0 then
                    0
                else
                    contimpl tmp_n tmp_s

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
      n = z80env.ram.cpu_time - z80env.ctime
      cont0_value = if n > 0 then
                z80env |> cont n
             else
                0
      env1 = z80env |> add_cpu_time_env cont0_value
      env2 = if and portn 0xC000 /= 0x4000 then
               let
                  cont_value = if and portn 0x0001 == 0 then
                                  env1 |> cont1 1
                               else
                                  0
                  env3 = env1 |> add_cpu_time_env cont_value
               in
                  { env3 | ctime = c_NOCONT }
             else
               let
                   env3 = { env1 | ctime = env1.ram.cpu_time }
                   contval = and portn 1 |> shiftLeftBy 1
                   c4 = env3 |> cont (2 + contval)
                   env4 = env3 |> add_cpu_time_env c4
               in
                  { env4 | ctime = env4.ram.cpu_time + 4 }
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
      value = env.keyboard |> z80_keyboard_input portnum
   in
      Z80EnvWithValue env value

add_cpu_time_env: Int -> Z80Env -> Z80Env
add_cpu_time_env value z80 =
       let
          ram = z80.ram |> add_cpu_time_ram value
       in
          { z80 | ram = ram }

reset_cpu_time: Z80Env -> Z80Env
reset_cpu_time z80env =
    let
        ram = z80env.ram
        ram2 = { ram | cpu_time = c_FRSTART }
    in
        { z80env | ram = ram2 }