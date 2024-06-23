--
-- $Id$
--
module Z80Env exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeWithValue, add_cpu_time_time, c_NOCONT, cont, cont1, cont_port)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import Utils exposing (shiftLeftBy8, shiftRightBy8, toHexString)
import Z80Ram exposing (Z80Ram, c_FRSTART, getRamValue)
import Z80Rom exposing (Z80ROM, getROMValue, set_spectrum_rom)


-- changing this to an array results in a recursion error in the browser :-(
type alias Z80Env =
    {
            rom48k: Z80ROM,
            ram: Z80Ram,
            keyboard: Keyboard,
            time: CpuTimeCTime,
            sp:  Int
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

z80env_constructor =
    Z80Env Z80Rom.constructor Z80Ram.constructor Keyboard.constructor (CpuTimeCTime c_FRSTART 0) 0

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
m1: Int -> Int -> Z80Env -> Z80EnvWithValue
m1 tmp_addr ir z80env =
    let
       n = z80env.time.cpu_time - z80env.time.ctime
       z80env_time = if n > 0 then
                  z80env.time |> cont n
                else
                  z80env.time
       addr = tmp_addr - 0x4000
       z80env_1_time = if ((and addr 0xC000) == 0) then
                       z80env_time |> cont1 0
                     else
                       z80env_time
       ctime = if and ir 0xC000 == 0x4000 then
                  z80env_1_time.cpu_time + 4
               else
                  c_NOCONT
       value = if addr >= 0 then
                  z80env.ram |> getRamValue addr
               else
                  -- not implementing IF1 switching for now
                  z80env.rom48k |> getROMValue tmp_addr
    in
        Z80EnvWithValue { z80env | time = CpuTimeCTime z80env_1_time.cpu_time ctime } value

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
mem: Int -> Z80Env -> CpuTimeWithValue
mem base_addr z80env =
    let
       n = z80env.time.cpu_time - z80env.time.ctime
       z80env_time = if n > 0 then
                        z80env.time |> cont n
                     else
                        z80env.time
       addr = base_addr - 0x4000
       (new_env, ctime, value) = if addr >= 0 then
                                    if addr < 0x4000 then
                                       let
                                          new_z80 = z80env_time |> cont1 0
                                       in
                                          (new_z80, new_z80.cpu_time + 3, z80env.ram |> getRamValue addr)
                                    else
                                       (z80env_time, c_NOCONT, z80env.ram |> getRamValue addr)
                                 else
                                    (z80env_time, c_NOCONT, z80env.rom48k |> getROMValue base_addr)
    in
        CpuTimeWithValue (CpuTimeCTime new_env.cpu_time ctime) value
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
mem16 addr z80env =
    let
       n = z80env.time.cpu_time - z80env.time.ctime
       z80env_time = if n > 0 then
                        z80env.time |> cont n
                     else
                        z80env.time
       addr1 = addr - 0x3FFF
    in
      if and addr1 0x3FFF /= 0 then
         if addr1 < 0 then
           let
              low = getROMValue addr z80env.rom48k
              high = getROMValue (addr1 + 0x4000) z80env.rom48k
           in
              Z80EnvWithValue { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT } (Bitwise.or low (shiftLeftBy8 high))
         else
           let
              low = getRamValue (addr - 0x4000) z80env.ram
              high = getRamValue addr1 z80env.ram
              z80env_1_time = if addr1 < 0x4000 then
                                 z80env_time |> cont1 0 |> cont1 3 |> add_cpu_time_time 6
                              else
                                 CpuTimeCTime z80env_time.cpu_time c_NOCONT
        in
            Z80EnvWithValue { z80env | time = z80env_1_time } (Bitwise.or low (shiftLeftBy8 high))
      else
       let
         addr1shift14 = shiftRightBy 14 addr1
       in
         if addr1shift14 == 0 then
            let
                new_z80_time = cont1 3 z80env_time
                low = z80env.rom48k |> getROMValue addr
                high = getRamValue 0 z80env.ram
            in
                Z80EnvWithValue { z80env | time = new_z80_time } (or low (shiftLeftBy8 high))
         else if addr1shift14 == 1 then
            let
                new_env_time = z80env_time |> cont1 0
                low = getRamValue (addr - 0x4000) z80env.ram
                high = getRamValue addr1 z80env.ram
            in
                Z80EnvWithValue { z80env | time = new_env_time } (or low (shiftLeftBy8 high))
         else if addr1shift14 == 2 then
            let
                low = getRamValue (addr - 0x4000) z80env.ram
                high = getRamValue addr1 z80env.ram
            in
                Z80EnvWithValue { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT } (or low (shiftLeftBy8 high))
         else
            let
                low = getRamValue 0xBFFF z80env.ram
                high = getRamValue 0 z80env.ram
            in
                Z80EnvWithValue { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT } (or low (shiftLeftBy8 high))
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
set_mem z80_addr value z80env =
   let
      n = z80env.time.cpu_time - z80env.time.ctime
      z80env_time = if n > 0 then
                       z80env.time |> cont n
                    else
                       CpuTimeCTime z80env.time.cpu_time c_NOCONT
      addr = z80_addr - 0x4000
      (new_env, ctime) = if addr < 0x4000 then
                            if addr < 0 then
                               (z80env, c_NOCONT)
                            else
                               let
                                   z80env_1_time = z80env_time |> cont1 0
                                   new_time = z80env_1_time.cpu_time + 3
                                   ram_value = getRamValue addr z80env.ram
                               in
                                  if ram_value == value then
                                     (z80env, new_time)
                                  else
                                     (z80env |> set_ram addr value, new_time)
                         else
                            (z80env |> set_ram addr value, c_NOCONT)
   in
      { new_env | time = CpuTimeCTime z80env_time.cpu_time ctime }
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
   in
      if (Bitwise.and addr1 0x3FFF) /= 0 then
         let
            n = z80env.time.cpu_time - z80env.time.ctime
            z80env_time = if (n > 0) then
                             cont n z80env.time
                          else
                             z80env.time
            env_1 = { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT }
         in
            if addr1 < 0 then
               env_1
            else
               if addr1 >= 0x4000 then
                  env_1 |> set_ram (addr - 1) (Bitwise.and value 0xFF)
                        |> set_ram addr (shiftRightBy8 value)
               else
                  env_1 |> set_mem addr (Bitwise.and value 0xFF)
                        |> set_mem (addr + 1) (shiftRightBy8 value)
      else
          z80env |> set_mem addr (Bitwise.and value 0xFF)
                 |> set_mem (addr + 1) (shiftRightBy8 value)

cont_port_env: Int -> Z80Env -> Z80Env
cont_port_env portn z80env =
    { z80env | time = z80env.time |> cont_port portn }

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
      env = env_in |> cont_port_env portnum
   in
      env

z80_in: Int -> Z80Env -> Z80EnvWithValue
z80_in portnum env_in =
   let
      env = env_in |> cont_port_env portnum
      --x = debug_log "z80_in" (portnum |> toHexString) Nothing
      value = env.keyboard |> z80_keyboard_input portnum
   in
      Z80EnvWithValue env value

add_cpu_time_env: Int -> Z80Env -> Z80Env
add_cpu_time_env value z80env =
   { z80env | time = z80env.time |> add_cpu_time_time value }

reset_cpu_time: Z80Env -> Z80Env
reset_cpu_time z80env =
   { z80env | time = CpuTimeCTime c_FRSTART z80env.time.ctime }

--public void push(int v) {
--	int sp;
--	time++;
--	env.mem((char)((sp=SP)-1), v>>>8);
--	time += 3;
--	env.mem(SP = (char)(sp-2), v&0xFF);
--	time += 3;
--}
push: Int -> Z80Env -> Z80Env
push v z80 =
   let
      --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
      sp_minus_1 = Bitwise.and (z80.sp - 1) 0xFFFF
      new_sp = Bitwise.and (z80.sp - 2) 0xFFFF
      env_2 = z80
             |> add_cpu_time_env 1
             |> set_mem sp_minus_1 (shiftRightBy8 v)
             |> add_cpu_time_env 3
             |> set_mem new_sp (Bitwise.and v 0xFF)
             |> add_cpu_time_env 3
   in
      { env_2 | sp = new_sp }

pop: Z80Env -> Z80EnvWithValue
pop z80 =
   let
      v = z80 |> mem16 z80.sp
      env = v.env |> add_cpu_time_env 6
   in
      Z80EnvWithValue { env | sp = z80.sp + 2 } v.value
