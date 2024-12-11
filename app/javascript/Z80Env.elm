--
-- $Id$
--


module Z80Env exposing (..)

import Bitwise exposing (or)
import CpuTimeCTime exposing (CpuTimeAndAddress, CpuTimeAndValue, CpuTimeCTime, CpuTimeIncrement, CpuTimeSpAndValue, addCpuTimeTime, addCpuTimeTimeInc, c_NOCONT, cont, cont1, cont_port)
import Keyboard exposing (Keyboard, z80_keyboard_input)
import ScreenStorage exposing (getScreenValue)
import Utils exposing (shiftLeftBy8, shiftRightBy8, toHexString2)
import Z80Address exposing (Z80Address(..), decrement, decrement2, fromInt, incrementBy1, incrementBy2, toInt)
import Z80Debug exposing (debugLog)
import Z80Memory exposing (getMemValue, setMemValue)
import Z80Ram exposing (Z80Ram)
import Z80Rom exposing (Z80ROM, getROMValue)


c_FRSTART =
    -14335


c_FRTIME =
    69888


c_TIME_LIMIT =
    c_FRSTART + c_FRTIME



-- changing this to an array results in a recursion error in the browser :-(


type alias Z80Env =
    { --rom48k : Z80ROM
      ram : Z80Ram
    , keyboard : Keyboard
    , time : CpuTimeCTime
    , sp : Z80Address
    }


type alias Z80EnvWithValue =
    { env : Z80Env
    , value : Int
    }


type alias Z80EnvWithPC =
    { env : Z80Env
    , pc : Z80Address
    }


type alias ValueWithTime =
    { value : Int
    , cpu_time : Int
    }


z80env_constructor =
    Z80Env Z80Ram.constructor Keyboard.constructor (CpuTimeCTime c_FRSTART 0) (0 |> fromInt)



--set_rom : Array Int -> Z80Env -> Z80Env
--set_rom romdata z80env =
--    let
--        rommy =
--            make_spectrum_rom romdata
--    in
--    { z80env | rom48k = rommy }
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


m1 : Z80Address -> Int -> Z80ROM -> Z80Env -> CpuTimeAndValue
m1 z80_addr ir rom48k z80env =
    let
        n =
            z80env.time.cpu_time - z80env.time.ctime

        z80env_time =
            if n > 0 then
                z80env.time |> cont n

            else
                z80env.time

        ( value, z80env_1_time ) =
            case z80_addr of
                --Z80Address addr addressType ->
                --    case addressType of
                ROM addr ->
                    -- not implementing IF1 switching for now
                    ( rom48k |> getROMValue addr, z80env_time )

                Screen addr ->
                    ( z80env.ram.screen |> getScreenValue addr, z80env_time |> cont1 0 )

                Lomem addr ->
                    ( z80env.ram.non_screen |> getMemValue addr, z80env_time |> cont1 0 )

                HiMem addr ->
                    ( z80env.ram.non_screen |> getMemValue addr, z80env_time )

        ctime =
            if Bitwise.and ir 0xC000 == 0x4000 then
                z80env_1_time.cpu_time + 4

            else
                c_NOCONT
    in
    CpuTimeAndValue { z80env_1_time | ctime = ctime } value



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


mem : Z80Address -> CpuTimeCTime -> Z80ROM -> Z80Ram -> CpuTimeAndValue
mem z80_addr time rom48k ram =
    let
        n =
            time.cpu_time - time.ctime

        z80env_time =
            if n > 0 then
                time |> cont n

            else
                time

        --base_addr =
        --    z80_addr |> toInt
        --addr =
        --    base_addr - 0x4000
        ( new_time, ctime, value ) =
            case z80_addr of
                --Z80Address addr_value addressType ->
                --    case addressType of
                ROM addr_value ->
                    ( z80env_time, c_NOCONT, rom48k |> getROMValue addr_value )

                Screen addr_value ->
                    let
                        new_z80 =
                            z80env_time |> cont1 0
                    in
                    ( new_z80, new_z80.cpu_time + 3, ram.screen |> getScreenValue addr_value )

                Lomem addr_value ->
                    let
                        new_z80 =
                            z80env_time |> cont1 0
                    in
                    ( new_z80, new_z80.cpu_time + 3, ram.non_screen |> getMemValue addr_value )

                HiMem addr_value ->
                    ( z80env_time, c_NOCONT, ram.non_screen |> getMemValue addr_value )

        --if addr >= 0 then
        --    if addr < 0x4000 then
        --        let
        --            new_z80 =
        --                z80env_time |> cont1 0
        --        in
        --        ( new_z80, new_z80.cpu_time + 3, ram |> getRamValue addr )
        --
        --    else
        --        ( z80env_time, c_NOCONT, ram |> getRamValue addr )
        --
        --else
        --    ( z80env_time, c_NOCONT, rom48k |> getROMValue base_addr )
    in
    CpuTimeAndValue { new_time | ctime = ctime } value



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


mem16 : Z80Address -> Z80ROM -> Z80Env -> CpuTimeAndAddress
mem16 z80_addr rom48k z80env =
    let
        n =
            z80env.time.cpu_time - z80env.time.ctime

        z80env_time =
            if n > 0 then
                z80env.time |> cont n

            else
                z80env.time

        --addr1 =
        --    addr - 0x3FFF
        addrNext =
            z80_addr |> incrementBy1
    in
    case z80_addr of
        --Z80Address loAddr loAddrType ->
        --    case loAddrType of
        ROM loAddr ->
            case addrNext of
                --Z80Address hiAddr hiAddrType ->
                --    case hiAddrType of
                ROM hiAddr ->
                    let
                        low =
                            getROMValue loAddr rom48k

                        high =
                            getROMValue hiAddr rom48k
                    in
                    CpuTimeAndAddress { z80env_time | ctime = c_NOCONT } (Bitwise.or low (shiftLeftBy8 high) |> fromInt)

                _ ->
                    let
                        new_z80_time =
                            cont1 3 z80env_time

                        low =
                            rom48k |> getROMValue loAddr

                        high =
                            getScreenValue 0 z80env.ram.screen
                    in
                    CpuTimeAndAddress new_z80_time (or low (shiftLeftBy8 high) |> fromInt)

        Screen loAddr ->
            case addrNext of
                --Z80Address hiAddr hiAddrType ->
                --    case hiAddrType of
                Screen hiAddr ->
                    let
                        new_env_time =
                            z80env_time |> cont1 0

                        low =
                            getScreenValue loAddr z80env.ram.screen

                        high =
                            getScreenValue hiAddr z80env.ram.screen
                    in
                    CpuTimeAndAddress new_env_time (or low (shiftLeftBy8 high) |> fromInt)

                Lomem hiAddr ->
                    let
                        new_env_time =
                            z80env_time |> cont1 0

                        low =
                            getScreenValue loAddr z80env.ram.screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress new_env_time (or low (shiftLeftBy8 high) |> fromInt)

                --    These last 2 can't actuallt happen
                HiMem hiAddr ->
                    let
                        new_env_time =
                            z80env_time |> cont1 0

                        low =
                            getScreenValue loAddr z80env.ram.screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress new_env_time (or low (shiftLeftBy8 high) |> fromInt)

                ROM hiAddr ->
                    let
                        new_env_time =
                            z80env_time |> cont1 0

                        low =
                            getScreenValue loAddr z80env.ram.screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress new_env_time (or low (shiftLeftBy8 high) |> fromInt)

        Lomem loAddr ->
            case addrNext of
                --Z80Address hiAddr hiAddrType ->
                --    case hiAddrType of
                Lomem hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen

                        z80env_1_time =
                            z80env_time |> cont1 0 |> cont1 3 |> addCpuTimeTime 6
                    in
                    CpuTimeAndAddress z80env_1_time (Bitwise.or low (shiftLeftBy8 high) |> fromInt)

                HiMem hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen

                        z80env_1_time =
                            --CpuTimeCTime z80env_time.cpu_time c_NOCONT
                            { z80env_time | ctime = c_NOCONT }
                    in
                    CpuTimeAndAddress z80env_1_time (Bitwise.or low (shiftLeftBy8 high) |> fromInt)

                --    last 2 can't happen
                ROM hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen

                        z80env_1_time =
                            --CpuTimeCTime z80env_time.cpu_time c_NOCONT
                            { z80env_time | ctime = c_NOCONT }
                    in
                    CpuTimeAndAddress z80env_1_time (Bitwise.or low (shiftLeftBy8 high) |> fromInt)

                Screen hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen

                        z80env_1_time =
                            --CpuTimeCTime z80env_time.cpu_time c_NOCONT
                            { z80env_time | ctime = c_NOCONT }
                    in
                    CpuTimeAndAddress z80env_1_time (Bitwise.or low (shiftLeftBy8 high) |> fromInt)

        HiMem loAddr ->
            case addrNext of
                --Z80Address hiAddr hiAddrType ->
                --    case hiAddrType of
                ROM _ ->
                    let
                        low =
                            getMemValue (0xBFFF - 6912) z80env.ram.non_screen

                        high =
                            getROMValue 0 rom48k
                    in
                    CpuTimeAndAddress { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high) |> fromInt)

                HiMem hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high) |> fromInt)

                Lomem hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high) |> fromInt)

                Screen hiAddr ->
                    let
                        low =
                            getMemValue loAddr z80env.ram.non_screen

                        high =
                            getMemValue hiAddr z80env.ram.non_screen
                    in
                    CpuTimeAndAddress { z80env_time | ctime = c_NOCONT } (or low (shiftLeftBy8 high) |> fromInt)



--if and addr1 0x3FFF /= 0 then
--    if addr1 < 0 then
--        let  ROMROM
--            low =
--                getROMValue addr rom48k
--
--            high =
--                getROMValue (addr1 + 0x4000) rom48k
--        in
--        CpuTimeAndValue (CpuTimeCTime z80env_time.cpu_time c_NOCONT) (Bitwise.or low (shiftLeftBy8 high))
--
--    else
--        let LOMEM LOMEM
--            low =
--                getRamValue (addr - 0x4000) z80env.ram
--
--            high =
--                getRamValue addr1 z80env.ram
--
--            z80env_1_time =
--                if addr1 < 0x4000 then
--                    z80env_time |> cont1 0 |> cont1 3 |> addCpuTimeTime 6
--
--                else
--                    CpuTimeCTime z80env_time.cpu_time c_NOCONT
--        in
--        CpuTimeAndValue z80env_1_time (Bitwise.or low (shiftLeftBy8 high))
--
--else
--    let
--        addr1shift14 =
--            shiftRightBy 14 addr1
--    in
--    if addr1shift14 == 0 then
--        let ROMRAMZERO
--            new_z80_time =
--                cont1 3 z80env_time
--
--            low =
--                rom48k |> getROMValue addr
--
--            high =
--                getRamValue 0 z80env.ram
--        in
--        CpuTimeAndValue new_z80_time (or low (shiftLeftBy8 high))
--
--    else if addr1shift14 == 1 then
--        let LOMEM RAM
--            new_env_time =
--                z80env_time |> cont1 0
--
--            low =
--                getRamValue (addr - 0x4000) z80env.ram
--
--            high =
--                getRamValue addr1 z80env.ram
--        in
--        CpuTimeAndValue new_env_time (or low (shiftLeftBy8 high))
--
--    else if addr1shift14 == 2 then
--        let HIMEM RAM
--            low =
--                getRamValue (addr - 0x4000) z80env.ram
--
--            high =
--                getRamValue addr1 z80env.ram
--        in
--        CpuTimeAndValue (CpuTimeCTime z80env_time.cpu_time c_NOCONT) (or low (shiftLeftBy8 high))
--
--    else
--        let
--            low =
--                getRamValue 0xBFFF z80env.ram
--
--            high =
--                getROMValue 0 rom48k
--        in
--        CpuTimeAndValue (CpuTimeCTime z80env_time.cpu_time c_NOCONT) (or low (shiftLeftBy8 high))
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


setRam : Int -> Int -> Z80Env -> Z80Env
setRam addr value z80env =
    --let
    --ram_value = getValue addr z80env.ram
    --n = if addr == 0x1CB6 || addr == 0x1CB7 then
    --       debug_log "Alert!" ("setting " ++ (addr |> toHexString) ++ " from " ++ (ram_value |> toHexString2) ++ " to " ++ (value |> toHexString2)) Nothing
    --    else
    --       Nothing
    --in
    { z80env | ram = z80env.ram |> Z80Ram.setRamValue addr value }


setMem : Z80Address -> Int -> Z80Env -> Z80Env
setMem in_addr value z80env =
    let
        z80_addr =
            in_addr |> toInt

        z80_time =
            z80env.time

        n =
            z80_time.cpu_time - z80_time.ctime

        z80env_time =
            if n > 0 then
                z80_time |> cont n

            else
                { z80_time | ctime = c_NOCONT }

        addr =
            z80_addr - 0x4000

        ( new_env, ctime ) =
            case in_addr of
                --Z80Address addr_value addressType ->
                --    case addressType of
                ROM _ ->
                    ( z80env, c_NOCONT )

                HiMem addr_value ->
                    let
                        z80ram = z80env.ram
                        --ram =  z80env.ram |> Z80Ram.setRamValue addr value
                        --{ z80ram | non_screen = z80ram.non_screen |> setMemValue (addr - 6912) value }
                        ram = { z80ram | non_screen = z80ram.non_screen |> setMemValue addr_value value }
                    in
                    ( { z80env | ram = ram }, c_NOCONT )

                Screen addr_value ->
                    let
                        z80env_1_time =
                            z80env_time |> cont1 0

                        new_time =
                            z80env_1_time.cpu_time + 3

                        ram_value =
                            getScreenValue addr_value z80env.ram.screen
                    in
                    if ram_value == value then
                        ( z80env, new_time )

                    else
                        ( z80env |> setRam addr value, new_time )

                Lomem addr_value ->
                    let
                        z80env_1_time =
                            z80env_time |> cont1 0

                        new_time =
                            z80env_1_time.cpu_time + 3

                        ram_value =
                            getMemValue addr_value z80env.ram.non_screen
                    in
                    if ram_value == value then
                        ( z80env, new_time )

                    else
                        ( z80env |> setRam addr value, new_time )
    in
    { new_env | time = { z80env_time | ctime = ctime } }



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


setMem16 : Z80Address -> Int -> Z80Env -> Z80Env
setMem16 z80_addr value z80env =
    let
        addr =
            z80_addr |> toInt

        addr1 =
            addr - 0x3FFF
    in
    if Bitwise.and addr1 0x3FFF /= 0 then
        let
            n =
                z80env.time.cpu_time - z80env.time.ctime

            z80env_time =
                if n > 0 then
                    cont n z80env.time

                else
                    z80env.time

            env_1 =
                { z80env | time = CpuTimeCTime z80env_time.cpu_time c_NOCONT }
        in
        if addr1 < 0 then
            env_1

        else if addr1 >= 0x4000 then
            env_1
                |> setRam (addr1 - 1) (Bitwise.and value 0xFF)
                |> setRam addr1 (shiftRightBy8 value)

        else
            env_1
                |> setMem z80_addr (Bitwise.and value 0xFF)
                |> setMem (z80_addr |> incrementBy1) (shiftRightBy8 value)

    else
        z80env
            |> setMem z80_addr (Bitwise.and value 0xFF)
            |> setMem (z80_addr |> incrementBy1) (shiftRightBy8 value)


contPortEnv : Int -> Z80Env -> Z80Env
contPortEnv portn z80env =
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


out : Int -> Int -> Z80Env -> Z80Env
out portnum _ env_in =
    let
        env =
            env_in |> contPortEnv portnum
    in
    env


z80_in : Int -> Z80Env -> CpuTimeAndValue
z80_in portnum env_in =
    let
        env =
            env_in.time |> cont_port portnum

        --x = debug_log "z80_in" (portnum |> toHexString) Nothing
        value =
            env_in.keyboard |> z80_keyboard_input portnum

        x =
            if value /= 0xFF then
                debugLog "keyboard value" ((portnum |> toHexString2) ++ " " ++ toHexString2 value) value

            else
                value
    in
    CpuTimeAndValue env x


addCpuTimeEnv : Int -> Z80Env -> Z80Env
addCpuTimeEnv value z80env =
    { z80env | time = z80env.time |> addCpuTimeTime value }


addCpuTimeEnvInc : CpuTimeIncrement -> Z80Env -> Z80Env
addCpuTimeEnvInc value z80env =
    { z80env | time = z80env.time |> addCpuTimeTimeInc value }


reset_cpu_time : Z80Env -> Z80Env
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


z80_push : Z80Address -> Z80Env -> Z80Env
z80_push v z80env =
    let
        --a = debug_log "push" ((v |> toHexString) ++ " onto " ++ (z80.sp |> toHexString)) Nothing
        sp_minus_1 =
            --Bitwise.and (z80env.sp - 1) 0xFFFF
            z80env.sp |> decrement

        new_sp =
            --Bitwise.and (z80env.sp - 2) 0xFFFF
            z80env.sp |> decrement2

        value =
            v |> toInt

        env_2 =
            z80env
                |> addCpuTimeEnv 1
                |> setMem sp_minus_1 (shiftRightBy8 value)
                --|> setMem sp_minus_1 (top8Bits v)
                |> addCpuTimeEnv 3
                |> setMem new_sp (Bitwise.and value 0xFF)
                --|> setMem new_sp (lower8Bits v)
                |> addCpuTimeEnv 3
    in
    { env_2 | sp = new_sp }


z80_pop : Z80ROM -> Z80Env -> CpuTimeSpAndValue
z80_pop z80rom z80_env =
    let
        v =
            z80_env |> mem16 z80_env.sp z80rom

        time =
            v.time |> addCpuTimeTime 6
    in
    --CpuTimeSpAndValue time (Bitwise.and (z80_env.sp + 2) 0xFFFF) v.value
    CpuTimeSpAndValue time (z80_env.sp |> incrementBy2) v.address