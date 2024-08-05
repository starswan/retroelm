module Z80Types exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndPc, CpuTimeCTime, CpuTimePcAndValue, add_cpu_time_time)
import Utils exposing (byte, char, shiftLeftBy8, shiftRightBy8)
import Z80Env exposing (Z80Env, Z80EnvWithPC, add_cpu_time_env, mem, mem16, z80_push)
import Z80Flags exposing (FlagRegisters)
import Z80Rom exposing (Z80ROM)


type alias MainRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    }


type alias MainWithIndexRegisters =
    { b : Int
    , c : Int
    , d : Int
    , e : Int
    , hl : Int
    , ix : Int
    , iy : Int
    }


type alias InterruptRegisters =
    { ir : Int
    , r : Int
    , --mp:  Int, -- /* MEMPTR, the hidden register emulated according to memptr_eng.txt */
      iff : Int
    , iM : Int
    , halted : Bool
    }


type alias Z80 =
    { env : Z80Env
    , pc : Int
    , main : MainWithIndexRegisters
    , flags : FlagRegisters
    , alt_main : MainRegisters
    , alt_flags : FlagRegisters
    , interrupts : InterruptRegisters
    , time_limit : Int
    }


type alias EnvWithPCAndValue =
    { env : Z80Env
    , pc : Int
    , value : Int
    }


type alias IntWithFlagsTimeAndPC =
    { value : Int
    , flags : FlagRegisters
    , time : CpuTimeCTime
    , pc : Int
    }



-- Think this could be a useful parameter to execute_lt40 to avoid the duplication
-- problem currently being experienced in function group_xy


type IXIYHL
    = IX
    | IY
    | HL


type IXIY
    = IXIY_IX
    | IXIY_IY



--
--	private int imm8()
--	{
--		int v = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 3;
--		return v;
--	}


imm8 : Z80ROM -> Z80 -> CpuTimePcAndValue
imm8 rom48k z80 =
    let
        v =
            z80.env |> mem z80.pc rom48k

        new_pc =
            Bitwise.and (z80.pc + 1) 0xFFFF

        env_1 =
            v.time |> add_cpu_time_time 3
    in
    CpuTimePcAndValue env_1 new_pc v.value



-- would need the side-effect of mem call as well
--imm8_discard: Z80 -> Z80
--imm8_discard z80 =
--    z80 |> inc_pc |> add_cpu_time 3
--	private int imm16()
--	{
--		int v = env.mem16(PC);
--		PC = (char)(PC+2);
--		time += 6;
--		return v;
--	}


imm16 : Z80ROM -> Z80 -> CpuTimePcAndValue
imm16 rom48k z80 =
    let
        v =
            z80.env |> mem16 z80.pc rom48k

        pc =
            Bitwise.and (z80.pc + 2) 0xFFFF

        env =
            v.time |> add_cpu_time_time 6
    in
    CpuTimePcAndValue env pc v.value



--	private void jp(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) PC = a;
--	}


jp_z80 : Bool -> Z80ROM -> Z80 -> Z80
jp_z80 y rom48k z80 =
    let
        a =
            z80 |> imm16 rom48k

        env =
            z80.env

        z80_1 =
            { z80 | pc = a.pc, env = { env | time = a.time } }
    in
    if y then
        { z80_1 | pc = a.value }

    else
        z80_1


jp : Bool -> Z80ROM -> Z80 -> CpuTimeAndPc
jp y rom48k z80 =
    let
        a =
            z80 |> imm16 rom48k
    in
    if y then
        CpuTimeAndPc a.time a.value

    else
        CpuTimeAndPc a.time a.pc



--	private void call(boolean y)
--	{
--		int a = MP = imm16();
--		if(y) {push(PC); PC = a;}
--	}


call_if : Bool -> Z80ROM -> Z80 -> Z80EnvWithPC
call_if y rom48k z80 =
    let
        a =
            z80 |> imm16 rom48k

        env =
            z80.env

        z80_2 =
            { z80 | pc = a.pc, env = { env | time = a.time } }
    in
    if y then
        let
            --b = debug_log "call" (a.value |> subName) Nothing
            --z80_1 = z80_2 |> push z80_2.pc |> set_pc a.value
            pushed =
                z80_2.env |> z80_push z80_2.pc

            --z80_1 = { z80_2 | env = pushed, pc = a.value }
        in
        Z80EnvWithPC pushed a.value

    else
        Z80EnvWithPC z80_2.env a.pc


rst_z80 : Int -> Z80 -> Z80
rst_z80 c z80 =
    --z80 |> push z80.pc |> set_pc (c - 199)
    let
        pushed =
            z80.env |> z80_push z80.pc
    in
    { z80 | env = pushed, pc = c - 199 }


rst : Int -> Z80 -> Z80EnvWithPC
rst c z80 =
    --z80 |> push z80.pc |> set_pc (c - 199)
    let
        pushed =
            z80.env |> z80_push z80.pc
    in
    Z80EnvWithPC pushed (c - 199)


a_with_z80 : Z80 -> CpuTimePcAndValue
a_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.flags.a


add_cpu_time : Int -> Z80 -> Z80
add_cpu_time value z80 =
    let
        env =
            z80.env |> add_cpu_time_env value
    in
    { z80 | env = env }


b_with_z80 : Z80 -> CpuTimePcAndValue
b_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.b


c_with_z80 : Z80 -> CpuTimePcAndValue
c_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.c


d_with_z80 : Z80 -> CpuTimePcAndValue
d_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.d


e_with_z80 : Z80 -> CpuTimePcAndValue
e_with_z80 z80 =
    CpuTimePcAndValue z80.env.time z80.pc z80.main.e


get_ixiy_xy : IXIY -> MainWithIndexRegisters -> Int
get_ixiy_xy ixiy z80_main =
    case ixiy of
        IXIY_IX ->
            z80_main.ix

        IXIY_IY ->
            z80_main.iy


h_with_z80 : IXIYHL -> Z80 -> CpuTimePcAndValue
h_with_z80 ixiyhl z80 =
    CpuTimePcAndValue z80.env.time z80.pc (shiftRightBy8 (get_xy ixiyhl z80.main))


hl_deref_with_z80 : IXIYHL -> Z80ROM -> Z80 -> CpuTimePcAndValue
hl_deref_with_z80 ixiyhl rom48k z80 =
    let
        a =
            z80 |> env_mem_hl ixiyhl rom48k

        new_b =
            z80.env |> mem a.value rom48k
    in
    CpuTimePcAndValue new_b.time a.pc new_b.value


inc_pc : Z80 -> Int
inc_pc z80 =
    Bitwise.and (z80.pc + 1) 0xFFFF


inc_pc2 : Z80 -> Int
inc_pc2 z80 =
    Bitwise.and (z80.pc + 2) 0xFFFF


l_with_z80 : IXIYHL -> Z80 -> CpuTimePcAndValue
l_with_z80 ixiyhl z80 =
    CpuTimePcAndValue z80.env.time z80.pc (Bitwise.and (get_xy ixiyhl z80.main) 0xFF)


set_flag_regs : FlagRegisters -> Z80 -> Z80
set_flag_regs flags z80 =
    { z80 | flags = flags }


set_h : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_h value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    set_xy (Bitwise.or (Bitwise.and xy 0xFF) (shiftLeftBy8 value)) ixiyhl z80


set_l : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_l value ixiyhl z80 =
    let
        xy =
            get_xy ixiyhl z80
    in
    set_xy (Bitwise.or (Bitwise.and xy 0xFF00) value) ixiyhl z80


get_xy : IXIYHL -> MainWithIndexRegisters -> Int
get_xy ixiyhl z80_main =
    case ixiyhl of
        IX ->
            z80_main.ix

        IY ->
            z80_main.iy

        HL ->
            z80_main.hl


set_xy : Int -> IXIYHL -> MainWithIndexRegisters -> MainWithIndexRegisters
set_xy value ixiyhl z80 =
    case ixiyhl of
        IX ->
            { z80 | ix = value }

        IY ->
            { z80 | iy = value }

        HL ->
            { z80 | hl = value }



--
--	private int getd(int xy)
--	{
--		int d = env.mem(PC);
--		PC = (char)(PC+1);
--		time += 8;
--		return MP = (char)(xy + (byte)d);
--	}
--getd_value: Int -> Z80 -> CpuTimePcAndValue
--getd_value xy z80 =
--   let
--      d = z80.env |> mem z80.pc
--   in
--      CpuTimePcAndValue (d.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (xy + byte d.value))


env_mem_hl : IXIYHL -> Z80ROM -> Z80 -> CpuTimePcAndValue
env_mem_hl ixiyhl rom48k z80 =
    case ixiyhl of
        HL ->
            CpuTimePcAndValue z80.env.time z80.pc z80.main.hl

        IX ->
            let
                dval =
                    z80.env |> mem z80.pc rom48k
            in
            CpuTimePcAndValue (dval.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (z80.main.ix + byte dval.value))

        IY ->
            let
                dval =
                    z80.env |> mem z80.pc rom48k
            in
            CpuTimePcAndValue (dval.time |> add_cpu_time_time 8) (char (z80.pc + 1)) (char (z80.main.iy + byte dval.value))


get_bc : Z80 -> Int
get_bc z80 =
    z80.main.b |> shiftLeftBy8 |> Bitwise.or z80.main.c


get_de : Z80 -> Int
get_de z80 =
    z80.main.d |> shiftLeftBy8 |> Bitwise.or z80.main.e


dec_pc2 : Z80 -> Z80
dec_pc2 z80 =
    { z80 | pc = Bitwise.and (z80.pc - 2) 0xFFFF }



--	void bc(int v) {C=v&0xFF; B=v>>>8;}


set_bc : Int -> Z80 -> Z80
set_bc v z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | b = shiftRightBy8 v, c = Bitwise.and v 0xFF } }



--	void de(int v) {E=v&0xFF; D=v>>>8;}


set_de : Int -> Z80 -> Z80
set_de v z80 =
    let
        z80_main =
            z80.main
    in
    { z80 | main = { z80_main | d = shiftRightBy8 v, e = Bitwise.and v 0xFF } }


set_bc_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_bc_main v z80_main =
    { z80_main | b = shiftRightBy8 v, c = Bitwise.and v 0xFF }


set_de_main : Int -> MainWithIndexRegisters -> MainWithIndexRegisters
set_de_main v z80_main =
    { z80_main | d = shiftRightBy8 v, e = Bitwise.and v 0xFF }



--	private void jr()
--	{
--		int pc = PC;
--		byte d = (byte)env.mem(pc); time += 8;
--		MP = PC = (char)(pc+d+1);
--	}


jr : Z80ROM -> Z80 -> CpuTimeAndPc
jr rom48k z80 =
    let
        mempc =
            z80.env |> mem z80.pc rom48k

        d =
            byte mempc.value

        --x = Debug.log "jr" ((String.fromInt d.value) ++ " " ++ (String.fromInt (byte d.value)))
    in
    --z80 |> set_env mempc.env |> add_cpu_time 8 |> set_pc (z80.pc + d + 1)
    CpuTimeAndPc (mempc.time |> add_cpu_time_time 8) (Bitwise.and (z80.pc + d + 1) 0xFFFF)

get_h: IXIYHL -> MainWithIndexRegisters -> Int
get_h ixiyhl z80 =
    shiftRightBy8 (get_xy ixiyhl z80)

get_l: IXIYHL -> MainWithIndexRegisters -> Int
get_l ixiyhl z80 =
    Bitwise.and (get_xy ixiyhl z80) 0xFF

