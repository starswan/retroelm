module CpuTimeCTime exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)


c_NOCONT =
    99999


c_SCRENDT =
    191 * 224 + 126


type alias CpuTimeCTime =
    { cpu_time : Int
    , ctime : Int
    }


type alias CpuTimeAndValue =
    { time : CpuTimeCTime
    , value : Int
    }


type alias CpuTimePcAndValue =
    { time : CpuTimeCTime
    , pc : Int
    , value : Int
    }


type alias CpuTimeSpAndValue =
    { time : CpuTimeCTime
    , sp : Int
    , value : Int
    }


type alias CpuTimeAndPc =
    { time : CpuTimeCTime
    , pc : Int
    }


type CpuTimeIncrement
    = CpuTimeIncrement Int


increment3 =
    CpuTimeIncrement 3


increment7 =
    CpuTimeIncrement 7


cpuTimeIncrement4 =
    CpuTimeIncrement 4


cpuTimeIncrement6 =
    CpuTimeIncrement 6


cpuTimeIncrement8 =
    CpuTimeIncrement 8


cpuTimeIncrement10 =
    CpuTimeIncrement 10


cpuTimeIncrement15 =
    CpuTimeIncrement 15


cpuTimeIncrement11 =
    CpuTimeIncrement 11



--increment2 =
--    CpuTimeIncrement 2
--increment0 =
--    CpuTimeIncrement 0
--	private final void cont1(int t) {
--		t += cpu.time;
--		if(t<0 || t>=SCRENDT) return;
--		if((t&7) >= 6) return;
--		if(t%224 < 126)
--			cpu.time += 6 - (t&7);
--	}


cont1 : Int -> CpuTimeCTime -> CpuTimeCTime
cont1 tmp_t z80 =
    let
        t =
            tmp_t + z80.cpu_time
    in
    if (t < 0) || (t >= c_SCRENDT) then
        z80

    else if Bitwise.and t 7 >= 6 then
        z80

    else if modBy 224 t < 126 then
        z80 |> addCpuTimeTime (6 - Bitwise.and t 7)

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


cont : Int -> CpuTimeCTime -> CpuTimeCTime
cont n z80env =
    let
        t =
            z80env.ctime
    in
    if t + n <= 0 then
        z80env

    else
        let
            s =
                c_SCRENDT - t
        in
        if s < 0 then
            z80env

        else
            let
                new_s =
                    s |> modBy 224

                maybe_ntk =
                    if new_s > 126 then
                        let
                            new_n =
                                n - (new_s - 126)
                        in
                        if new_n <= 0 then
                            Nothing

                        else
                            Just { n = new_n, t = 6, k = 15 }

                    else
                        let
                            k =
                                new_s |> shiftRightBy 3

                            s2 =
                                Bitwise.and new_s 0x07
                        in
                        if s2 == 7 then
                            let
                                n1 =
                                    n - 1
                            in
                            if n1 == 0 then
                                Nothing

                            else
                                Just { n = n1, t = s2 - 1, k = k }

                        else
                            Just { n = n, t = s2, k = k }
            in
            case maybe_ntk of
                Just ntk ->
                    let
                        n3 =
                            (ntk.n - 1) |> shiftRightBy 1

                        n4 =
                            if ntk.k < n3 then
                                ntk.k

                            else
                                n3
                    in
                    { z80env | cpu_time = z80env.cpu_time + (ntk.t + 6 * n4) }

                Nothing ->
                    z80env



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


cont_port : Int -> CpuTimeCTime -> CpuTimeCTime
cont_port portn z80env =
    let
        n =
            z80env.cpu_time - z80env.ctime

        env1_time =
            if n > 0 then
                z80env |> cont n

            else
                z80env

        env2 =
            if Bitwise.and portn 0xC000 /= 0x4000 then
                let
                    env3 =
                        if Bitwise.and portn 0x01 == 0 then
                            env1_time |> cont1 1

                        else
                            env1_time
                in
                { env3 | ctime = c_NOCONT }

            else
                let
                    env3 =
                        CpuTimeCTime env1_time.cpu_time env1_time.cpu_time

                    contval =
                        Bitwise.and portn 1 |> shiftLeftBy 1

                    env4 =
                        env3 |> cont (2 + contval)
                in
                env4 |> addCpuTimeTime 4
    in
    env2


addCpuTimeTime : Int -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTime value z80env =
    { z80env | cpu_time = z80env.cpu_time + value }


addCpuTimeTimeInc : CpuTimeIncrement -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTimeInc value z80env =
    case value of
        CpuTimeIncrement int ->
            { z80env | cpu_time = z80env.cpu_time + int }
