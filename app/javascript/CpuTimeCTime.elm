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


increment2 =
    CpuTimeIncrement 2


increment0 =
    CpuTimeIncrement 0



--	private final void cont1(int t) {
--		t += cpu.time;
--		if(t<0 || t>=SCRENDT) return;
--		if((t&7) >= 6) return;
--		if(t%224 < 126)
--			cpu.time += 6 - (t&7);
--	}


cont1 : Int -> CpuTimeCTime -> Maybe CpuTimeCTime
cont1 tmp_t z80 =
    let
        t =
            tmp_t + z80.cpu_time
    in
    if (t < 0) || (t >= c_SCRENDT) then
        Nothing

    else if Bitwise.and t 7 >= 6 then
        Nothing

    else if modBy 224 t < 126 then
        Just (z80 |> addCpuTimeTime (6 - Bitwise.and t 7))

    else
        Nothing



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


cont : Int -> CpuTimeCTime -> Maybe CpuTimeCTime
cont n z80env =
    let
        t =
            z80env.ctime
    in
    if t + n <= 0 then
        Nothing

    else
        let
            s =
                c_SCRENDT - t
        in
        if s < 0 then
            Nothing

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
                            k2 =
                                new_s |> shiftRightBy 3

                            s2 =
                                Bitwise.and new_s 0x07

                            ( new_t, maybe_n2 ) =
                                if s2 == 7 then
                                    if n - 1 == 0 then
                                        ( s2 - 1, Nothing )

                                    else
                                        ( s2 - 1, Just (n - 1) )

                                else
                                    ( s2, Just n )
                        in
                        case maybe_n2 of
                            Just n2 ->
                                Just { n = n2, t = new_t, k = k2 }

                            Nothing ->
                                Nothing
            in
            case maybe_ntk of
                Just ntk ->
                    let
                        n3 =
                            shiftRightBy 1 (ntk.n - 1)

                        n4 =
                            if ntk.k < n3 then
                                ntk.k

                            else
                                n3
                    in
                    { z80env | cpu_time = z80env.cpu_time + (ntk.t + 6 * n4) }

                Nothing ->
                    Nothing

            else
                Just { z80env | cpu_time = z80env.cpu_time + (ntk.t + 6 * n4) }



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
                Nothing
    in
    if Bitwise.and portn 0xC000 /= 0x4000 then
        let
            env3 =
                if Bitwise.and portn 0x01 == 0 then
                    case env1_time of
                        Just env2_time ->
                            env2_time |> cont1 1

                        Nothing ->
                            Nothing
                    --env1_time |> cont1 1

                else
                    env1_time
        in
        case env3 of
            Just env3_time ->
                { env3_time | ctime = c_NOCONT }

            Nothing ->
                case env1_time of
                    Just env1_time_time ->
                        { env1_time_time | ctime = c_NOCONT }

                    Nothing ->
                        { z80env | ctime = c_NOCONT }

    else
        let
            env3 =
                case env1_time of
                    Just env1_time_time ->
                        { env1_time_time | ctime = env1_time_time.cpu_time }

                    Nothing ->
                        { z80env | ctime = z80env.cpu_time }

            contval =
                Bitwise.and portn 1 |> shiftLeftBy 1

            env4 =
                env3 |> cont (2 + contval)
        in
        case env4 of
            Just env4_time ->
                env4_time |> addCpuTimeTime 4

            Nothing ->
                env3 |> addCpuTimeTime 4


addCpuTimeTime : Int -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTime value z80env =
    { z80env | cpu_time = z80env.cpu_time + value }


addCpuTimeTimeInc : CpuTimeIncrement -> CpuTimeCTime -> CpuTimeCTime
addCpuTimeTimeInc value z80env =
    case value of
        CpuTimeIncrement int ->
            { z80env | cpu_time = z80env.cpu_time + int }
