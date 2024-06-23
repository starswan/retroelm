module CpuTimeCTime exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
c_NOCONT = 99999
c_SCRENDT = 191*224+126

type alias CpuTimeCTime =
    {
            cpu_time: Int,
            ctime: Int
    }

type alias CpuTimeAndValue =
    {
        time: CpuTimeCTime,
        value: Int
    }

type alias CpuTimePcAndValue =
   {
        time: CpuTimeCTime,
        pc: Int,
        value: Int
   }

type alias CpuTimeAndPc =
   {
        time: CpuTimeCTime,
        pc: Int
   }

--	private final void cont1(int t) {
--		t += cpu.time;
--		if(t<0 || t>=SCRENDT) return;
--		if((t&7) >= 6) return;
--		if(t%224 < 126)
--			cpu.time += 6 - (t&7);
--	}
cont1: Int -> CpuTimeCTime -> CpuTimeCTime
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
                  z80 |> add_cpu_time_time (6 - (Bitwise.and t 7))
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
cont_port: Int -> CpuTimeCTime -> CpuTimeCTime
cont_port portn z80env =
   let
      n = z80env.cpu_time - z80env.ctime
      env1_time = if n > 0 then
                     z80env |> cont n
                  else
                     z80env
      env2 = if Bitwise.and portn 0xC000 /= 0x4000 then
               let
                  env3 = if Bitwise.and portn 0x0001 == 0 then
                            env1_time |> cont1 1
                         else
                            env1_time
               in
                  { env3 | ctime = c_NOCONT }
             else
               let
                   env3 = CpuTimeCTime env1_time.cpu_time env1_time.cpu_time
                   contval = Bitwise.and portn 1 |> shiftLeftBy 1
                   env4 = env3 |> cont (2 + contval)
               in
                   env4 |> add_cpu_time_time 4
   in
      env2

add_cpu_time_time: Int -> CpuTimeCTime -> CpuTimeCTime
add_cpu_time_time value z80env =
   { z80env | cpu_time = z80env.cpu_time + value }




