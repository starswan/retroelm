module SingleByteWithEnv exposing (..)

import Bitwise exposing (shiftRightBy)
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement(..), addCpuTimeTimeInc, cpuTimeIncrement4, increment2)
import Dict exposing (Dict)
import Z80Address exposing (Z80Address, decrement, incrementBy1)
import Z80Env exposing (Z80Env, c_TIME_LIMIT)
import Z80Types exposing (Z80)


type SingleByteEnvChange
    = NewSPValue Z80Address CpuTimeIncrement
    | AddToInterrupts Int CpuTimeIncrement


singleByteZ80Env : Dict Int (Z80Env -> SingleByteEnvChange)
singleByteZ80Env =
    Dict.fromList
        [ ( 0x33, inc_sp )
        , ( 0x3B, dec_sp )
        , ( 0x76, execute_0x76_halt )
        ]


applyEnvChangeDelta : CpuTimeCTime -> SingleByteEnvChange -> Z80 -> Z80
applyEnvChangeDelta cpu_time z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        new_pc =
            --Bitwise.and (z80.pc + 1) 0xFFFF
            z80.pc |> incrementBy1

        env =
            z80.env
    in
    case z80changeData of
        NewSPValue int time ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc time |> addCpuTimeTimeInc cpuTimeIncrement4, sp = int }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        AddToInterrupts int cpuTimeIncrement ->
            { z80
                | pc = new_pc
                , env = { env | time = cpu_time |> addCpuTimeTimeInc cpuTimeIncrement |> addCpuTimeTimeInc cpuTimeIncrement4 }
                , interrupts = { interrupts | halted = True, r = interrupts.r + int }
            }


inc_sp : Z80Env -> SingleByteEnvChange
inc_sp z80_env =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp + 1) 0xFFFF
    --in
    --NewSPValue (Bitwise.and (z80_env.sp + 1) 0xFFFF) increment2
    NewSPValue (z80_env.sp |> incrementBy1) increment2


dec_sp : Z80Env -> SingleByteEnvChange
dec_sp z80_env =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp - 1) 0xFFFF
    --in
    --NewSPValue (Bitwise.and (z80_env.sp - 1) 0xFFFF) increment2
    NewSPValue (z80_env.sp |> decrement) increment2


execute_0x76_halt : Z80Env -> SingleByteEnvChange
execute_0x76_halt z80_env =
    -- case 0x76: halt(); break;    --
    --	private void halt()
    --	{
    --		halted = true;
    --		int n = time_limit-time+3 >> 2;
    --		if(n>0) {
    --			n = env.halt(n, IR|R&0x7F);
    --			R+=n; time+=4*n;
    --		}
    --	}
    let
        --interrupts =
        --    z80.interrupts
        --n =
        --    shiftRightBy 2 (z80.time_limit - z80.env.time.cpu_time + 3)
        n =
            shiftRightBy 2 (c_TIME_LIMIT - z80_env.time.cpu_time + 3)
    in
    --( new_interrupts, time ) =
    if n > 0 then
        -- turns out env.halt(n, r) just returns n...?
        --{ z80 | interrupts = { interrupts | r = interrupts.r + n } } |> add_cpu_time (4 * n)
        --( { interrupts | r = interrupts.r + n }, z80.env.time |> addCpuTimeTime (4 * n) )
        AddToInterrupts n (CpuTimeIncrement (4 * n))

    else
        --( interrupts, z80.env.time )
        AddToInterrupts 0 (CpuTimeIncrement 0)



--z80
--in
--{ z80_1 | interrupts = { interrupts | halted = True } }
--InterruptsWithCpuTime { new_interrupts | halted = True } time
