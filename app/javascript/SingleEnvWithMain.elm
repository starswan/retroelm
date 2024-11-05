module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (shiftLeftBy8)
import Z80Env exposing (Z80Env, mem)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters)


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime


singleEnvMainRegs : Dict Int (MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange)
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ld_a_indirect_bc )
        , ( 0x1A, ld_a_indirect_de )
        ]


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c

        new_a =
            mem v z80_env.time rom48k z80_env.ram
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    --CpuTimeWithFlags (new_a.time |> addCpuTimeTime 3) new_flags
    SingleEnvNewARegister new_a.value (new_a.time |> addCpuTimeTime 3)


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e

        new_a =
            mem addr z80_env.time rom48k z80_env.ram
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    --CpuTimeWithFlags env_1 new_flags
    SingleEnvNewARegister new_a.value (new_a.time |> addCpuTimeTime 3)
