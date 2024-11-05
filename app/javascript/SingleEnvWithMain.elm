module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime, cpuTimeIncrement4)
import Dict exposing (Dict)
import Utils exposing (shiftLeftBy8)
import Z80Env exposing (Z80Env, addCpuTimeEnvInc, mem)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnvNewBRegister Int CpuTimeCTime
    | SingleEnvNewCRegister Int CpuTimeCTime


singleEnvMainRegs : Dict Int (MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange)
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ld_a_indirect_bc )
        , ( 0x1A, ld_a_indirect_de )
        , ( 0x46, ld_b_indirect_hl )
        , ( 0x4E, ld_c_indirect_hl )
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


ld_b_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_b_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
    --MainRegsWithPcAndCpuTime { main | b = value.value } value.pc value.time
    SingleEnvNewBRegister value.value (value.time |> addCpuTimeTime 3)


ld_c_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_c_indirect_hl z80_main rom48k z80_env =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
    --MainRegsWithPcAndCpuTime { main | c = value.value } value.pc value.time
    SingleEnvNewCRegister value.value (value.time |> addCpuTimeTime 3)


applySingleEnvMainChange : SingleEnvMainChange -> Z80 -> Z80
applySingleEnvMainChange z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        env =
            z80.env
    in
    case z80changeData of
        SingleEnvNewARegister int cpuTimeCTime ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 1) 0xFFFF

                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                flags =
                    z80.flags
            in
            { z80
                | pc = new_pc
                , flags = { flags | a = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        SingleEnvNewBRegister int cpuTimeCTime ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 1) 0xFFFF

                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | b = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        SingleEnvNewCRegister int cpuTimeCTime ->
            let
                new_pc =
                    Bitwise.and (z80.pc + 1) 0xFFFF

                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | c = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }
