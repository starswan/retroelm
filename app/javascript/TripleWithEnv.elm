module TripleWithEnv exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime)
import Dict exposing (Dict)
import Z80Env exposing (Z80Env, mem, mem16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80)


type TripleWithEnvChange
    = NewHLRegister Int CpuTimeCTime
    | TripleEnvNewARegister Int CpuTimeCTime


triple16WithEnv : Dict Int (Int -> Z80ROM -> Z80Env -> TripleWithEnvChange)
triple16WithEnv =
    Dict.fromList
        [ ( 0x2A, ld_hl_indirect_nn )
        , ( 0x3A, ld_a_indirect_nn )
        ]


applyTripleEnvMainChange : TripleWithEnvChange -> Z80 -> Z80
applyTripleEnvMainChange change z80 =
    case change of
        NewHLRegister int cpuTime ->
            let
                main =
                    z80.main

                env =
                    z80.env

                pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF
            in
            { z80 | pc = pc, main = { main | hl = int }, env = { env | time = cpuTime } }

        TripleEnvNewARegister int cpuTimeCTime ->
            let
                flags =
                    z80.flags

                env =
                    z80.env

                pc =
                    Bitwise.and (z80.pc + 3) 0xFFFF
            in
            { z80 | pc = pc, flags = { flags | a = int }, env = { env | time = cpuTimeCTime } }


ld_hl_indirect_nn : Int -> Z80ROM -> Z80Env -> TripleWithEnvChange
ld_hl_indirect_nn param16 rom48k z80_env =
    -- case 0x2A: MP=(v=imm16())+1; HL=env.mem16(v); time+=6; break;
    -- case 0x2A: MP=(v=imm16())+1; xy=env.mem16(v); time+=6; break;
    let
        --v =
        --    z80 |> imm16 rom48k
        --z80_1 = { z80 | pc = v.pc }
        --new_xy =
        --    z80_env |> mem16 v.value rom48k
        new_xy =
            z80_env |> mem16 param16 rom48k

        --z80_2 = { z80_1 | env = new_xy.env }
        --main =
        --    z80.main |> set_xy new_xy.value ixiyhl
    in
    --{ z80_2 | main = main } |> add_cpu_time 6
    --MainRegsWithPcAndCpuTime main v.pc (new_xy.time |> addCpuTimeTime 6)
    NewHLRegister new_xy.value (new_xy.time |> addCpuTimeTime 6)


ld_a_indirect_nn : Int -> Z80ROM -> Z80Env -> TripleWithEnvChange
ld_a_indirect_nn param16 rom48k z80_env =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    let
        --z80_flags =
        --    z80.flags
        --
        --v =
        --    z80 |> imm16 rom48k
        mem_value =
            mem param16 z80_env.time rom48k z80_env.ram
    in
    --CpuTimeWithFlagsAndPc (mem_value.time |> addCpuTimeTime 3) { z80_flags | a = mem_value.value } v.pc
    TripleEnvNewARegister mem_value.value (mem_value.time |> addCpuTimeTime 3)
