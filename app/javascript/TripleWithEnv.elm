module TripleWithEnv exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime)
import Dict exposing (Dict)
import Z80Env exposing (Z80Env, mem16)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80)


type TripleWithEnvChange
    = NewHLRegister Int CpuTimeCTime


triple16WithEnv : Dict Int (Int -> Z80ROM -> Z80Env -> TripleWithEnvChange)
triple16WithEnv =
    Dict.fromList
        [ ( 0x2A, ld_hl_indirect_nn )
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
