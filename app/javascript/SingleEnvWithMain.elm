module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime, addCpuTimeTimeInc, cpuTimeIncrement4, increment3)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import Utils exposing (shiftLeftBy8)
import Z80Address exposing (Z80Address, fromInt, incrementBy1, incrementBy2, lower8Bits, top8BitsWithoutShift)
import Z80Env exposing (Z80Env, addCpuTimeEnvInc, mem)
import Z80Flags exposing (BitTest(..), testBit)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnvNewBRegister Int CpuTimeCTime
    | SingleEnvNewCRegister Int CpuTimeCTime
    | SingleEnvNewDRegister Int CpuTimeCTime
    | SingleEnvNewERegister Int CpuTimeCTime
    | SingleEnvNewHLRegister Z80Address CpuTimeCTime
    | SingleBitTest BitTest CpuTimeAndValue


singleEnvMainRegs : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement )
singleEnvMainRegs =
    Dict.fromList
        [ ( 0x0A, ( ld_a_indirect_bc, IncrementByOne ) )
        , ( 0x1A, ( ld_a_indirect_de, IncrementByOne ) )
        , ( 0x46, ( ld_b_indirect_hl, IncrementByOne ) )
        , ( 0x4E, ( ld_c_indirect_hl, IncrementByOne ) )
        , ( 0x56, ( ld_d_indirect_hl, IncrementByOne ) )
        , ( 0x5E, ( ld_e_indirect_hl, IncrementByOne ) )
        , ( 0x66, ( ld_h_indirect_hl, IncrementByOne ) )
        , ( 0x6E, ( ld_l_indirect_hl, IncrementByOne ) )
        , ( 0x7E, ( ld_a_indirect_hl, IncrementByOne ) )
        , ( 0xCB46, ( bit_0_indirect_hl, IncrementByTwo ) )
        , ( 0xCB4E, ( bit_1_indirect_hl, IncrementByTwo ) )
        , ( 0xCB56, ( bit_2_indirect_hl, IncrementByTwo ) )
        , ( 0xCB5E, ( bit_3_indirect_hl, IncrementByTwo ) )
        , ( 0xCB66, ( bit_4_indirect_hl, IncrementByTwo ) )
        , ( 0xCB6E, ( bit_5_indirect_hl, IncrementByTwo ) )
        , ( 0xCB76, ( bit_6_indirect_hl, IncrementByTwo ) )
        , ( 0xCB7E, ( bit_7_indirect_hl, IncrementByTwo ) )
        ]


applySingleEnvMainChange : PCIncrement -> SingleEnvMainChange -> Z80 -> Z80
applySingleEnvMainChange pcInc z80changeData z80 =
    let
        interrupts =
            z80.interrupts

        env =
            z80.env

        new_pc =
            case pcInc of
                IncrementByOne ->
                    z80.pc |> incrementBy1

                IncrementByTwo ->
                    z80.pc |> incrementBy2
    in
    case z80changeData of
        SingleEnvNewARegister int cpuTimeCTime ->
            let
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

        SingleEnvNewDRegister int cpuTimeCTime ->
            let
                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | d = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        SingleEnvNewERegister int cpuTimeCTime ->
            let
                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | e = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        SingleEnvNewHLRegister int cpuTimeCTime ->
            let
                env1 =
                    { env | time = cpuTimeCTime } |> addCpuTimeEnvInc cpuTimeIncrement4

                main =
                    z80.main
            in
            { z80
                | pc = new_pc
                , main = { main | hl = int }
                , env = env1
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        SingleBitTest bitTest intwithTime ->
            { z80
                | pc = new_pc
                , env = { env | time = intwithTime.time }
                , flags = z80.flags |> testBit bitTest intwithTime.value
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }


ld_a_indirect_bc : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_bc z80_main rom48k z80_env =
    -- case 0x0A: MP=(v=B<<8|C)+1; A=env.mem(v); time+=3; break;
    let
        v =
            Bitwise.or (shiftLeftBy8 z80_main.b) z80_main.c |> fromInt

        new_a =
            mem v z80_env.time rom48k z80_env.ram
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    SingleEnvNewARegister new_a.value (new_a.time |> addCpuTimeTime 3)


ld_a_indirect_de : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_de z80_main rom48k z80_env =
    -- case 0x1A: MP=(v=D<<8|E)+1; A=env.mem(v); time+=3; break;
    let
        addr =
            Bitwise.or (shiftLeftBy8 z80_main.d) z80_main.e |> fromInt

        new_a =
            mem addr z80_env.time rom48k z80_env.ram
    in
    --{ z80 | env = new_a.env, flags = new_flags } |> add_cpu_time 3
    SingleEnvNewARegister new_a.value (new_a.time |> addCpuTimeTime 3)


ld_b_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_b_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: B=env.mem(HL); time+=3; break;
    -- case 0x46: B=env.mem(getd(xy)); time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_b value.value
    SingleEnvNewBRegister value.value (value.time |> addCpuTimeTime 3)


ld_c_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_c_indirect_hl z80_main rom48k z80_env =
    -- case 0x4E: C=env.mem(HL); time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_c value.value
    --MainRegsWithPcAndCpuTime { main | c = value.value } value.pc value.time
    SingleEnvNewCRegister value.value (value.time |> addCpuTimeTime 3)


ld_d_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_d_indirect_hl z80_main rom48k z80_env =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    --MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time
    SingleEnvNewDRegister value.value (value.time |> addCpuTimeTime 3)


ld_e_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_e_indirect_hl z80_main rom48k z80_env =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_e value.value
    --MainRegsWithPcAndCpuTime { main | e = value.value } value.pc value.time
    SingleEnvNewERegister value.value (value.time |> addCpuTimeTime 3)


ld_h_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_h_indirect_hl z80_main rom48k z80_env =
    -- case 0x66: HL=HL&0xFF|env.mem(HL)<<8; time+=3; break;
    -- case 0x66: HL=HL&0xFF|env.mem(getd(xy))<<8; time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram

        new_hl =
            (z80_main.hl |> lower8Bits) |> Bitwise.or (value.value |> shiftLeftBy8) |> fromInt
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_h_z80 value.value HL |> add_cpu_time 3
    --MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)
    SingleEnvNewHLRegister new_hl (value.time |> addCpuTimeTime 3)


ld_l_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_l_indirect_hl z80_main rom48k z80_env =
    -- case 0x6E: HL=HL&0xFF00|env.mem(HL); time+=3; break;
    -- case 0x6E: HL=HL&0xFF00|env.mem(getd(xy)); time+=3; break;
    let
        value =
            mem (z80_main.hl)z80_env.time rom48k z80_env.ram
        --new_hl = z80_main.hl |> Bitwise.and 0xFF00 |> Bitwise.or value.value
        new_hl = z80_main.hl |> top8BitsWithoutShift |> Bitwise.or value.value |> fromInt
    in
    --MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)
    SingleEnvNewHLRegister new_hl (value.time |> addCpuTimeTime 3)


ld_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_hl z80_main rom48k z80_env =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    SingleEnvNewARegister value.value (value.time |> addCpuTimeTimeInc increment3)


bit_0_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_0_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_0 value



bit_1_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_1_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_1 value


bit_2_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_2_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_2 value


bit_3_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_3_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_3 value


bit_4_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_4_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_4 value


bit_5_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_5_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_5 value


bit_6_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_6_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_6 value


bit_7_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_7_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem (z80_main.hl) z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_7 value
