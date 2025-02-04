module SingleEnvWithMain exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeAndValue, CpuTimeCTime, CpuTimeIncrement, addCpuTimeTime, addCpuTimeTimeInc, increment3)
import Dict exposing (Dict)
import PCIncrement exposing (PCIncrement(..))
import TransformTypes exposing (InstructionDuration(..))
import Utils exposing (shiftLeftBy8)
import Z80Env exposing (Z80Env, mem)
import Z80Flags exposing (BitTest(..), FlagRegisters, add16, testBit)
import Z80Rom exposing (Z80ROM)
import Z80Transform exposing (ChangeMainFlagsOperation(..), ChangeMainOperation(..), InstructionLength(..), Z80Operation(..), Z80Transform)
import Z80Types exposing (MainWithIndexRegisters, Z80)


type SingleEnvMainChange
    = SingleEnvNewARegister Int CpuTimeCTime
    | SingleEnvNewBRegister Int CpuTimeCTime
    | SingleEnvNewCRegister Int CpuTimeCTime
    | SingleEnvNewDRegister Int CpuTimeCTime
    | SingleEnvNewERegister Int CpuTimeCTime
    | SingleEnvNewHLRegister Int CpuTimeCTime
    | SingleBitTest BitTest CpuTimeAndValue
    | SingleEnvNewHL16BitAdd Int Int CpuTimeCTime
    | SingleEnvNewIX16BitAdd Int Int CpuTimeCTime
    | SingleEnvNewIY16BitAdd Int Int CpuTimeCTime


standardSingleEnvMain : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement )
standardSingleEnvMain =
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
        , ( 0x39, ( add_hl_sp, IncrementByOne ) )
        ]

ixSingleEnvMain : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement )
ixSingleEnvMain =
    Dict.fromList
        [ ( 0xDD39, ( add_ix_sp, IncrementByTwo ) )
        ]

iySingleEnvMain : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement )
iySingleEnvMain =
    Dict.fromList
        [ ( 0xFD39, ( add_iy_sp, IncrementByTwo ) )
        ]

cbSingleEnvMain : Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement )
cbSingleEnvMain =
    Dict.fromList
        [ ( 0xCB46, ( bit_0_indirect_hl, IncrementByTwo ) )
        , ( 0xCB4E, ( bit_1_indirect_hl, IncrementByTwo ) )
        , ( 0xCB56, ( bit_2_indirect_hl, IncrementByTwo ) )
        , ( 0xCB5E, ( bit_3_indirect_hl, IncrementByTwo ) )
        , ( 0xCB66, ( bit_4_indirect_hl, IncrementByTwo ) )
        , ( 0xCB6E, ( bit_5_indirect_hl, IncrementByTwo ) )
        , ( 0xCB76, ( bit_6_indirect_hl, IncrementByTwo ) )
        , ( 0xCB7E, ( bit_7_indirect_hl, IncrementByTwo ) )
        ]


parseSingleEnvMain :  Dict Int ( MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange, PCIncrement ) -> CpuTimeCTime -> Int -> Z80ROM -> Z80 -> Maybe Z80Transform
parseSingleEnvMain operationDict _ instrCode rom48k z80 =
    case operationDict |> Dict.get instrCode of
        Just ( f, pcInc ) ->
            let
                x =
                    z80.flags |> applySingleEnvMainChange pcInc (f z80.main rom48k z80.env)
            in
            Just x

        Nothing ->
            Nothing


applySingleEnvMainChange : PCIncrement -> SingleEnvMainChange -> FlagRegisters -> Z80Transform
applySingleEnvMainChange pcInc z80changeData z80_flags =
    let
        --interrupts =
        --    z80.interrupts
        --env =
        --    z80.env
        new_pc =
            case pcInc of
                IncrementByOne ->
                    OneByteInstruction

                --Bitwise.and (z80.pc + 1) 0xFFFF
                --
                IncrementByTwo ->
                    TwoByteInstruction

        --Bitwise.and (z80.pc + 2) 0xFFFF
    in
    case z80changeData of
        SingleEnvNewARegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeFlagRegisters { z80_flags | a = int } }

        SingleEnvNewBRegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeMain (ChangeBRegister int) }

        SingleEnvNewCRegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeMain (ChangeCRegister int) }

        SingleEnvNewDRegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeMain (ChangeDRegister int) }

        SingleEnvNewERegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeMain (ChangeERegister int) }

        SingleEnvNewHLRegister int cpuTimeCTime ->
            { pcIncrement = new_pc, time = cpuTimeCTime, timeIncrement = FourTStates, operation = ChangeMain (ChangeHLRegister int) }

        SingleBitTest bitTest intwithTime ->
            let
                flags =
                    z80_flags |> testBit bitTest intwithTime.value
            in
            { pcIncrement = new_pc, time = intwithTime.time, timeIncrement = FourTStates, operation = ChangeFlagRegisters flags }

        SingleEnvNewHL16BitAdd hl sp ctime ->
            let
                new_xy =
                    add16 hl sp z80_flags

                --new_time =
                --    ctime |> addCpuTimeTimeInc new_xy.time
            in
            { pcIncrement = new_pc, time = ctime, timeIncrement = ElevenTStates, operation = ChangeMainWithFlags (ChangeMainFlagsHL new_xy.value) new_xy.flags }

        SingleEnvNewIX16BitAdd hl sp ctime ->
            let
                new_xy =
                    add16 hl sp z80_flags

                --new_time =
                --    ctime |> addCpuTimeTimeInc new_xy.time
            in
            { pcIncrement = new_pc, time = ctime, timeIncrement = ElevenTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIX new_xy.value) new_xy.flags }

        SingleEnvNewIY16BitAdd hl sp ctime ->
            let
                new_xy =
                    add16 hl sp z80_flags

                --new_time =
                --    ctime |> addCpuTimeTimeInc new_xy.time
            in
            { pcIncrement = new_pc, time = ctime, timeIncrement = ElevenTStates, operation = ChangeMainWithFlags (ChangeMainFlagsIY new_xy.value) new_xy.flags }


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


ld_d_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_d_indirect_hl z80_main rom48k z80_env =
    -- case 0x56: D=env.mem(HL); time+=3; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = value.env } |> set_d value.value
    --MainRegsWithPcAndCpuTime { main | d = value.value } value.pc value.time
    SingleEnvNewDRegister value.value (value.time |> addCpuTimeTime 3)


ld_e_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_e_indirect_hl z80_main rom48k z80_env =
    -- case 0x5E: E=env.mem(HL); time+=3; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
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
            mem z80_main.hl z80_env.time rom48k z80_env.ram

        new_hl =
            (z80_main.hl |> Bitwise.and 0xFF) |> Bitwise.or (value.value |> shiftLeftBy8)
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
            mem z80_main.hl z80_env.time rom48k z80_env.ram

        new_hl =
            z80_main.hl |> Bitwise.and 0xFF00 |> Bitwise.or value.value
    in
    --MainRegsWithPcAndCpuTime (main |> set_h value.value HL) value.pc (value.time |> addCpuTimeTime 3)
    SingleEnvNewHLRegister new_hl (value.time |> addCpuTimeTime 3)


ld_a_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
ld_a_indirect_hl z80_main rom48k z80_env =
    -- case 0x7E: A=env.mem(HL); time+=3; break;
    -- case 0x7E: A=env.mem(getd(xy)); time+=3; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    --{ z80 | pc = value.pc, env = { env_1 | time = value.time } } |> set_a value.value
    SingleEnvNewARegister value.value (value.time |> addCpuTimeTimeInc increment3)


bit_0_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_0_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_0 value


bit_1_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_1_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_1 value


bit_2_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_2_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_2 value


bit_3_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_3_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_3 value


bit_4_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_4_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_4 value


bit_5_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_5_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_5 value


bit_6_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_6_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_6 value


bit_7_indirect_hl : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
bit_7_indirect_hl z80_main rom48k z80_env =
    -- case 0x46: bit(o,env.mem(HL)); Ff=Ff&~F53|MP>>>8&F53; time+=4; break;
    let
        value =
            mem z80_main.hl z80_env.time rom48k z80_env.ram
    in
    SingleBitTest Bit_7 value


add_hl_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_hl_sp z80_main rom48k z80_env =
    --case 0x39: HL=add16(HL,SP); break;
    SingleEnvNewHL16BitAdd z80_main.hl z80_env.sp z80_env.time


add_ix_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_ix_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewIX16BitAdd z80_main.ix z80_env.sp z80_env.time


add_iy_sp : MainWithIndexRegisters -> Z80ROM -> Z80Env -> SingleEnvMainChange
add_iy_sp z80_main rom48k z80_env =
    --case 0x39: xy=add16(xy,SP); break;
    SingleEnvNewIY16BitAdd z80_main.iy z80_env.sp z80_env.time
