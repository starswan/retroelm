module Group0x30 exposing (..)

import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Z80Address exposing (addIndexOffset, incrementBy1, incrementBy2, toInt)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, mem, setMem)
import Z80Flags exposing (add16, dec, inc)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY, IXIYHL(..), Z80, env_mem_hl_ixiy, get_xy, get_xy_ixiy, imm16, set_xy)


miniDict30 : Dict Int (IXIY -> Z80ROM -> Z80 -> Z80Delta)
miniDict30 =
    Dict.fromList
        [ ( 0x34, inc_indirect_hl )
        , ( 0x35, dec_indirect_hl )
        , ( 0x36, ld_indirect_hl_n )
        ]


delta_dict_30 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_30 =
    Dict.fromList
        [ ( 0x39, add_hl_sp ) -- need single byte with env, main amd flags for this
        ]


delta_dict_lite_30 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_30 =
    Dict.fromList
        [ ( 0x3A, ld_a_indirect_nn ) -- need triple byte with env for this
        ]


inc_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
inc_indirect_hl ixiyhl rom48k z80 =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        cpuTimePcAndValue =
            z80 |> env_mem_hl_ixiy ixiyhl rom48k

        env_1 =
            z80.env

        cpuTimeAndValue =
            mem cpuTimePcAndValue.address cpuTimePcAndValue.time rom48k env_1.ram

        valueWithFlags =
            z80.flags |> inc cpuTimeAndValue.value

        new_env =
            { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 } |> setMem cpuTimePcAndValue.address valueWithFlags.value

        --case cpuTimePcAndValue.value of
        --  ROMAddress int ->
        --    { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 }
        --  RAMAddress ramAdddress ->
        --    { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 } |> setMem ramAdddress valueWithFlags.value

        --case cpuTimePcAndValue.value of
        --  ROMAddress int ->
        --    { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 }
        --  RAMAddress ramAdddress ->
        --    { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 } |> setMem ramAdddress valueWithFlags.value
    in
    --{ z80_1 | env = new_env, flags = v.flags } |> add_cpu_time 3
    EnvWithFlagsAndPc (new_env |> addCpuTimeEnv 3) valueWithFlags.flags cpuTimePcAndValue.pc


dec_indirect_hl : IXIY -> Z80ROM -> Z80 -> Z80Delta
dec_indirect_hl ixiyhl rom48k z80 =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        a =
            z80 |> env_mem_hl_ixiy ixiyhl rom48k

        --z80_1 = { z80 | pc = a.pc }
        value =
            mem a.address z80.env.time rom48k z80.env.ram

        env_1 =
            z80.env

        v =
            z80.flags |> dec value.value

        new_env =
            { env_1 | time = value.time } |> addCpuTimeEnv 4 |> setMem a.address v.value

        --case a.value of
        --  ROMAddress int ->
        --    { env_1 | time = value.time } |> addCpuTimeEnv 4
        --  RAMAddress z80WriteableAddress ->
        --    { env_1 | time = value.time } |> addCpuTimeEnv 4 |> setMem z80WriteableAddress v.value
        env_2 =
            new_env |> addCpuTimeEnv 3
    in
    --{ z80_1 | env = env_2, flags = v.flags }
    EnvWithFlagsAndPc env_2 v.flags a.pc


ld_indirect_hl_n : IXIY -> Z80ROM -> Z80 -> Z80Delta
ld_indirect_hl_n ixiyhl rom48k z80 =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    --	v=env.mem((char)(PC+1)); time+=5;
    --	env.mem(a,v); PC=(char)(PC+2); time+=3;} break;
    let
        xy =
            get_xy_ixiy ixiyhl z80.main

        mempc =
            mem (z80.pc) z80.env.time rom48k z80.env.ram

        env_1 =
            z80.env

        a =
            xy |> addIndexOffset mempc.value

        v =
            --mem (char (z80.pc + 1)) (mempc.time |> addCpuTimeTime 3) rom48k env_1.ram
            mem (z80.pc |> incrementBy1) (mempc.time |> addCpuTimeTime 3) rom48k env_1.ram

        z80_2 =
            { env_1 | time = v.time |> addCpuTimeTime 5 }

        x =
            setMem a v.value z80_2

        --x = case a of
        --  ROMAddress int ->
        --    z80_2
        --  RAMAddress z80WriteableAddress ->
        --    z80_2 |> setMem z80WriteableAddress v.value
        new_pc =
            --Bitwise.and (z80.pc + 2) 0xFFFF
            z80.pc |> incrementBy2
    in
    --{ z80_2 | env = x, pc = new_pc } |> add_cpu_time 3
    EnvWithPc (x |> addCpuTimeEnv 3) new_pc


add_hl_sp : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
add_hl_sp ixiyhl rom48k z80 =
    --case 0x39: HL=add16(HL,SP); break;
    --case 0x39: xy=add16(xy,SP); break;
    let
        xy =
            get_xy ixiyhl z80.main

        new_xy =
            add16 xy (z80.env.sp |> toInt) z80.flags

        new_z80 =
            set_xy new_xy.value ixiyhl z80.main
    in
    --{ z80 | main = new_z80, flags = new_xy.flags }  |> add_cpu_time new_xy.time
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time



-- need triplebytewithenv for this


ld_a_indirect_nn : Z80ROM -> Z80 -> Z80Delta
ld_a_indirect_nn rom48k z80 =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        v =
            z80 |> imm16 rom48k

        mem_value =
            mem v.address z80.env.time rom48k z80.env.ram
    in
    CpuTimeWithFlagsAndPc (mem_value.time |> addCpuTimeTime 3) { z80_flags | a = mem_value.value } v.pc
