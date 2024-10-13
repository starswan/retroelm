module Group0x30 exposing (..)

import Bitwise
import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import Utils exposing (byte, char)
import Z80Address exposing (Z80Address(..), addIndexOffset, decrement, fromInt, increment, increment2, toInt)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (addCpuTimeEnv, mem, setMem)
import Z80Flags exposing (add16, dec, inc, scf_ccf)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), Z80, env_mem_hl, get_xy, imm16, imm8, jr, set_xy)


delta_dict_30 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_30 =
    Dict.fromList
        [ ( 0x34, execute_0x34 )
        , ( 0x35, execute_0x35 )
        , ( 0x36, execute_0x36 )
        , ( 0x39, execute_0x39 )
        ]


delta_dict_lite_30 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_30 =
    Dict.fromList
        [ ( 0x30, execute_0x30 )
        , ( 0x31, execute_0x31 )
        , ( 0x32, execute_0x32 )
        , ( 0x33, execute_0x33 )
        , ( 0x38, execute_0x38 )
        , ( 0x3A, execute_0x3A )
        , ( 0x3B, execute_0x3B )
        , ( 0x3E, execute_0x3E )
        ]


execute_0x30 : Z80ROM -> Z80 -> Z80Delta
execute_0x30 rom48k z80 =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    if Bitwise.and z80.flags.ff 0x0100 == 0 then
        let
            x =
                z80 |> jr rom48k
        in
        --{ z80 | env = x.env, pc = x.register_value }
        CpuTimeWithPc x.time x.pc

    else
        let
            v =
                 imm8 z80.pc z80.env.time rom48k z80.env.ram
        in
        --{ z80 | env = v.env, pc = v.pc }
        CpuTimeWithPc v.time v.pc


execute_0x31 : Z80ROM -> Z80 -> Z80Delta
execute_0x31 rom48k z80 =
    -- case 0x31: SP=imm16(); break;
    let
        v =
            z80 |> imm16 rom48k
    in
    --{ z80 | env = v.env, pc = v.pc, sp = v.value }
    CpuTimeWithSpAndPc v.time (v.value |> fromInt) v.pc


execute_0x32 : Z80ROM -> Z80 -> Z80Delta
execute_0x32 rom48k z80 =
    -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        v =
            z80 |> imm16 rom48k

        --new_z80 = { z80 | pc = v.pc }
    in
    --{ new_z80 | env = v.env |> set_mem v.value new_z80.flags.a } |> add_cpu_time 3
    case v.value |> fromInt of
      ROMAddress int ->
        OnlyPc v.pc
      RAMAddress ramAddress ->
        EnvWithPc (z80.env |> setMem ramAddress z80.flags.a |> addCpuTimeEnv 3) v.pc

execute_0x33 : Z80ROM -> Z80 -> Z80Delta
execute_0x33 rom48k z80 =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    let
        new_sp =
            --Bitwise.and (z80.env.sp + 1) 0xFFFF
            z80.env.sp |> increment
    in
    SpAndCpuTime new_sp 2



--{ z80 | sp = new_sp } |> add_cpu_time 2


execute_0x34 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x34 ixiyhl rom48k z80 =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        cpuTimePcAndValue =
            z80 |> env_mem_hl ixiyhl rom48k

        env_1 =
            z80.env

        cpuTimeAndValue =
            mem cpuTimePcAndValue.value cpuTimePcAndValue.time rom48k env_1.ram

        valueWithFlags =
            z80.flags |> inc cpuTimeAndValue.value

        new_env =
          case cpuTimePcAndValue.value of
            ROMAddress int ->
              { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 }
            RAMAddress ramAdddress ->
              { env_1 | time = cpuTimeAndValue.time |> addCpuTimeTime 4 } |> setMem ramAdddress valueWithFlags.value
    in
    --{ z80_1 | env = new_env, flags = v.flags } |> add_cpu_time 3
    EnvWithFlagsAndPc (new_env |> addCpuTimeEnv 3) valueWithFlags.flags cpuTimePcAndValue.pc


execute_0x35 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x35 ixiyhl rom48k z80 =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        a =
            z80 |> env_mem_hl ixiyhl rom48k

        --z80_1 = { z80 | pc = a.pc }
        value =
            mem a.value z80.env.time rom48k z80.env.ram

        env_1 =
            z80.env

        v =
            z80.flags |> dec value.value

        new_env =
          case a.value of
            ROMAddress int ->
              { env_1 | time = value.time } |> addCpuTimeEnv 4
            RAMAddress z80WriteableAddress ->
              { env_1 | time = value.time } |> addCpuTimeEnv 4 |> setMem z80WriteableAddress v.value
        env_2 =
            new_env |> addCpuTimeEnv 3
    in
    --{ z80_1 | env = env_2, flags = v.flags }
    EnvWithFlagsAndPc env_2 v.flags a.pc


execute_0x36 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x36 ixiyhl rom48k z80 =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    --	v=env.mem((char)(PC+1)); time+=5;
    --	env.mem(a,v); PC=(char)(PC+2); time+=3;} break;
    case ixiyhl of
        HL ->
            let
                v =
                    imm8 z80.pc z80.env.time rom48k z80.env.ram

                env_1 =
                    z80.env

                new_z80 =
                    { z80 | env = { env_1 | time = v.time }, pc = v.pc }

                new_env = case z80.main.hl of
                  ROMAddress int ->
                    new_z80.env
                  RAMAddress z80WriteableAddress ->
                    new_z80.env |> setMem z80WriteableAddress v.value
            in
            --{ new_z80 | env = new_env }
            EnvWithPc new_env v.pc

        _ ->
            let
                xy =
                    get_xy ixiyhl z80.main

                mempc =
                    mem z80.pc z80.env.time rom48k z80.env.ram

                env_1 =
                    z80.env

                a =
                    --char (xy + byte mempc.value)
                    xy |> addIndexOffset mempc.value

                --z80_1 = { z80 | env = mempc.env } |> add_cpu_time 3
                --z80_1_env =
                --    { env_1 | time = mempc.time } |> addCpuTimeEnv 3

                v =
                    --mem (char (z80.pc + 1)) (mempc.time |> addCpuTimeTime 3) rom48k env_1.ram
                    mem (z80.pc |> increment) (mempc.time |> addCpuTimeTime 3) rom48k env_1.ram

                z80_2 =
                    { env_1 | time = (v.time |> addCpuTimeTime 5) }

                x = case a of
                  ROMAddress int ->
                    z80_2
                  RAMAddress z80WriteableAddress ->
                    z80_2 |> setMem z80WriteableAddress v.value

                new_pc =
                    --Bitwise.and (z80.pc + 2) 0xFFFF
                    z80.pc |> increment2
            in
            --{ z80_2 | env = x, pc = new_pc } |> add_cpu_time 3
            EnvWithPc (x |> addCpuTimeEnv 3) new_pc


execute_0x38 : Z80ROM -> Z80 -> Z80Delta
execute_0x38 rom48k z80 =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    if Bitwise.and z80.flags.ff 0x0100 /= 0 then
        let
            x =
                z80 |> jr rom48k
        in
        --{ z80 | env = x.env, pc = x.register_value }
        CpuTimeWithPc x.time x.pc

    else
        let
            v =
                imm8 z80.pc z80.env.time rom48k z80.env.ram
        in
        --{ z80 | env = v.env, pc = v.pc }
        CpuTimeWithPc v.time v.pc


execute_0x39 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x39 ixiyhl rom48k z80 =
    --case 0x39: HL=add16(HL,SP); break;
    --case 0x39: xy=add16(xy,SP); break;
    let
        xy =
            get_xy ixiyhl z80.main |> toInt

        new_xy =
            add16 xy (z80.env.sp |> toInt) z80.flags

        new_z80 =
            set_xy (new_xy.value |> fromInt) ixiyhl z80.main
    in
    --{ z80 | main = new_z80, flags = new_xy.flags }  |> add_cpu_time new_xy.time
    FlagsWithPCMainAndTime new_xy.flags z80.pc new_z80 new_xy.time


execute_0x3A : Z80ROM -> Z80 -> Z80Delta
execute_0x3A rom48k z80 =
    -- case 0x3A: MP=(v=imm16())+1; A=env.mem(v); time+=3; break;
    let
        z80_flags =
            z80.flags

        v =
            z80 |> imm16 rom48k

        mem_value =
            mem (v.value |> fromInt) z80.env.time rom48k z80.env.ram
    in
    CpuTimeWithFlagsAndPc (mem_value.time |> addCpuTimeTime 3) { z80_flags | a = mem_value.value } v.pc


execute_0x3B : Z80ROM -> Z80 -> Z80Delta
execute_0x3B rom48k z80 =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    let
        new_sp =
            --Bitwise.and (z80.env.sp - 1) 0xFFFF
            z80.env.sp |> decrement
    in
    SpAndCpuTime new_sp 2


execute_0x3E : Z80ROM -> Z80 -> Z80Delta
execute_0x3E rom48k z80 =
    -- case 0x3E: A=imm8(); break;
    let
        z80_flags =
            z80.flags

        v =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        --new_z80 = { z80 | env = v.env, pc = v.pc }
    in
    --{ new_z80 | flags = { z80_flags | a = v.value } }
    CpuTimeWithFlagsAndPc v.time { z80_flags | a = v.value } v.pc
