module Group30 exposing (..)

import Bitwise
import CpuTimeCTime exposing (add_cpu_time_time)
import Dict exposing (Dict)
import Utils exposing (byte, char)
import Z80Delta exposing (Z80Delta(..))
import Z80Env exposing (add_cpu_time_env, mem, set_mem)
import Z80Flags exposing (add16, dec, inc, scf_ccf)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIYHL(..), Z80, env_mem_hl, get_xy, imm16, imm8, jr, set_xy)

delta_dict_30 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_30 =
    Dict.fromList
        [
          (0x34, execute_0x34),
          (0x35, execute_0x35),
          (0x36, execute_0x36),
          (0x39, execute_0x39)
        ]


delta_dict_lite_30 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_30 =
    Dict.fromList
        [
          (0x30, execute_0x30),
          (0x31, execute_0x31),
          (0x32, execute_0x32),
          (0x33, execute_0x33),
          (0x37, execute_0x37),
          (0x38, execute_0x38),
          (0x3A, execute_0x3A),
          (0x3B, execute_0x3B),
          (0x3C, execute_0x3C),
          (0x3D, execute_0x3D),
          (0x3E, execute_0x3E),
          (0x3F, execute_0x3F)
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
                z80 |> imm8 rom48k
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
    CpuTimeWithSpAndPc v.time v.value v.pc


execute_0x32 : Z80ROM -> Z80 -> Z80Delta
execute_0x32 rom48k z80 =
    -- case 0x32: MP=(v=imm16())+1&0xFF|A<<8; env.mem(v,A); time+=3; break;
    let
        v =
            z80 |> imm16 rom48k

        --new_z80 = { z80 | pc = v.pc }
    in
    EnvWithPc (z80.env |> set_mem v.value z80.flags.a |> add_cpu_time_env 3) v.pc



--{ new_z80 | env = v.env |> set_mem v.value new_z80.flags.a } |> add_cpu_time 3


execute_0x33 : Z80ROM -> Z80 -> Z80Delta
execute_0x33 rom48k z80 =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    let
        new_sp =
            Bitwise.and (z80.env.sp + 1) 0xFFFF
    in
    SpAndCpuTime new_sp 2



--{ z80 | sp = new_sp } |> add_cpu_time 2


execute_0x34 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x34 ixiyhl rom48k z80 =
    -- case 0x34: v=inc(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x34: {int a; v=inc(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        z80_flags =
            z80.flags

        a =
            z80 |> env_mem_hl ixiyhl rom48k

        env_1 =
            z80.env

        env_2 =
            { env_1 | time = a.time }

        value =
            env_2 |> mem a.value rom48k

        v =
            z80_flags |> inc value.value

        --z80_1 = { z80 | pc = a.pc } |> add_cpu_time 4
        new_env =
            { env_2 | time = value.time |> add_cpu_time_time 4 } |> set_mem a.value v.value
    in
    --{ z80_1 | env = new_env, flags = v.flags } |> add_cpu_time 3
    EnvWithFlagsAndPc (new_env |> add_cpu_time_env 3) v.flags a.pc


execute_0x35 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x35 ixiyhl rom48k z80 =
    -- case 0x35: v=dec(env.mem(HL)); time+=4; env.mem(HL,v); time+=3; break;
    -- case 0x35: {int a; v=dec(env.mem(a=getd(xy))); time+=4; env.mem(a,v); time+=3;} break;
    let
        a =
            z80 |> env_mem_hl ixiyhl rom48k

        --z80_1 = { z80 | pc = a.pc }
        value =
            z80.env |> mem a.value rom48k

        env_1 =
            z80.env

        v =
            z80.flags |> dec value.value

        new_env =
            { env_1 | time = value.time } |> add_cpu_time_env 4 |> set_mem a.value v.value

        env_2 =
            new_env |> add_cpu_time_env 3
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
                    z80 |> imm8 rom48k

                env_1 =
                    z80.env

                new_z80 =
                    { z80 | env = { env_1 | time = v.time }, pc = v.pc }

                new_env =
                    set_mem z80.main.hl v.value new_z80.env
            in
            --{ new_z80 | env = new_env }
            EnvWithPc new_env v.pc

        _ ->
            let
                xy =
                    get_xy ixiyhl z80.main

                mempc =
                    z80.env |> mem z80.pc rom48k

                env_1 =
                    z80.env

                a =
                    char (xy + byte mempc.value)

                --z80_1 = { z80 | env = mempc.env } |> add_cpu_time 3
                z80_1_env =
                    { env_1 | time = mempc.time } |> add_cpu_time_env 3

                v =
                    z80_1_env |> mem (char (z80.pc + 1)) rom48k

                z80_2 =
                    z80_1_env |> add_cpu_time_env 5

                x =
                    set_mem a v.value z80_2

                new_pc =
                    Bitwise.and (z80.pc + 2) 0xFFFF
            in
            --{ z80_2 | env = x, pc = new_pc } |> add_cpu_time 3
            EnvWithPc (x |> add_cpu_time_env 3) new_pc


execute_0x37 : Z80ROM -> Z80 -> Z80Delta
execute_0x37 rom48k z80 =
    -- case 0x37: scf_ccf(0); break;
    --{ z80 | flags = z80.flags |> scf_ccf 0 }
    z80.flags |> scf_ccf 0 |> FlagRegs


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
                z80 |> imm8 rom48k
        in
        --{ z80 | env = v.env, pc = v.pc }
        CpuTimeWithPc v.time v.pc


execute_0x39 : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0x39 ixiyhl rom48k z80 =
    --case 0x39: HL=add16(HL,SP); break;
    --case 0x39: xy=add16(xy,SP); break;
    let
        xy =
            get_xy ixiyhl z80.main

        new_xy =
            add16 xy z80.env.sp z80.flags

        new_z80 =
            set_xy new_xy.value ixiyhl z80.main
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
            z80.env |> mem v.value rom48k
    in
    CpuTimeWithFlagsAndPc (mem_value.time |> add_cpu_time_time 3) { z80_flags | a = mem_value.value } v.pc


execute_0x3B : Z80ROM -> Z80 -> Z80Delta
execute_0x3B rom48k z80 =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    let
        new_sp =
            Bitwise.and (z80.env.sp - 1) 0xFFFF
    in
    SpAndCpuTime new_sp 2


execute_0x3C : Z80ROM -> Z80 -> Z80Delta
execute_0x3C rom48k z80 =
    -- case 0x3C: A=inc(A); break;
    let
        v =
            inc z80.flags.a z80.flags

        new_flags =
            v.flags
    in
    --{ z80 | flags = { new_flags | a = v.value } }
    FlagRegs { new_flags | a = v.value }


execute_0x3D : Z80ROM -> Z80 -> Z80Delta
execute_0x3D rom48k z80 =
    -- case 0x3D: A=dec(A); break;
    let
        v =
            dec z80.flags.a z80.flags

        new_flags =
            v.flags
    in
    --{ z80 | flags = { new_flags | a = v.value } }
    FlagRegs { new_flags | a = v.value }


execute_0x3E : Z80ROM -> Z80 -> Z80Delta
execute_0x3E rom48k z80 =
    -- case 0x3E: A=imm8(); break;
    let
        z80_flags =
            z80.flags

        v =

            z80 |> imm8 rom48k

        --new_z80 = { z80 | env = v.env, pc = v.pc }
    in
    --{ new_z80 | flags = { z80_flags | a = v.value } }
    CpuTimeWithFlagsAndPc v.time { z80_flags | a = v.value } v.pc


execute_0x3F : Z80ROM -> Z80 -> Z80Delta
execute_0x3F rom48k z80 =
    -- case 0x3F: scf_ccf(Ff&0x100); break;
    --{ z80 | flags = z80.flags |> scf_ccf (and z80.flags.ff 0x100) }
    z80.flags |> scf_ccf (Bitwise.and z80.flags.ff 0x0100) |> FlagRegs
