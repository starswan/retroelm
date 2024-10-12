module Group0xC0 exposing (..)

import CpuTimeCTime exposing (addCpuTimeTime)
import Dict exposing (Dict)
import GroupCB exposing (group_cb, group_xy_cb)
import Z80Delta exposing (Z80Delta(..), jp, jp_delta, rst_delta)
import Z80Env exposing (addCpuTimeEnv, pop)
import Z80Flags exposing (adc, z80_add)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (IXIY(..), IXIYHL(..), Z80, call_if, get_bc, imm16, imm8, set_bc_main)


delta_dict_C0 : Dict Int (IXIYHL -> Z80ROM -> Z80 -> Z80Delta)
delta_dict_C0 =
    Dict.fromList
        [ ( 0xCB, execute_0xCB )
        ]


delta_dict_lite_C0 : Dict Int (Z80ROM -> Z80 -> Z80Delta)
delta_dict_lite_C0 =
    Dict.fromList
        [ ( 0xC0, execute_0xC0 )
        , ( 0xC1, execute_0xC1 )
        , ( 0xC2, execute_0xC2 )
        , ( 0xC3, execute_0xC3 )
        , ( 0xC4, execute_0xC4 )
        , ( 0xC5, execute_0xC5 )
        , ( 0xC6, execute_0xC6 )
        , ( 0xC7, execute_0xC7 )
        , ( 0xC8, retz_0xC8 )
        , ( 0xC9, execute_0xC9 )
        , ( 0xCA, execute_0xCA )
        , ( 0xCC, execute_0xCC )
        , ( 0xCD, call_0xCD )
        , ( 0xCE, execute_0xCE )
        , ( 0xCF, execute_0xCF )
        ]


execute_0xC0 : Z80ROM -> Z80 -> Z80Delta
execute_0xC0 rom48k z80 =
    -- case 0xC0: time++; if(Fr!=0) MP=PC=pop(); break;
    let
        env =
            z80.env |> addCpuTimeEnv 1
    in
    if z80.flags.fr /= 0 then
        let
            result =
                env |> pop rom48k

            --   env = z80_1.env
            --   --x = debug_log "ret nz" (result.value |> subName) Nothing
            --   z80_2 = { z80_1 | env = { env | time = result.time, sp = result.sp } }
        in
        --   { z80_2 | pc = result.value }
        CpuTimeWithSpAndPc result.time result.sp result.value

    else
        --z80_1
        NoChange


execute_0xC1 : Z80ROM -> Z80 -> Z80Delta
execute_0xC1 rom48k z80 =
    -- case 0xC1: v=pop(); B=v>>>8; C=v&0xFF; break;
    let
        v =
            z80.env |> pop rom48k

        --env = z80.env
        --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
        --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
    in
    --z80_1 |> set_bc v.value
    MainRegsWithSpAndTime (z80.main |> set_bc_main v.value) v.sp v.time


execute_0xC2 : Z80ROM -> Z80 -> Z80Delta
execute_0xC2 rom48k z80 =
    -- case 0xC2: jp(Fr!=0); break;
    --jp_z80 (z80.flags.fr /= 0) z80
    let
        v =
            z80 |> jp (z80.flags.fr /= 0) rom48k
    in
    CpuTimeWithPc v.time v.pc


execute_0xC3 : Z80ROM -> Z80 -> Z80Delta
execute_0xC3 rom48k z80 =
    -- case 0xC3: MP=PC=imm16(); break;
    let
        v =
            z80 |> imm16 rom48k

        --env = z80.env
        --z80_1 = { z80 | pc = v.pc, env = { env | time = v.time } }
        --y = debug_log "jp" (v.value |> subName) Nothing
    in
    --z80_1 |> set_pc v.value
    CpuTimeWithPc v.time v.value


execute_0xC4 : Z80ROM -> Z80 -> Z80Delta
execute_0xC4 rom48k z80 =
    -- case 0xC4: call(Fr!=0); break;
    --call_z80 (z80.flags.fr /= 0) z80
    let
        result =
            z80 |> call_if (z80.flags.fr /= 0) rom48k
    in
    EnvWithPc result.env result.pc


execute_0xC5 : Z80ROM -> Z80 -> Z80Delta
execute_0xC5 _ z80 =
    -- case 0xC5: push(B<<8|C); break;
    --z80 |> push (z80 |> get_bc)
    let
        bc =
            z80 |> get_bc

        --pushed = z80.env |> z80_push bc
    in
    --{ z80 | env = pushed }
    OnlyPush bc


execute_0xC6 : Z80ROM -> Z80 -> Z80Delta
execute_0xC6 rom48k z80 =
    -- case 0xC6: add(imm8()); break;
    let
        v =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80.flags |> z80_add v.value
    in
    --{ z80_1 | flags = flags }
    FlagsWithPcAndTime flags v.pc v.time


execute_0xC7 : Z80ROM -> Z80 -> Z80Delta
execute_0xC7 _ z80 =
    --z80 |> rst_z80 0xC7
    --let
    --   result = z80 |> rst 0xC7
    --in
    --  EnvWithPc result.env result.pc
    z80 |> rst_delta 0xC7


retz_0xC8 : Z80ROM -> Z80 -> Z80Delta
retz_0xC8 rom48k z80 =
    -- case 0xC8: time++; if(Fr==0) MP=PC=pop(); break;
    let
        z80_1_time =
            z80.env.time |> addCpuTimeTime 1

        env =
            z80.env
    in
    if z80.flags.fr == 0 then
        let
            popped =
                { env | time = z80_1_time } |> pop rom48k
        in
        --{ z80_1 | env = { env | time = popped.time, sp = popped.sp }, pc = popped.value }
        CpuTimeWithSpAndPc popped.time popped.sp popped.value

    else
        --z80_1
        OnlyTime z80_1_time


execute_0xC9 : Z80ROM -> Z80 -> Z80Delta
execute_0xC9 rom48k z80 =
    -- case 0xC9: MP=PC=pop(); break;
    let
        a =
            z80.env |> pop rom48k

        --b = debug_log "ret" (a.value |> subName) Nothing
        --env = z80.env
    in
    --{ z80 | env = { env | time = a.time, sp = a.sp }, pc = a.value }
    CpuTimeWithSpAndPc a.time a.sp a.value


execute_0xCA : Z80ROM -> Z80 -> Z80Delta
execute_0xCA rom48k z80 =
    -- case 0xCA: jp(Fr==0); break;
    --jp_z80 (z80.flags.fr == 0) z80
    --let
    --  result = z80 |> jp (z80.flags.fr == 0)
    --in
    --  CpuTimeWithPc result.time result.pc
    z80 |> jp_delta (z80.flags.fr == 0) rom48k


execute_0xCB : IXIYHL -> Z80ROM -> Z80 -> Z80Delta
execute_0xCB ixiyhl rom48k z80 =
    case ixiyhl of
        IX ->
            z80 |> group_xy_cb IXIY_IX rom48k

        IY ->
            z80 |> group_xy_cb IXIY_IY rom48k

        HL ->
            z80 |> group_cb rom48k


execute_0xCC : Z80ROM -> Z80 -> Z80Delta
execute_0xCC rom48k z80 =
    -- case 0xCC: call(Fr==0); break;
    --call_z80 (z80.flags.fr == 0) z80
    let
        result =
            z80 |> call_if (z80.flags.fr == 0) rom48k
    in
    EnvWithPc result.env result.pc


call_0xCD : Z80ROM -> Z80 -> Z80Delta
call_0xCD rom48k z80 =
    -- case 0xCD: v=imm16(); push(PC); MP=PC=v; break;
    let
        v =
            z80 |> imm16 rom48k

        --env = z80.env
        --d = debug_log "call" ("from " ++ (v.z80.pc |> toHexString) ++ " to " ++ (v.value |> subName)) Nothing
        --pushed = { env | time = v.time } |> z80_push v.pc
    in
    --{ z80_1 | env = pushed, pc = v.value }
    --EnvWithPc pushed v.value
    PushWithCpuTimeAndPc v.pc v.time v.value


execute_0xCE : Z80ROM -> Z80 -> Z80Delta
execute_0xCE rom48k z80 =
    -- case 0xCE: adc(imm8()); break;
    let
        v =
            imm8 z80.pc z80.env.time rom48k z80.env.ram

        flags =
            z80.flags |> adc v.value

        --env_1 = z80.env
    in
    --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
    FlagsWithPcAndTime flags v.pc v.time


execute_0xCF : Z80ROM -> Z80 -> Z80Delta
execute_0xCF _ z80 =
    --let
    --   result = z80 |> rst 0xCF
    --in
    --   EnvWithPc result.env result.pc
    z80 |> rst_delta 0xCF
