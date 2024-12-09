module SingleWith8BitParameter exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement, increment3)
import Dict exposing (Dict)
import PCIncrement exposing (MediumPCIncrement(..), PCIncrement(..))
import Utils exposing (byte, shiftLeftBy8)
import Z80Flags exposing (FlagRegisters, adc, sbc, z80_add, z80_and, z80_cp, z80_or, z80_sub, z80_xor)
import Z80Types exposing (MainWithIndexRegisters, Z80)


singleWith8BitParam : Dict Int ( Int -> Single8BitChange, MediumPCIncrement )
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ( ld_b_n, IncreaseByTwo ) )
        , ( 0x0E, ( ld_c_n, IncreaseByTwo ) )
        , ( 0x16, ( ld_d_n, IncreaseByTwo ) )
        , ( 0x1E, ( ld_e_n, IncreaseByTwo ) )
        ]


doubleWithRegisters : Dict Int ((MainWithIndexRegisters -> Int -> DoubleWithRegisterChange), MediumPCIncrement)
doubleWithRegisters =
    Dict.fromList
        [ ( 0x10, (djnz, IncreaseByTwo) )
        , ( 0x36, (ld_indirect_hl_n, IncreaseByTwo) )
        , ( 0x26, ( ld_h_n, IncreaseByTwo ) )
        , ( 0xDD26, ( ld_ix_h_n, IncreaseByThree ) )
        , ( 0xFD26, ( ld_iy_h_n, IncreaseByThree ) )
        , ( 0x2E, ( ld_l_n, IncreaseByTwo ) )
        , ( 0xDD2E, ( ld_ix_l_n, IncreaseByThree ) )
        , ( 0xFD2E, ( ld_iy_l_n, IncreaseByThree ) )
        ]


maybeRelativeJump : Dict Int (Int -> FlagRegisters -> JumpChange)
maybeRelativeJump =
    Dict.fromList
        [ ( 0x18, jr_n )
        , ( 0x20, jr_nz_d )
        , ( 0x28, jr_z_d )
        , ( 0x30, jr_nc_d )
        , ( 0x38, jr_c_d )
        , ( 0x3E, ld_a_n )
        , ( 0xC6, add_a_n )
        , ( 0xCE, adc_n )
        , ( 0xD6, sub_n )
        , ( 0xDE, sbc_a_n )
        , ( 0xE6, and_n )
        , ( 0xEE, xor_n )
        , ( 0xF6, or_n )
        , ( 0xFE, cp_n )
        ]


type Single8BitChange
    = NewBRegister Int
    | NewCRegister Int
    | NewDRegister Int
    | NewERegister Int


type DoubleWithRegisterChange
    = RelativeJumpWithTimeOffset Single8BitChange (Maybe Int) Int
    | DoubleRegChangeStoreIndirect Int Int CpuTimeIncrement
    | NewHLRegisterValue Int
    | NewIXRegisterValue Int
    | NewIYRegisterValue Int


type JumpChange
    = ActualJump Int
    | NoJump
    | FlagJump FlagRegisters


applySimple8BitChange : Single8BitChange -> MainWithIndexRegisters -> MainWithIndexRegisters
applySimple8BitChange change z80_main =
    case change of
        NewBRegister int ->
            { z80_main | b = int }

        NewCRegister int ->
            { z80_main | c = int }

        NewDRegister int ->
            { z80_main | d = int }

        NewERegister int ->
            { z80_main | e = int }

ld_b_n : Int -> Single8BitChange
ld_b_n param =
    -- case 0x06: B=imm8(); break;
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    NewBRegister param


ld_c_n : Int -> Single8BitChange
ld_c_n param =
    -- case 0x0E: C=imm8(); break;
    --{ z80 | env = new_c.env, pc = new_c.pc, main = { z80_main | c = new_c.value } }
    NewCRegister param


ld_d_n : Int -> Single8BitChange
ld_d_n param =
    -- case 0x16: D=imm8(); break;
    NewDRegister param


ld_e_n : Int -> Single8BitChange
ld_e_n param =
    -- case 0x1E: E=imm8(); break;
    NewERegister param


ld_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_h_n z80_main param =
    -- case 0x26: HL=HL&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.hl 0xFF) |> NewHLRegisterValue

ld_ix_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.ix 0xFF) |> NewIXRegisterValue


ld_iy_h_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_iy_h_n z80_main param =
    -- case 0x26: xy=xy&0xFF|imm8()<<8; break;
    Bitwise.or (param |> shiftLeftBy8) (Bitwise.and z80_main.iy 0xFF) |> NewIYRegisterValue


ld_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_l_n z80_main param =
    -- case 0x2E: HL=HL&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.hl 0xFF00) |> NewHLRegisterValue


ld_ix_l_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_ix_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.ix 0xFF00) |> NewIXRegisterValue


ld_iy_l_n : MainWithIndexRegisters ->  Int -> DoubleWithRegisterChange
ld_iy_l_n z80_main param =
    -- case 0x2E: xy=xy&0xFF00|imm8(); break;
    Bitwise.or param (Bitwise.and z80_main.iy 0xFF00) |> NewIYRegisterValue


djnz : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
djnz z80_main param =
    --case 0x10: {time++; v=PC; byte d=(byte)env.mem(v++); time+=3;
    --if((B=B-1&0xFF)!=0) {time+=5; MP=v+=d;}
    --PC=(char)v;} break;
    let
        d =
            byte param

        b =
            Bitwise.and (z80_main.b - 1) 0xFF

        ( time, jump ) =
            if b /= 0 then
                ( 9, Just d )

            else
                ( 4, Nothing )
    in
    RelativeJumpWithTimeOffset (NewBRegister b) jump time


ld_indirect_hl_n : MainWithIndexRegisters -> Int -> DoubleWithRegisterChange
ld_indirect_hl_n z80_main param =
    -- case 0x36: env.mem(HL,imm8()); time+=3; break;
    -- case 0x36: {int a=(char)(xy+(byte)env.mem(PC)); time+=3;
    DoubleRegChangeStoreIndirect z80_main.hl param increment3


jr_n : Int -> FlagRegisters -> JumpChange
jr_n param _ =
    -- case 0x18: MP=PC=(char)(PC+1+(byte)env.mem(PC)); time+=8; break;
    -- This is just an inlined jr() call
    --z80 |> set_pc dest |> add_cpu_time 8
    ActualJump (byte param)


jr_nz_d : Int -> FlagRegisters -> JumpChange
jr_nz_d param z80_flags =
    -- case 0x20: if(Fr!=0) jr(); else imm8(); break;
    if z80_flags.fr /= 0 then
        ActualJump (byte param)

    else
        NoJump


jr_z_d : Int -> FlagRegisters -> JumpChange
jr_z_d param z80_flags =
    -- case 0x28: if(Fr==0) jr(); else imm8(); break;
    if z80_flags.fr == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_nc_d : Int -> FlagRegisters -> JumpChange
jr_nc_d param z80_flags =
    -- case 0x30: if((Ff&0x100)==0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 == 0 then
        ActualJump (byte param)

    else
        NoJump


jr_c_d : Int -> FlagRegisters -> JumpChange
jr_c_d param z80_flags =
    -- case 0x38: if((Ff&0x100)!=0) jr(); else imm8(); break;
    if Bitwise.and z80_flags.ff 0x0100 /= 0 then
        ActualJump (byte param)

    else
        NoJump


add_a_n : Int -> FlagRegisters -> JumpChange
add_a_n param z80_flags =
    -- case 0xC6: add(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_add param
    in
    --{ z80_1 | flags = flags }
    FlagJump flags


adc_n : Int -> FlagRegisters -> JumpChange
adc_n param z80_flags =
    -- case 0xCE: adc(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> adc param

        --env_1 = z80.env
    in
    --{z80 | pc = v.pc, env = { env_1 | time = v.time }, flags = flags }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


sub_n : Int -> FlagRegisters -> JumpChange
sub_n param z80_flags =
    -- case 0xD6: sub(imm8()); break;
    let
        --v = imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_sub param

        --env_1 = z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


sbc_a_n : Int -> FlagRegisters -> JumpChange
sbc_a_n param z80_flags =
    -- case 0xDE: sbc(imm8()); break;
    z80_flags |> sbc param |> FlagJump


and_n : Int -> FlagRegisters -> JumpChange
and_n param z80_flags =
    -- case 0xE6: and(imm8()); break;
    let
        --a =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --
        --env_1 =
        --    z80.env
        --
        --z80_1 =
        --    { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_and param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags a.pc a.time
    FlagJump flags


xor_n : Int -> FlagRegisters -> JumpChange
xor_n param z80_flags =
    -- case 0xEE: xor(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = v.time }, pc = v.pc }
        flags =
            z80_flags |> z80_xor param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags v.pc v.time
    FlagJump flags


or_n : Int -> FlagRegisters -> JumpChange
or_n param z80_flags =
    -- case 0xF6: or(imm8()); break;
    let
        --a = imm8 z80.pc z80.env.time rom48k z80.env.ram
        --env_1 = z80.env
        --z80_1 = { z80 | env = { env_1 | time = a.time }, pc = a.pc }
        flags =
            z80_flags |> z80_or param
    in
    --{ z80_1 | flags = flags }
    --FlagsWithPcAndTime flags a.pc a.time
    FlagJump flags


cp_n : Int -> FlagRegisters -> JumpChange
cp_n param z80_flags =
    -- case 0xFE: cp(imm8()); break;
    let
        --v =
        --    imm8 z80.pc z80.env.time rom48k z80.env.ram
        flags =
            z80_flags |> z80_cp param

        --env_1 =
        --    z80.env
    in
    --{ z80 | flags = flags, env = { env_1 | time = v.time }, pc = v.pc }
    FlagJump flags


ld_a_n : Int -> FlagRegisters -> JumpChange
ld_a_n param z80_flags =
    -- case 0x3E: A=imm8(); break;
    --let
    --v =
    --    imm8 z80.pc z80.env.time rom48k z80.env.ram
    --new_z80 = { z80 | env = v.env, pc = v.pc }
    --in
    --{ new_z80 | flags = { z80_flags | a = v.value } }
    --CpuTimeWithFlagsAndPc v.time { z80_flags | a = v.value } v.pc
    FlagJump { z80_flags | a = param }
