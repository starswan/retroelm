module SingleByteWithEnv exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeIncrement(..), increment2)
import Dict exposing (Dict)
import Z80Env exposing (Z80Env)


type SingleByteEnvChange
    = NewSPValue Int CpuTimeIncrement


singleByteZ80Env : Dict Int (Z80Env -> SingleByteEnvChange)
singleByteZ80Env =
    Dict.fromList
        [ ( 0x33, inc_sp )
        , ( 0x3B, dec_sp )
        ]


inc_sp : Z80Env -> SingleByteEnvChange
inc_sp z80_env =
    -- case 0x33: SP=(char)(SP+1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp + 1) 0xFFFF
    --in
    NewSPValue (Bitwise.and (z80_env.sp + 1) 0xFFFF) increment2


dec_sp : Z80Env -> SingleByteEnvChange
dec_sp z80_env =
    -- case 0x3B: SP=(char)(SP-1); time+=2; break;
    --let
    --    new_sp =
    --        Bitwise.and (z80.env.sp - 1) 0xFFFF
    --in
    NewSPValue (Bitwise.and (z80_env.sp - 1) 0xFFFF) increment2
