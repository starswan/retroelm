module SingleNoParams exposing (..)

import Bitwise
import CpuTimeCTime exposing (CpuTimeCTime, addCpuTimeTime)
import Dict exposing (Dict)
import Z80Env exposing (addCpuTimeEnv, z80_pop)
import Z80Flags exposing (set_af)
import Z80Rom exposing (Z80ROM)
import Z80Types exposing (Z80, set_bc_main, set_de_main, set_iff)


type NoParamChange
    = NoOp
    | DisableInterrupts
    | EnableInterrupts
    | PopBC
    | PopDE
    | PopHL
    | PopAF
    | Exx
    | ExAfAfDash


singleWithNoParam : Dict Int NoParamChange
singleWithNoParam =
    Dict.fromList
        [ ( 0x00, NoOp )
        , ( 0x08, ExAfAfDash )

        -- case 0x40: break;
        , ( 0x40, NoOp )

        -- case 0x49: break;
        , ( 0x49, NoOp )

        -- case 0x52: break;
        , ( 0x52, NoOp )

        -- case 0x5B: break;
        , ( 0x5B, NoOp )

        -- case 0x64: break;
        , ( 0x64, NoOp )

        -- case 0x6D: break;
        , ( 0x6D, NoOp )

        -- case 0x7F: break;
        , ( 0x7F, NoOp )
        , ( 0xC1, PopBC )
        , ( 0xD1, PopDE )

        -- case 0xD9: exx(); break;
        , ( 0xD9, Exx )
        , ( 0xE1, PopHL )
        , ( 0xF1, PopAF )
        , ( 0xF3, DisableInterrupts )
        , ( 0xFB, EnableInterrupts )
        ]


applyNoParamsDelta : CpuTimeCTime -> NoParamChange -> Z80ROM -> Z80 -> Z80
applyNoParamsDelta cpu_time z80changeData rom48k z80 =
    let
        interrupts =
            z80.interrupts

        old_env =
            z80.env

        pc =
            Bitwise.and (z80.pc + 1) 0xFFFF
    in
    case z80changeData of
        NoOp ->
            { z80
                | pc = pc
                , env = { old_env | time = cpu_time |> addCpuTimeTime 4 }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        PopBC ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                --env = z80.env
                --z80_1 = { z80 | env = { env | time = v.time, sp = v.sp } }
                --x = debug_log "pop_bc" (v.value |> toHexString) Nothing
            in
            { z80
                | pc = pc
                , main = z80.main |> set_bc_main v.value
                , env = { env1 | time = v.time, sp = v.sp }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        PopHL ->
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k

                main =
                    z80.main
            in
            { z80
                | pc = pc
                , main = { main | hl = v.value }
                , env = { env1 | time = v.time, sp = v.sp }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        DisableInterrupts ->
            -- case 0xF3: IFF=0; break;
            let
                ints =
                    z80 |> set_iff 0
            in
            { z80
                | pc = pc
                , interrupts = { ints | r = interrupts.r + 1 }
            }

        EnableInterrupts ->
            -- case 0xFB: IFF=3; break;
            let
                ints =
                    z80 |> set_iff 3
            in
            { z80
                | pc = pc
                , interrupts = { ints | r = interrupts.r + 1 }
            }

        ExAfAfDash ->
            -- case 0x08: ex_af(); break;
            let
                new_z80 =
                    z80 |> ex_af
            in
            { new_z80 | pc = pc, interrupts = { interrupts | r = interrupts.r + 1 } }

        Exx ->
            -- case 0xD9: exx(); break;
            let
                main =
                    z80.main

                alt =
                    z80.alt_main
            in
            { z80
                | pc = pc
                , interrupts = { interrupts | r = interrupts.r + 1 }
                , main = { main | b = alt.b, c = alt.c, d = alt.d, e = alt.e, hl = alt.hl }
                , alt_main = { alt | b = main.b, c = main.c, d = main.d, e = main.e, hl = main.hl }
            }

        PopAF ->
            -- case 0xF1: af(pop()); break;
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k
            in
            { z80
                | pc = pc
                , flags = set_af v.value
                , env = { env1 | time = v.time, sp = v.sp }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }

        PopDE ->
            -- case 0xD1: v=pop(); D=v>>>8; E=v&0xFF; break;
            let
                env1 =
                    { old_env | time = cpu_time }

                v =
                    env1 |> z80_pop rom48k
            in
            { z80
                | pc = pc
                , main = z80.main |> set_de_main v.value
                , env = { env1 | time = v.time, sp = v.sp }
                , interrupts = { interrupts | r = interrupts.r + 1 }
            }



ex_af : Z80 -> Z80
ex_af z80 =
    -- called by Spectrum loader
    { z80 | flags = z80.alt_flags, alt_flags = z80.flags }

