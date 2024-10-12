module SingleWith8BitParameter exposing (..)

import Dict exposing (Dict)
import Z80Types exposing (Z80)


singleWith8BitParam : Dict Int (Int -> Single8BitChange)
singleWith8BitParam =
    Dict.fromList
        [ ( 0x06, ld_b_n )
        ]


type Single8BitChange
    = NewBRegister Int


applySimple8BitChange : Single8BitChange -> Z80 -> Z80
applySimple8BitChange change z80 =
    case change of
        NewBRegister int ->
            let
                main =
                    z80.main
            in
            { z80 | main = { main | b = int } }


ld_b_n : Int -> Single8BitChange
ld_b_n param =
    -- case 0x06: B=imm8(); break;
    --{ z80 | env = new_b.env, pc = new_b.pc }|> set_b new_b.value
    NewBRegister param
