module Z80Memory exposing (..)

import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Byte exposing (Z80Byte)
import Z80Debug exposing (debug_todo)


type Z80Memory
    = Z80Memory (Dict Z80Byte Z80Byte)


constructor : List Z80Byte -> Z80Memory
constructor list =
    Z80Memory (ramListToDict list)


ramListToDict : List Z80Byte -> Dict Z80Byte Z80Byte
ramListToDict list =
    let
        ramarray =
            List.indexedMap Tuple.pair list
    in
    Dict.fromList ramarray


getValue : Int -> Z80Memory -> Int
getValue addr z80mem =
    case z80mem of
        Z80Memory z80dict ->
            case Dict.get addr z80dict of
                Just a ->
                    a

                Nothing ->
                    debug_todo ("Z80Memory:getValue " ++ (addr |> toHexString)) (Dict.size z80dict |> toHexString) -1


set_value : Int -> Int -> Z80Memory -> Z80Memory
set_value addr value z80mem =
    case z80mem of
        Z80Memory dict ->
            dict |> Dict.insert addr value |> Z80Memory
