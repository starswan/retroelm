module Z80Memory exposing (..)

import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Debug exposing (debug_todo)
--import Z80Memory exposing (ramarray)


type Z80Memory
    = Z80Memory Int (Dict Int Int)


type GetValueErr = EmptyAddress | OutOfBounds


constructor : List Int -> Z80Memory
constructor list =
    let
        ramarray =
            List.indexedMap Tuple.pair list
    in
    Z80Memory (List.length ramarray) (Dict.fromList ramarray)


getValue : Int -> Z80Memory -> Result GetValueErr Int
getValue addr z80mem =
    let
        size : Int
        size =
            case z80mem of
                Z80Memory int _ -> int
    in
    if addr < 0 || addr >= size then
        Err OutOfBounds
    else
        case z80mem of
            Z80Memory _ z80dict ->
                case Dict.get addr z80dict of
                    Just a ->
                        Ok a

                    Nothing -> Err EmptyAddress

                        --debug_todo ("Z80Memory:getValue " ++ (addr |> toHexString)) (Dict.size z80dict |> toHexString) -1


set_value : Int -> Int -> Z80Memory -> Z80Memory
set_value addr value z80mem =
    case z80mem of
        Z80Memory int dict ->
            dict |> Dict.insert addr value |> Z80Memory int
