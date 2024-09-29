module Z80Memory exposing (..)

import Dict exposing (Dict)
import Utils exposing (toHexString)
import Z80Debug exposing (debugTodo)


type Z80Memory
    = Z80Memory (Dict Int Int)


constructor : List Int -> Z80Memory
constructor list =
    Z80Memory (ramListToDict list)


ramListToDict : List Int -> Dict Int Int
ramListToDict list =
    let
        ramarray =
            List.indexedMap Tuple.pair list
    in
    Dict.fromList ramarray


getMemValue : Int -> Z80Memory -> Int
getMemValue addr z80mem =
    case z80mem of
        Z80Memory z80dict ->
            case Dict.get addr z80dict of
                Just a ->
                    a

                Nothing ->
                    debugTodo ("Z80Memory:getValue " ++ (addr |> toHexString)) (Dict.size z80dict |> toHexString) -1


setMemValue : Int -> Int -> Z80Memory -> Z80Memory
setMemValue addr value z80mem =
    case z80mem of
        Z80Memory dict ->
            dict |> Dict.insert addr value |> Z80Memory


-- This delivers the values in the order of the keys
getDataItems: Z80Memory -> List Int
getDataItems z80mem =
    case z80mem of
        Z80Memory dict ->
            dict |> Dict.values
