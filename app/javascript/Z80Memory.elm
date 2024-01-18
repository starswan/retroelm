module Z80Memory exposing (..)

import Bitwise exposing (shiftLeftBy)
import Dict exposing (Dict)
import Screen exposing (RawScreenData)
import Utils exposing (toHexString)
import Z80Debug exposing (debug_todo)
type alias Z80Memory =
   {
      mainDict: Dict Int Int
   }

constructor: List Int -> Z80Memory
constructor list =
   let
      ramarray = List.indexedMap Tuple.pair list
   in
      Z80Memory (Dict.fromList ramarray)

getValue: Int -> Z80Memory -> Int
getValue addr z80dict  =
    case Dict.get addr z80dict.mainDict of
        Just a ->
          a
        Nothing ->
          debug_todo "Z80Memory:getValue" (addr |> toHexString) -1

-- insert value at address addr (except that 16384 has been already subtracted)
set_value: Int -> Int -> Z80Memory -> Z80Memory
set_value addr value z80mem =
   { z80mem | mainDict = Dict.insert addr value z80mem.mainDict }

-- Convert row index into start row data location and (colour-ish) attribute memory location
calcIndexes: Int -> (Int, Int)
calcIndexes start =
   let
      bank = start // 64
      bankOffset = start |> modBy 64
      startDiv8 = bankOffset // 8
      data_offset = start |> modBy 8 |> shiftLeftBy 3
   in
      (64 * bank + startDiv8 + data_offset, (bank * 8) + (bankOffset |> modBy 8))

mapScreen: Int -> Z80Memory -> Int -> RawScreenData
mapScreen line_num z80mem index  =
   let
      --y = debug_log "mapScreen of" index Nothing
      (row_index, attr_index) = calcIndexes line_num
      --x = debug_log "mapScreen 2" (row_index, attr_index) Nothing
      data = getValue (row_index * 32 + index) z80mem
      colour = getValue (0x1800 + (attr_index * 32) + index) z80mem
   in
      { colour=colour,data=data }

range031 = (List.range 0 31)

-- line_num ranges from 0 to 255
getScreenLine: Int -> Z80Memory -> List RawScreenData
getScreenLine line_num z80dict =
   List.map (mapScreen line_num z80dict) range031