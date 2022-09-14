module Z80Memory exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)
import Screen exposing (RawScreenData)
import Utils exposing (debug_log, debug_todo, toHexString)
type alias Z80Memory =
   {
      mainDict: Dict Int Int
      --quickDict: Dict Int Int,
      --mainSize: Int,
      --opCount: Int
   }

constructor: List Int -> Z80Memory
constructor list =
   let
      ramarray = List.indexedMap Tuple.pair list
   in
      --Z80Memory (Dict.fromList ramarray) Dict.empty (List.length ramarray) 0
      Z80Memory (Dict.fromList ramarray)

-- These ideas don't seem to give us much of a boost sadly
getValue: Int -> Z80Memory -> Int
getValue addr z80dict  =
    --case Dict.get addr z80dict.quickDict of
    --    Just a ->
    --      a
    --    Nothing ->
    --      case Dict.get addr z80dict.mainDict of
    --          Just a ->
    --            a
    --          Nothing ->
    --            -1
    case Dict.get addr z80dict.mainDict of
        Just a ->
          a
        Nothing ->
          debug_todo "Z80Memory:getValue" (addr |> toHexString) -1

insert: Int -> Int -> Z80Memory -> Z80Memory
insert addr value z80mem =
   --let
   --   newq = Dict.insert addr value z80mem.quickDict
   --in
   --   if z80mem.opCount == 1024 then
   --      { z80mem | opCount = 0, quickDict = Dict.empty, mainDict = Dict.union newq z80mem.mainDict }
   --   else
   --      { z80mem | quickDict = newq, opCount = z80mem.opCount + 1 }
   { z80mem | mainDict = Dict.insert addr value z80mem.mainDict }

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

-- line_num ranges from 0 to 255
getScreenLine: Int -> Z80Memory -> List RawScreenData
getScreenLine line_num z80dict =
   let
      indexes = List.range 0 31
   in
      List.map (mapScreen line_num z80dict) indexes