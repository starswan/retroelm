--
-- $Id$
--
module Utils exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)
import Hex

compact list = List.filterMap identity list

shiftLeftBy8: Int -> Int
shiftLeftBy8 reg =
    shiftLeftBy 8 reg

shiftLeftBy1: Int -> Int
shiftLeftBy1 reg =
    shiftLeftBy 1 reg

shiftRightBy8: Int -> Int
shiftRightBy8 reg =
    shiftRightBy 8 reg

shiftRightBy1: Int -> Int
shiftRightBy1 reg =
    shiftRightBy 1 reg

listToDict: List Int -> Dict Int Int
listToDict intlist =
    let
       x = List.indexedMap Tuple.pair intlist
    in
       Dict.fromList x

digitToString: Int -> Int -> String
digitToString pad int =
   int |> String.fromInt |> String.padLeft pad '0'

byte: Int -> Int
byte value =
   if ((Bitwise.and value 0x80) /= 0) then
      value - 256
   else
      value

char: Int -> Int
char value =
   Bitwise.and value 0xFFFF

toHexString: Int -> String
toHexString value =
   "0x" ++ (Hex.toString value |> String.toUpper |> (String.padLeft 4 '0'))

-- just used for debugging
toHexString2: Int -> String
toHexString2 value =
   "0x" ++ (Hex.toString value |> String.toUpper |> (String.padLeft 2 '0'))

debug_log msg thingToLog thingToReturn =
   --Debug.log msg thingToLog |> (\_ -> thingToReturn)
   thingToReturn

debug_todo msg thingToLog thingToReturn =
   --Debug.todo (msg ++ " " ++ thingToLog) |> (\_ -> thingToReturn)
   thingToReturn




