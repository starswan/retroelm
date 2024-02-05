--
-- $Id$
--
module Utils exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Dict exposing (Dict)
import Hex
import Process
import Task

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

--https://stackoverflow.com/questions/40599512/how-to-achieve-behavior-of-settimeout-in-elm
--delay : Float -> msg -> Cmd msg
--delay time msg =
--    -- create a task that sleeps for `time`
--    Process.sleep time
--        |> -- once the sleep is over, ignore its output (using `always`)
--           -- and then we create a new task that simply returns a success, and the msg
--           Task.andThen (always <| Task.succeed msg)
--        |> -- finally, we ask Elm to perform the Task, which
--           -- takes the result of the above task and
--           -- returns it to our update function
--           Task.perform identity

delay : Float -> msg -> Cmd msg
delay time msg =
  Process.sleep time
  |> Task.perform (\_ -> msg)