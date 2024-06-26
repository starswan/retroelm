--
-- $Id$
--
module Int16 exposing (..)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Byte exposing (Byte)
import Int8 exposing (Int8)
-- low high
type Int16 = Int16 Byte Byte

fromInt: Int -> Int16
fromInt value =
    let
        low = Int8.fromInt value
        high = Int8.fromInt (value |> shiftRightBy 8)
   in
      Int16 low high

fromInt8: Int8 -> Int8 -> Int16
fromInt8 int1 int2 =
   case int1 of
      (Int8 value) -> case int2 of
                        (Int8 value2) -> fromInt ((shiftLeftBy 8 value) + value2)

add : Int16 -> Int8 -> Int16
add int16 int8 =
   case int16 of
      (Int16 low high) -> fromInt (value + int2)

shiftRightBy8: Int16 -> Int8
shiftRightBy8 reg =
   case reg of
      (Int16 low high) -> high