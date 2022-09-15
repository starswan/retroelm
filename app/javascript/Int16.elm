--
-- $Id$
--
module Int16 exposing (..)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Int8 exposing (Int8)
type Int16 = Int16 Int

fromInt: Int -> Int16
fromInt value =
   Int16 (Bitwise.and value 0xFFFF)

fromInt8: Int8 -> Int8 -> Int16
fromInt8 int1 int2 =
   case int1 of
      (Int8 value) -> case int2 of
                        (Int8 value2) -> fromInt ((shiftLeftBy 8 value) + value2)

add : Int16 -> Int -> Int16
add int1 int2 =
   case int1 of
      (Int16 value) -> fromInt (value + int2)

shiftRightBy8: Int16 -> Int8
shiftRightBy8 reg =
   case reg of
      (Int16 value) -> Int8.fromInt (shiftRightBy 8 value)