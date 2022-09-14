--
-- $Id$
--
module Int8 exposing (..)

import Bitwise
type Int8 = Int8 Int

fromInt: Int -> Int8
fromInt value =
   Int8 (Bitwise.and value 0xFF)

add : Int -> Int8 -> Int8
add int int8 =
   case int8 of
      (Int8 value) -> fromInt (value + int)