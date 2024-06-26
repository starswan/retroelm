--
-- $Id$
--
module Int8 exposing (..)

import Bitwise
import Byte exposing (Byte)
type Int8 = Int8 Byte

fromInt: Int -> Int8
fromInt value =
   Int8 (Byte.fromInt value)

add : Int -> Int8 -> Int8
add int int8 =
   case int8 of
      (Int8 value) -> fromInt (value + int)