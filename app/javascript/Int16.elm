--
-- $Id$
--
module Int16 exposing (..)
--import Bitwise exposing (shiftLeftBy, shiftRightBy)
--import Byte exposing (Byte, toInt)
--import Z80Byte exposing (Z80Byte)
---- low high
--type Int16 = Int16 Byte Byte
--
--fromInt: Int -> Int16
--fromInt value =
--    let
--        low = Z80Byte.fromInt value
--        high = Z80Byte.fromInt (value |> shiftRightBy 8)
--   in
--      Int16 low high
--
--fromInt8: Z80Byte -> Z80Byte -> Int16
--fromInt8 int1 int2 =
--   case int1 of
--      (Z80Byte value) -> case int2 of
--                        (Z80Byte value2) -> fromInt ((shiftLeftBy 8 (toInt value)) + toInt value2)
--
--add : Int16 -> Z80Byte -> Int16
--add int16 int8 =
--   case int16 of
--      (Int16 low high) -> fromInt (value + int2)
--
--shiftRightBy8: Int16 -> Z80Byte
--shiftRightBy8 reg =
--   case reg of
--      (Int16 low high) -> Z80Byte high