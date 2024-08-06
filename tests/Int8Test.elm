module Int8Test exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

suite : Test
suite =
   describe "Z80Byte" -- Nest as many descriptions as you like.
      [ test "add" <|
         \_ ->
            1
            |> Z80Byte.fromInt
            |> Z80Byte.add 2
            |> Expect.equal (Z80Byte.fromInt 3)
         ]