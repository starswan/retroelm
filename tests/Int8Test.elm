module Int8Test exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Int8
import Test exposing (..)

suite : Test
suite =
   describe "Int8" -- Nest as many descriptions as you like.
      [ test "add" <|
         \_ ->
            1
            |> Int8.fromInt
            |> Int8.add 2
            |> Expect.equal (Int8.fromInt 3)
         ]