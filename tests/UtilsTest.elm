module UtilsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Dict exposing (Dict)
import Utils exposing (listToDict)

suite : Test
suite =
   describe "Utils"
   [
      test "listToDict" <|
         \_ ->
            let
               a = listToDict [97, 98, 99, 100, 101, 102, 103, 104]
            in
               Expect.equal (Dict.fromList [(0,97),(1,98),(2,99),(3,100),(4,101),(5,102),(6,103),(7,104)]) a
   ]