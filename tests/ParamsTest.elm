module ParamsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Params exposing (splitOnEquals, valid_params)
import Test exposing (..)

suite : Test
suite =
   describe "Parameter parsing" -- Nest as many descriptions as you like.
      [
      test "valid params" <|
         \_ ->
            let
               a = valid_params "load=x rom=y"
               sp1 = Params.StringPair "load" "x"
               sp2 = Params.StringPair "rom" "y"
            in
               Expect.equal [sp1, sp2] a
      ,test "split on equals" <|
         \_ ->
            let
               a = splitOnEquals "load=x"
               sp1 = Params.StringPair "load" "x"
            in
               Expect.equal (Just sp1) a
         ]