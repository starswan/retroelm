module Z80MemoryTest exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Z80Memory exposing (calcIndexes)
suite : Test
suite =
   -- The Spectrum screen is mapped as 3 banks of 8 lines. The lines are scanned
   -- in banks of 8 so memory location 32 is at the start of line 8
   describe "things" [
      describe "calc indexes"
         [
         test "one item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 1
               in
                  Expect.equal (8, 1) (data, attrs)
         ,test "seven item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 7
               in
                  Expect.equal (56, 7) (data, attrs)
         ,test "eight item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 8
               in
                  Expect.equal (1, 0) (data, attrs)
         ,test "15 item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 15
               in
                  Expect.equal (57, 7) (data, attrs)
         ,test "63 item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 63
               in
                  Expect.equal (63, 7) (data, attrs)
         ,test "64 item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 64
               in
                  Expect.equal (64, 8) (data, attrs)
         ,test "65 item" <|
            \_ ->
               let
                  (data, attrs) = calcIndexes 65
               in
                  Expect.equal (72, 9) (data, attrs)
         ]
      ]