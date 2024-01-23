module Z80MemoryTest exposing (..)

import Expect
import Screen exposing (calcOffsets)
import Test exposing (Test, describe, test)
suite : Test
suite =
   -- The Spectrum screen is mapped as 3 banks of 8 lines. The lines are scanned
   -- in banks of 8 so memory location 32 is at the start of line 8
   describe "things" [
      describe "calc indexes"
         [
         test "line 0" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 0
               in
                  Expect.equal (0, 6144) (data, attrs)
         ,test "line 1" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 1
               in
                  Expect.equal (256, 6176) (data, attrs)
         ,test "line 2" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 2
               in
                  Expect.equal (512, 6208) (data, attrs)
         ,test "line 3" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 3
               in
                  Expect.equal (768, 6240) (data, attrs)
         ,test "line 7" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 7
               in
                  Expect.equal (1792, 6368) (data, attrs)
         ,test "line 8" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 8
               in
                  Expect.equal (32, 6144) (data, attrs)
         ,test "line 9" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 9
               in
                  Expect.equal (256 + 32, 6176) (data, attrs)
         ,test "line 15" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 15
               in
                  Expect.equal (256*7 + 32, 6368) (data, attrs)
         ,test "line 16" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 16
               in
                  Expect.equal (64, 6144) (data, attrs)
         ,test "line 24" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 24
               in
                  Expect.equal (96, 6144) (data, attrs)
         ,test "line 32" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 32
               in
                  Expect.equal (128, 6144) (data, attrs)
         ,test "line 40" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 40
               in
                  Expect.equal (160, 6144) (data, attrs)
         ,test "line 48" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 48
               in
                  Expect.equal (192, 6144) (data, attrs)
         ,test "line 56" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 56
               in
                  Expect.equal (224, 6144) (data, attrs)
         ,test "line 64" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 64
               in
                  Expect.equal (256 * 8, 6144 + 32 * 8) (data, attrs)
         ,test "line 65" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 65
               in
                  Expect.equal (256 * 8 + 256, 6144 + 32 * 8 + 32) (data, attrs)
         ,test "line 128" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 128
               in
                  Expect.equal (256 * 16, 6144 + 32 * 16) (data, attrs)
         ,test "line 191" <|
            \_ ->
               let
                  (data, attrs) = calcOffsets 191
               in
                  Expect.equal (6112, 6880) (data, attrs)
         ]
      ]