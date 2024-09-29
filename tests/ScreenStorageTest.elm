module ScreenStorageTest exposing (..)

import Expect
import ScreenStorage exposing (screenOffsets)
import Test exposing (Test, describe, test)
import Vector24
import Vector8
suite : Test
suite =
   -- The Spectrum screen is mapped as 3 banks of 8 lines. The lines are scanned
   -- in banks of 8 so memory location 32 is at the start of line 8
   describe "things" [
      let
          numericOffsets = screenOffsets
            |> List.map (\((a, b), c) -> (8 * (a |> Vector24.indexToInt) + ((b |> Vector8.indexToInt)), c |> Vector24.indexToInt))
      in
      describe "calc indexes"
         [
         test "line 0" <|
            \_ ->
               let
                  data = numericOffsets |> List.take 8
               in
                  Expect.equal [(0,0),(8,1),(16,2),(24,3),(32,4),(40,5),(48,6),(56,7)] data
         ,test "line 8" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 8 |> List.take 8
               in
                  Expect.equal [(1,0),(9,1),(17,2),(25,3),(33,4),(41,5),(49,6),(57,7)] data
         ,test "line 16" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 16 |> List.take 8
               in
                  Expect.equal [(2,0),(10,1),(18,2),(26,3),(34,4),(42,5),(50,6),(58,7)] data
         ,test "line 24" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 24 |> List.take 8
               in
                  Expect.equal [(3,0),(11,1),(19,2),(27,3),(35,4),(43,5),(51,6),(59,7)] data
         ,test "line 32" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 32 |> List.take 8
               in
                  Expect.equal [(4,0),(12,1),(20,2),(28,3),(36,4),(44,5),(52,6),(60,7)] data
         ,test "line 40" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 40 |> List.take 8
               in
                  Expect.equal [(5,0),(13,1),(21,2),(29,3),(37,4),(45,5),(53,6),(61,7)] data
         ,test "line 48" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 48 |> List.take 8
               in
                  Expect.equal [(6,0),(14,1),(22,2),(30,3),(38,4),(46,5),(54,6),(62,7)] data
         ,test "line 56" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 56 |> List.take 8
               in
                  Expect.equal [(7,0),(15,1),(23,2),(31,3),(39,4),(47,5),(55,6),(63,7)] data
         ,test "line 64" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 64 |> List.take 8
               in
                  Expect.equal [(64,8),(72,9),(80,10),(88,11),(96,12),(104,13),(112,14),(120,15)] data
         ,test "line 128" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 128 |> List.take 8
               in
                  Expect.equal [(128,16),(136,17),(144,18),(152,19),(160,20),(168,21),(176,22),(184,23)] data
         ,test "line 191" <|
            \_ ->
               let
                  data = numericOffsets |> List.drop 184 |> List.take 8
               in
                  Expect.equal [(135,16),(143,17),(151,18),(159,19),(167,20),(175,21),(183,22),(191,23)] data
         ]
      ]