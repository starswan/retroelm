module ScreenTest exposing (..)

import Bitwise
import Expect exposing (Expectation)
import Z80Screen exposing (runCounts)
import Test exposing (..)

suite : Test
suite =
   describe "things"
   [
      describe "lines"
      [
         test "without dups" <|
            \_ ->
               let
                  a = [{colour=0x70, data=0x76}]
               in
                  Expect.equal [{colour=0x70, data=[0x76]}] (a |> Z80Screen.lines)
         ,test "with dups" <|
            \_ ->
               let
                  a = [{colour=0x70, data=0x76}, {colour=0x70, data=0x71}, { colour=0x45, data=0x87}]
               in
                  Expect.equal [{colour=0x70, data=[0x76, 0x71]}, {colour=0x45, data=[0x87]}] (a |> Z80Screen.lines)
      ],
      describe "rawToLines"
      [
         test "runCounts" <|
            \_ ->
               let
                  a = [0x80, 0x03] |> Z80Screen.intsToBools
               in
                  Expect.equal [
                  {start=0,count=1,value=True},
                  {start=1,count=13,value=False},
                  {start=14,count=2,value=True}]
                  (a |> List.foldl runCounts [] |> List.reverse)
         ,test "simple" <|
            \_ ->
               let
                  -- colour is Ink 1 (bits 0-2) Paper 2 (bits 5-3) 1 dot, 7 blanks + 7 blanks, 1 dot
                  a = [{colour=Bitwise.or 0x01 0x20, data=0x80}, {colour=Bitwise.or 0x01 0x20, data=0x03}]
               in
                  Expect.equal [
                  {start=0,length=1,colour="#0000D7",flash=False},
                  {start=1,length=13, colour="#00D700", flash=False},
                  {start=14,length=2, colour="#0000D7", flash=False}]
                   (a |> Z80Screen.rawToLines)
         --,test "with dups" <|
         --   \_ ->
         --      let
         --         a = [{colour=0x70, data=0x76}, {colour=0x70, data=0x71}, { colour=0x45, data=0x87}]
         --      in
         --         Expect.equal [{colour=0x70, data=[0x76, 0x71]}, {colour=0x45, data=[0x87]}] (a |> Z80Screen.lines)
      ]
   ]
