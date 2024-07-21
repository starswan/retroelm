module ScreenTest exposing (..)

import Bitwise
import Expect exposing (Expectation)
import SpectrumColour exposing (SpectrumColour(..))
import Z80Screen exposing (getScreenValue, runCounts, setScreenValue)
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
                  Expect.equal [{colour=0x70, data=[0x76]}] (a |> Z80Screen.fold_lines)
         ,test "with dups" <|
            \_ ->
               let
                  a = [{colour=0x70, data=0x76}, {colour=0x70, data=0x71}, { colour=0x45, data=0x87}]
               in
                  Expect.equal [{colour=0x70, data=[0x76, 0x71]}, {colour=0x45, data=[0x87]}] (a |> Z80Screen.fold_lines)
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
                  {start=0,length=1,colour=Blue,flash=False},
                  {start=1,length=13, colour=Green, flash=False},
                  {start=14,length=2, colour=Blue, flash=False}]
                   (a |> Z80Screen.rawToLines)
         --,test "with dups" <|
         --   \_ ->
         --      let
         --         a = [{colour=0x70, data=0x76}, {colour=0x70, data=0x71}, { colour=0x45, data=0x87}]
         --      in
         --         Expect.equal [{colour=0x70, data=[0x76, 0x71]}, {colour=0x45, data=[0x87]}] (a |> Z80Screen.lines)
      ]
      ,describe "get and set"
            [
              test "get set zero" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0 34
                  in
                      Expect.equal 34 (scr |> getScreenValue 0)
              ,test "get set 0x20" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 34 0x78
                  in
                      Expect.equal 0x78 (scr |> getScreenValue 34)
              ,test "get set 0x40" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x43 0x79
                  in
                      Expect.equal 0x79 (scr |> getScreenValue 0x43)
              ,test "get set 0x60" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x63 0x79
                  in
                      Expect.equal 0x79 (scr |> getScreenValue 0x63)
              ,test "get set 0x800" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x800 0xA3
                  in
                      Expect.equal 0xA3 (scr |> getScreenValue 0x0800)
              ,test "get set 0x1000" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x1007 0xA8
                  in
                      Expect.equal 0xA8 (scr |> getScreenValue 0x1007)
              ,test "get set 0x1800" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x1807 0xA8
                  in
                      Expect.equal 0xA8 (scr |> getScreenValue 0x1807)
              ,test "get set 0x1900" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x1907 0xA8
                  in
                      Expect.equal 0xA8 (scr |> getScreenValue 0x1907)
              ,test "get set 0x1A00" <|
                \_ ->
                  let
                      scr = Z80Screen.constructor |> setScreenValue 0x1A07 0xB8
                  in
                      Expect.equal 0xB8 (scr |> getScreenValue 0x1A07)
        ]
   ]
