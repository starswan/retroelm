module Screen exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict
import Maybe exposing (withDefault)
import Z80Env exposing (Z80Env)
import Z80Memory exposing (getValue)

-- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink
type alias RawScreenData =
   {
      colour: Int,
      data: Int
   }

type alias ScreenData =
   {
      colour: Int,
      data: List Int
   }

-- line definition - length colour (3 bits) and brightness
-- ignore flash for now
type alias ScreenLine =
   {
      start: Int,
      length: Int,
      colour: String,
      flash: Bool
   }

-- convert a row of data, colour into a combo of repeated blocks
foldUp: RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
   case (List.head list) of
      Just head ->
         if head.colour == raw.colour then
            let
               tail = withDefault [] (List.tail list)
            in
               (ScreenData raw.colour (raw.data :: head.data)) :: tail
         else
            (ScreenData raw.colour [raw.data]) :: list
      Nothing ->
         (ScreenData raw.colour [raw.data]) :: list

isBitSet: Byte -> Int -> Bool
isBitSet value shift =
   getBit shift value

bitsToLines: Byte -> List Bool
bitsToLines datum =
   [7, 6, 5, 4, 3, 2, 1, 0] |> List.map (isBitSet datum)

type alias RunCount =
   {
      start: Int,
      value: Bool,
      count: Int
   }

c_BLACK = "#000000"
c_BLUE = "#0000FF"
c_RED = "#FF0000"
c_MAGENTA = "#FF00FF"
c_GREEN = "#00FF00"
c_CYAN = "#00FFFF"
c_YELLOW = "#FFFF00"
c_WHITE = "#FFFFFF"
c_UNBRIGHT = "D7"

spectrumColours = Dict.fromList [(0, c_BLACK), (1, c_BLUE), (2, c_RED), (3, c_MAGENTA),
                                 (4, c_GREEN), (5, c_CYAN), (6,c_YELLOW), (7, c_WHITE)]

spectrumColour: Int -> Bool -> String
spectrumColour value bright =
   let
      colour = Dict.get value spectrumColours |> withDefault c_WHITE
   in
      if bright then colour else (String.replace "FF" c_UNBRIGHT colour)

pairToColour: Int -> RunCount -> ScreenLine
pairToColour raw_colour runcount =
   let
      colour_byte = Byte.fromInt raw_colour
      bright = colour_byte |> getBit 6
      colour = if runcount.value then
                  Bitwise.and raw_colour 0x07
               else
                  (Bitwise.and raw_colour 0x38 |> shiftRightBy 3)
   in
      ScreenLine runcount.start runcount.count (spectrumColour colour bright) (colour_byte |> getBit 7)

intsToBools: List Int -> List Bool
intsToBools data =
   data |> List.map Byte.fromInt |> List.map bitsToLines |> List.concat

runCounts: Bool -> List RunCount -> List RunCount
runCounts item list =
   case (List.head list) of
      Just (runcount) ->
         if item == runcount.value then
            let
               tail = (List.tail list) |> withDefault []
            in
               (RunCount runcount.start runcount.value (runcount.count + 1)) :: tail
         else
            (RunCount (runcount.start + runcount.count) item 1) :: list
      Nothing ->
         (RunCount 0 item 1) :: list

toDrawn: ScreenData -> List ScreenLine -> List ScreenLine
toDrawn screendata linelist =
   let
      new_list = screendata.data |> intsToBools |> List.foldl runCounts [] |> List.reverse |> List.map (pairToColour screendata.colour)
   in
      new_list :: List.singleton(linelist) |> List.concat

lines: List RawScreenData -> List ScreenData
lines screen =
   screen |> List.foldr foldUp []

rawToLines: List RawScreenData -> List ScreenLine
rawToLines screen =
   lines screen |> List.foldr toDrawn []

-- Convert row index into start row data location and (colour-ish) attribute memory location
calcOffsets: Int -> (Int, Int)
calcOffsets start =
   let
      bank = start // 64
      bankOffset = start |> modBy 64
      startDiv8 = bankOffset // 8
      data_offset = start |> modBy 8 |> shiftLeftBy 3
      row_index = 64 * bank + startDiv8 + data_offset
      attr_index = (bank * 8) + (bankOffset |> modBy 8)
      row_offset = row_index * 32
      attr_offset = 0x1800 + (attr_index * 32)
   in
      (row_offset, attr_offset)

mapScreen: Int -> Z80Env -> Int -> RawScreenData
mapScreen line_num z80env index  =
   let
      (row_offset, attr_offset) = calcOffsets line_num
      data = getValue (row_offset + index) z80env.ram
      colour = getValue (attr_offset + index) z80env.ram
   in
      { colour=colour,data=data }

range031 = List.range 0 31

-- line_num ranges from 0 to 255
getScreenLine: Int -> Z80Env -> List RawScreenData
getScreenLine line_num z80dict =
   List.map (mapScreen line_num z80dict) range031

rawScreenData: Z80Env -> Int -> List RawScreenData
rawScreenData z80env index =
   z80env |> getScreenLine index

range0192 = List.range 0 191

rawLines: Z80Env -> List (List RawScreenData)
rawLines z80env =
    List.map (rawScreenData z80env) range0192

screenLines: Z80Env -> List (List ScreenLine)
screenLines z80env =
    List.map rawToLines (rawLines z80env)




