module Z80Screen exposing (..)

import Array exposing (Array)
import Array.Extra exposing (zip)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Z80Memory exposing (Z80Memory, getValue)

type alias DataRow =
    {
       addr: Int,
       list: Array Int
    }

type alias AttributeRow =
    {
       addr: Int,
       list: List Int,
       dict: Dict Int Int
    }

type alias CharacterRow =
  {
       lineOffset: Int,
       line0: DataRow,
       line1: DataRow,
       line2: DataRow,
       line3: DataRow,
       line4: DataRow,
       line5: DataRow,
       line6: DataRow,
       line7: DataRow,
       attrs: DataRow
  }

type alias ScreenBank =
    {
       lineOffset: Int,
       attrOffset: Int,
       line0: CharacterRow,
       line1: CharacterRow,
       line2: CharacterRow,
       line3: CharacterRow,
       line4: CharacterRow,
       line5: CharacterRow,
       line6: CharacterRow,
       line7: CharacterRow,
       line_dict: Dict Int Int,
       attr_dict: Dict Int Int
    }

characterRow: Int -> Int -> CharacterRow
characterRow lineNum attrOffset =
    let
       default_data = List.repeat 32 0 |> Array.fromList
       default_attrs = List.repeat 32 0x38 |> Array.fromList

       dataOffset = lineNum * 256
       l0 = DataRow (dataOffset + 32 * 8 * 0) default_data
       l1 = DataRow (dataOffset + 32 * 8 * 1) default_data
       l2 = DataRow (dataOffset + 32 * 8 * 2) default_data
       l3 = DataRow (dataOffset + 32 * 8 * 3) default_data
       l4 = DataRow (dataOffset + 32 * 8 * 4) default_data
       l5 = DataRow (dataOffset + 32 * 8 * 5) default_data
       l6 = DataRow (dataOffset + 32 * 8 * 6) default_data
       l7 = DataRow (dataOffset + 32 * 8 * 7) default_data
       a = DataRow attrOffset default_attrs
    in
       CharacterRow lineNum l0 l1 l2 l3 l4 l5 l6 l7 a

fromOffsets: Int -> Int -> ScreenBank
fromOffsets lineNum attrOffset =
    let
       c0 = characterRow lineNum attrOffset
       c1 = characterRow (lineNum + 1) (attrOffset + 32)
       c2 = characterRow (lineNum + 2) (attrOffset + 64)
       c3 = characterRow (lineNum + 3) (attrOffset + 96)
       c4 = characterRow (lineNum + 4) (attrOffset + 128)
       c5 = characterRow (lineNum + 5) (attrOffset + 160)
       c6 = characterRow (lineNum + 6) (attrOffset + 192)
       c7 = characterRow (lineNum + 7) (attrOffset + 224)
    in
       ScreenBank lineNum attrOffset c0 c1 c2 c3 c4 c5 c6 c7 Dict.empty Dict.empty

type alias ScreenMemory =
    {
        bank1: ScreenBank,
        bank2: ScreenBank,
        bank3: ScreenBank
    }

type alias Z80Screen =
    {
        screen: ScreenMemory,
        border: Int
        --flash: Int,
        --refrs_a: Int,
        --refrs_b: Int,
        --refrs_t: Int,
        --refrs_s: Int
    }

constructor: Z80Screen
constructor =
   let
      --for(int i=6144;i<6912;i++) ram[i] = 070; // white
      -- 3 banks of 2048 (8x8x32) screen data
      --screen_data = List.repeat 6144 0
      -- 3 banks of 256 (8x32) attributes 0x5800 - 0x5AFF
      --attributes = List.repeat 768 0x38 -- white

      --screen = List.concat [screen_data, attributes] |> Z80Memory.constructor
      screen = ScreenMemory (fromOffsets 0 0x1800) (fromOffsets 8 0x1900) (fromOffsets 16 0x1A00)
   in
      --Z80Screen screen 7 0 0 0 0 0
      Z80Screen screen 7

set_screen_value: Int -> Int -> ScreenMemory -> ScreenMemory
set_screen_value addr value mem =
    let
        bank1 = mem.bank1
        bank2 = mem.bank2
        bank3 = mem.bank3
        bank3_attrs = mem.bank3.attr_dict
        bank2_attrs = mem.bank2.attr_dict
        bank1_attrs = mem.bank1.attr_dict
    in
    if addr >= mem.bank3.attrOffset then
       { mem | bank3 = { bank3 | attr_dict = bank3_attrs |> Dict.insert addr value } }
    else if addr >= mem.bank2.attrOffset then
       { mem | bank2 = { bank2 | attr_dict = bank2_attrs |>  Dict.insert addr value } }
    else if addr >= mem.bank1.attrOffset then
       { mem | bank1 = { bank1 | attr_dict = bank1_attrs |> Dict.insert addr value } }
    else if addr >= mem.bank3.lineOffset then
       { mem | bank3 = { bank3 | line_dict = mem.bank3.line_dict |> Dict.insert addr value } }
    else if addr >= mem.bank2.lineOffset then
       { mem | bank2 = { bank2 | line_dict = mem.bank2.line_dict |> Dict.insert addr value } }
    else
       { mem | bank1 = { bank1 | line_dict = mem.bank1.line_dict |> Dict.insert addr value } }

set_value: Int -> Int -> Z80Screen -> Z80Screen
set_value addr value z80s =
    let
        z80screen = z80s |> refresh_screen
    in
       { z80screen | screen = z80screen.screen |> set_screen_value addr value }

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

bytes0to255 = List.range 0 255 |> List.map Byte.fromInt

-- optimization of intToBools
intToBoolsCache: Array (List Bool)
intToBoolsCache =
    List.map bitsToLines bytes0to255 |> Array.fromList

intsToBools: List Int -> List Bool
intsToBools data =
   data |> List.map (\index -> Array.get index intToBoolsCache)
        |> List.map (Maybe.withDefault [])
        |> List.concat

runCounts: Bool -> List RunCount -> List RunCount
runCounts item list =
   case list of
       runcount :: tail ->
         if item == runcount.value then
            (RunCount runcount.start runcount.value (runcount.count + 1)) :: tail
         else
            (RunCount (runcount.start + runcount.count) item 1) :: list
       _ ->
         [RunCount 0 item 1]

-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash
toDrawn: ScreenData -> List ScreenLine -> List ScreenLine
toDrawn screendata linelist =
   let
      bools = screendata.data |> intsToBools
      run_counts = bools |> List.foldl runCounts []
      new_list = run_counts |> List.reverse |> List.map (pairToColour screendata.colour)
   in
      new_list :: List.singleton(linelist) |> List.concat

-- convert a row of data, colour into a combo of repeated blocks
foldUp: RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
   case list of
       head :: tail ->
         if head.colour == raw.colour then
            (ScreenData raw.colour (raw.data :: head.data)) :: tail
         else
            (ScreenData raw.colour [raw.data]) :: list
       _ ->
         [ScreenData raw.colour [raw.data]]

fold_lines: List RawScreenData -> List ScreenData
fold_lines screen =
   screen |> List.foldr foldUp []

rawToLines: List RawScreenData -> List ScreenLine
rawToLines screen =
   fold_lines screen |> List.foldr toDrawn []

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

range0192 = List.range 0 191

screenOffsets = range0192 |> List.map (\line_num -> calcOffsets line_num)

mapScreen: (Int, Int) -> Z80Memory -> Int -> RawScreenData
mapScreen (row_offset, attr_offset) z80env_ram index  =
   let
      data = getValue (row_offset + index) z80env_ram
      colour = getValue (attr_offset + index) z80env_ram
   in
      { colour=colour,data=data }

range031 = List.range 0 31

singleScreenLine: (Int, Int) -> Z80Memory -> List RawScreenData
singleScreenLine line_num z80env =
    List.map (mapScreen line_num z80env) range031

--screenLines: Z80Screen -> Dict Int (List ScreenLine)
--screenLines z80env =
--    let
--        rawlines = screenOffsets |> List.map (\line_num -> singleScreenLine line_num z80env.screen)
--        lines2 = List.map rawToLines rawlines
--    in
--        lines2 |> List.indexedMap (\index linelist -> (index, linelist)) |> Dict.fromList

getDataItem: Int -> Int -> Dict Int Int -> Int
getDataItem index item screenrow =
   case screenrow |> Dict.get index of
       Just a ->  a
       Nothing -> item

screenBank: ScreenBank -> Dict Int (List ScreenLine)
screenBank bank =
    let
        attrs = Array.indexedMap (\index item -> getDataItem index item bank.attr_dict) bank.line0.attrs.list
        data0 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line0.list
        data1 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line1.list
        data2 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line2.list
        data3 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line3.list
        data4 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line4.list
        data5 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line5.list
        data6 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line6.list
        data7 = Array.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.line7.list

        line0 = zip data0 attrs
        line1 = zip data1 attrs
        line2 = zip data2 attrs
        line3 = zip data3 attrs
        line4 = zip data4 attrs
        line5 = zip data5 attrs
        line6 = zip data6 attrs
        line7 = zip data7 attrs

        -- This is the first pixel row (8 lines)
        l0 = Array.map (\(data, attr) -> {colour=attr, data=data}) line0 |> Array.toList
        l1 = Array.map (\(data, attr) -> {colour=attr, data=data}) line1 |> Array.toList
        l2 = Array.map (\(data, attr) -> {colour=attr, data=data}) line2 |> Array.toList
        l3 = Array.map (\(data, attr) -> {colour=attr, data=data}) line3 |> Array.toList
        l4 = Array.map (\(data, attr) -> {colour=attr, data=data}) line4 |> Array.toList
        l5 = Array.map (\(data, attr) -> {colour=attr, data=data}) line5 |> Array.toList
        l6 = Array.map (\(data, attr) -> {colour=attr, data=data}) line6 |> Array.toList
        l7 = Array.map (\(data, attr) -> {colour=attr, data=data}) line7 |> Array.toList

        -- line key is lineOffset + n (N = 0..7)
        --x = List.map (\line -> line) [l0, l1, l2, l3, l4, l5, l6, l7]
        -- something not quite right as this should produce 64 keys not 8
        --lineOffset is 0, 32 * 8, 32 * 8 * 2, 32 * 8 * 3 etc up to 32 * 8 * 7
        -- 32 * 8 * 8 === 16 * 2 * 16 * 2 * 2 === 256 * 8 === 2048
    in
        Dict.empty
            |> Dict.insert (bank.lineOffset + 0) (rawToLines l0)
            |> Dict.insert (bank.lineOffset + 1) (rawToLines l1)
            |> Dict.insert (bank.lineOffset + 2) (rawToLines l2)
            |> Dict.insert (bank.lineOffset + 3) (rawToLines l3)
            |> Dict.insert (bank.lineOffset + 4) (rawToLines l4)
            |> Dict.insert (bank.lineOffset + 5) (rawToLines l5)
            |> Dict.insert (bank.lineOffset + 6) (rawToLines l6)
            |> Dict.insert (bank.lineOffset + 7) (rawToLines l7)

getBankAttr: ScreenBank -> Int -> Maybe Int
getBankAttr bank addr =
  let
    x = bank.attr_dict |> Dict.get addr
  in
    case x of
      Just _ -> x
      Nothing ->
        if addr >= bank.line7.attrs.addr then
          bank.line7.attrs.list |> Array.get (addr - bank.line7.attrs.addr)
        else
          Just 0

getScreenValue: Int -> ScreenMemory -> Int
getScreenValue addr screen =
  if addr >= screen.bank3.attrOffset then
    getBankAttr screen.bank3 addr |> Maybe.withDefault 0
  else if addr >= screen.bank2.attrOffset then
    getBankAttr screen.bank2 addr |> Maybe.withDefault 0
 else
    getBankAttr screen.bank1 addr |> Maybe.withDefault 0

screenLines: Z80Screen -> Dict Int (List ScreenLine)
screenLines z80env =
    let
        b1 = z80env.screen.bank1 |> screenBank
        b2 = z80env.screen.bank2 |> screenBank
        b3 = z80env.screen.bank3 |> screenBank
    in
        Dict.union (Dict.union b1 b2) b3
--private final void refresh_screen() {
--	int ft = cpu.time;
--	if(ft < refrs_t)
--		return;
--	final int flash = this.flash;
--	int a = refrs_a, b = refrs_b;
--	int t = refrs_t, s = refrs_s;
--	do {
--		int sch = 0;
--
--		int v = ram[a]<<8 | ram[b++];
--		if(v>=0x8000) v ^= flash;
--		v = canonic[v];
--		if(v!=screen[s]) {
--			screen[s] = v;
--			sch = 1;
--		}
--
--		v = ram[a+1]<<8 | ram[b++];
--		if(v>=0x8000) v ^= flash;
--		v = canonic[v];
--		if(v!=screen[++s]) {
--			screen[s] = v;
--			sch += 2;
--		}
--
--		if(sch!=0)
--			scrchg[a-0x1800>>5] |= sch<<(a&31);
--
--		a+=2; t+=8; s++;
--		if((a&31)!=0) continue;
--		// next line
--		t+=96; s+=2*Mh;
--		a-=32; b+=256-32;
--		if((b&0x700)!=0) continue;
--		// next row
--		a+=32; b+=32-0x800;
--		if((b&0xE0)!=0) continue;
--		// next segment
--		b+=0x800-256;
--		if(b>=6144) {
--			t = REFRESH_END;
--			break;
--		}
--	} while(ft >= t);
--	refrs_a = a; refrs_b = b;
--	refrs_t = t; refrs_s = s;
--}
refresh_screen: Z80Screen -> Z80Screen
refresh_screen z80env =
   z80env

