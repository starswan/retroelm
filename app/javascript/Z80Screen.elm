module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict exposing (Dict)
import List.Extra
import Maybe exposing (withDefault)
import Z80Memory exposing (Z80Memory, getValue)

type alias DataRow =
    {
       addr: Int,
       list: List Int
    }

type alias AttributeRow =
    {
       addr: Int,
       list: List Int,
       dict: Dict Int Int
    }

--default_attrs = List.repeat 8 (List.repeat 32 0x38)
--default_data = List.repeat 64 (List.repeat 32 0)
default_attrs = List.repeat 32 0x38
default_data = List.repeat 32 0

type alias ScreenBank =
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
       attrs: AttributeRow,
       line_dict: Dict Int Int
    }

fromOffsets: Int -> Int -> ScreenBank
fromOffsets lineNum attrOffset =
    let
       dataOffset = lineNum * 256
       l0 = DataRow dataOffset default_data
       l1 = DataRow (dataOffset + 32 * 8) default_data
       l2 = DataRow (dataOffset + 32 * 8 * 2) default_data
       l3 = DataRow (dataOffset + 32 * 8 * 3) default_data
       l4 = DataRow (dataOffset + 32 * 8 * 4) default_data
       l5 = DataRow (dataOffset + 32 * 8 * 5) default_data
       l6 = DataRow (dataOffset + 32 * 8 * 6) default_data
       l7 = DataRow (dataOffset + 32 * 8 * 7) default_data
       a = AttributeRow attrOffset default_attrs Dict.empty
    in
       ScreenBank lineNum l0 l1 l2 l3 l4 l5 l6 l7 a Dict.empty

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
        bank3_attrs = mem.bank3.attrs
        bank2_attrs = mem.bank2.attrs
        bank1_attrs = mem.bank1.attrs
    in
    if addr >= bank3_attrs.addr then
       { mem | bank3 = { bank3 | attrs = { bank3_attrs | dict = bank3_attrs.dict |> Dict.insert addr value } } }
    else if addr >= bank2_attrs.addr then
       { mem | bank2 = { bank2 | attrs = { bank2_attrs | dict = bank2_attrs.dict |>  Dict.insert addr value } } }
    else if addr >= bank1_attrs.addr then
       { mem | bank1 = { bank1 | attrs = { bank1_attrs | dict = bank1_attrs.dict |> Dict.insert addr value } } }
    else if addr >= mem.bank3.line0.addr then
       { mem | bank3 = { bank3 | line_dict = mem.bank3.line_dict |> Dict.insert addr value } }
    else if addr >= mem.bank2.line0.addr then
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
        attrs = List.indexedMap (\index item -> getDataItem index item bank.attrs.dict) bank.attrs.list
        data0 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line0.list
        data1 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line1.list
        data2 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line2.list
        data3 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line3.list
        data4 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line4.list
        data5 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line5.list
        data6 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line6.list
        data7 = List.indexedMap (\index item -> getDataItem index item bank.line_dict) bank.line7.list

        line0 = List.Extra.zip data0 attrs
        line1 = List.Extra.zip data1 attrs
        line2 = List.Extra.zip data2 attrs
        line3 = List.Extra.zip data3 attrs
        line4 = List.Extra.zip data4 attrs
        line5 = List.Extra.zip data5 attrs
        line6 = List.Extra.zip data6 attrs
        line7 = List.Extra.zip data7 attrs

        l0 = List.map (\(data, attr) -> {colour=attr, data=data}) line0
        l1 = List.map (\(data, attr) -> {colour=attr, data=data}) line1
        l2 = List.map (\(data, attr) -> {colour=attr, data=data}) line2
        l3 = List.map (\(data, attr) -> {colour=attr, data=data}) line3
        l4 = List.map (\(data, attr) -> {colour=attr, data=data}) line4
        l5 = List.map (\(data, attr) -> {colour=attr, data=data}) line5
        l6 = List.map (\(data, attr) -> {colour=attr, data=data}) line6
        l7 = List.map (\(data, attr) -> {colour=attr, data=data}) line7

        -- line key is lineOffset + n (N = 0..7)
        --x = List.map (\line -> line) [l0, l1, l2, l3, l4, l5, l6, l7]
        -- something not quite right as this should produce 64 keys not 8
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

