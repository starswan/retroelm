module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict exposing (Dict)
import Maybe
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Z80Memory exposing (Z80Memory, getValue)


type alias Z80Screen =
    { screen : Z80Memory
    , border : Int

    --flash: Int,
    --refrs_a: Int,
    --refrs_b: Int,
    --refrs_t: Int,
    --refrs_s: Int
    }


constructor : Z80Screen
constructor =
    let
        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        screen_data =
            List.repeat 6144 0

        attributes =
            List.repeat 768 0x38

        -- white
        screen =
            List.concat [ screen_data, attributes ] |> Z80Memory.constructor
    in
    --Z80Screen screen 7 0 0 0 0 0
    Z80Screen screen 7


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80s =
    let
        z80screen =
            z80s |> refresh_screen
    in
    { z80screen | screen = z80screen.screen |> Z80Memory.set_value addr value }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr screen =
    screen.screen |> getValue addr



-- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink


type alias RawScreenData =
    { colour : Int
    , data : Int
    }


type alias ScreenData =
    { colour : Int
    , data : List Int
    }



-- line definition - length colour (3 bits) and brightness
-- ignore flash for now


type alias ScreenColourRun =
    { start : Int
    , length : Int
    , colour : SpectrumColour
    , flash : Bool
    }


isBitSet : Byte -> Int -> Bool
isBitSet value shift =
    getBit shift value


bitsToLines : Byte -> List Bool
bitsToLines datum =
    [ 7, 6, 5, 4, 3, 2, 1, 0 ] |> List.map (isBitSet datum)


type alias RunCount =
    { start : Int
    , value : Bool
    , count : Int
    }


pairToColour : Int -> RunCount -> ScreenColourRun
pairToColour raw_colour runcount =
    let
        colour_byte =
            Byte.fromInt raw_colour

        bright =
            colour_byte |> getBit 6

        colour =
            if runcount.value then
                Bitwise.and raw_colour 0x07

            else
                Bitwise.and raw_colour 0x38 |> shiftRightBy 3
    in
    ScreenColourRun runcount.start runcount.count (spectrumColour colour bright) (colour_byte |> getBit 7)


bytes0to255 =
    List.range 0 255 |> List.map Byte.fromInt



-- optimization of intToBools


intToBoolsCache : Array (List Bool)
intToBoolsCache =
    List.map bitsToLines bytes0to255 |> Array.fromList


intsToBools : List Int -> List Bool
intsToBools data =
    data
        |> List.map (\index -> Array.get index intToBoolsCache)
        |> List.map (Maybe.withDefault [])
        |> List.concat


runCounts : Bool -> List RunCount -> List RunCount
runCounts item list =
    case list of
        runcount :: tail ->
            if item == runcount.value then
                RunCount runcount.start runcount.value (runcount.count + 1) :: tail

            else
                RunCount (runcount.start + runcount.count) item 1 :: list

        _ ->
            [ RunCount 0 item 1 ]



-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


toDrawn : ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn screendata linelist =
    let
        bools =
            screendata.data |> intsToBools

        run_counts =
            bools |> List.foldl runCounts []

        new_list =
            run_counts |> List.reverse |> List.map (pairToColour screendata.colour)
    in
    new_list :: List.singleton linelist |> List.concat



-- convert a row of data, colour into a combo of repeated blocks


foldUp : RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
    case list of
        head :: tail ->
            if head.colour == raw.colour then
                ScreenData raw.colour (raw.data :: head.data) :: tail

            else
                ScreenData raw.colour [ raw.data ] :: list

        _ ->
            [ ScreenData raw.colour [ raw.data ] ]


fold_lines : List RawScreenData -> List ScreenData
fold_lines screen =
    screen |> List.foldr foldUp []


rawToLines : List RawScreenData -> List ScreenColourRun
rawToLines screen =
    fold_lines screen |> List.foldr toDrawn []



-- Convert row index into start row data location and (colour-ish) attribute memory location


calcDataOffset : Int -> Int
calcDataOffset start =
    let
        bankStart =
            start |> Bitwise.and 0xC0

        offset =
            start |> modBy 64

        bankOffset =
            offset // 8

        data_offset =
            start |> modBy 8 |> shiftLeftBy 3
    in
    bankStart + bankOffset + data_offset


range07 =
    List.range 0 7


range8_15 =
    List.range 8 15


range16_23 =
    List.range 16 23


bank0_attr_indexes =
    range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07


bank1_attr_indexes =
    range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15


bank2_attr_indexes =
    range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23


attr_indexes =
    bank0_attr_indexes ++ bank1_attr_indexes ++ bank2_attr_indexes


screenOffsets =
    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))


mapScreen : ( Int, Int ) -> Z80Memory -> Int -> RawScreenData
mapScreen ( row_index, attr_index ) z80env_ram index =
    let
        row_offset =
            row_index * 32

        attr_offset =
            0x1800 + (attr_index * 32)

        data =
            getValue (row_offset + index) z80env_ram

        colour =
            getValue (attr_offset + index) z80env_ram
    in
    { colour = colour, data = data }


range031 =
    List.range 0 31


singleScreenLine : ( Int, Int ) -> Z80Memory -> List RawScreenData
singleScreenLine line_num z80env =
    List.map (mapScreen line_num z80env) range031


screenLines : Z80Screen -> Dict Int (List ScreenColourRun)
screenLines z80env =
    let
        rawlines =
            screenOffsets |> List.map (\line_num -> singleScreenLine line_num z80env.screen)

        lines2 =
            List.map rawToLines rawlines
    in
    lines2 |> List.indexedMap (\index linelist -> ( index, linelist )) |> Dict.fromList



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


refresh_screen : Z80Screen -> Z80Screen
refresh_screen z80env =
    z80env
