module ScreenStorage exposing (..)

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Vector24 exposing (Vector24)
import Vector32 exposing (Vector32)
import Vector8 exposing (Vector8)


range07 =
    [ Vector24.Index0
    , Vector24.Index1
    , Vector24.Index2
    , Vector24.Index3
    , Vector24.Index4
    , Vector24.Index5
    , Vector24.Index6
    , Vector24.Index7
    ]


range8_15 =
    [ Vector24.Index8
    , Vector24.Index9
    , Vector24.Index10
    , Vector24.Index11
    , Vector24.Index12
    , Vector24.Index13
    , Vector24.Index14
    , Vector24.Index15
    ]


range16_23 =
    [ Vector24.Index16
    , Vector24.Index17
    , Vector24.Index18
    , Vector24.Index19
    , Vector24.Index20
    , Vector24.Index21
    , Vector24.Index22
    , Vector24.Index23
    ]


bank0_attr_indexes =
    range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07 ++ range07


bank1_attr_indexes =
    range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15 ++ range8_15


bank2_attr_indexes =
    range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23 ++ range16_23


attr_indexes =
    bank0_attr_indexes ++ bank1_attr_indexes ++ bank2_attr_indexes


type alias RowIndex =
    { row : Vector24.Index
    , subRow : Vector8.Index
    }



--dataOffsets =
--    range0_191 |> List.map calcDataOffset


range0_191 =
    List.range 0 191


type alias ScreenRow =
    { data : Vector8 (Vector32 Int)
    , attributes : Vector32 Int
    }


type alias Z80Screen =
    { screenData : Vector24 ScreenRow
    , border : Int

    --flash: Int,
    --refrs_a: Int,
    --refrs_b: Int,
    --refrs_t: Int,
    --refrs_s: Int
    }



-- colour data is bit 7 flash, bit 6 bright, bits 5-3 paper, bits 2-0 ink


type alias RawScreenData =
    { colour : Int
    , data : Int
    }


constructor : Z80Screen
constructor =
    let
        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        --screen_data =
        --    List.repeat 6144 0 |> Z80Memory.constructor
        screenLine =
            Vector32.repeat 0

        screenVec =
            Vector8.repeat screenLine

        attrline =
            Vector32.repeat 0x38

        row =
            ScreenRow screenVec attrline

        screenData =
            Vector24.repeat row
    in
    Z80Screen screenData 7



-- Convert row index into start row data location


calcDataOffset : Int -> ( Vector24.Index, Vector8.Index )
calcDataOffset start =
    let
        bankStart =
            start |> Bitwise.and 0xC0

        bankOffset =
            start |> Bitwise.and 0x3F |> shiftRightBy 3

        data_offset =
            start |> Bitwise.and 0x07 |> shiftLeftBy 3

        offset =
            bankStart + bankOffset + data_offset

        row =
            Vector24.intToIndex (offset // 8) |> Maybe.withDefault Vector24.Index0

        subRow =
            Vector8.intToIndex (offset |> remainderBy 8) |> Maybe.withDefault Vector8.Index0
    in
    ( row, subRow )


screenOffsets =
    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))


mapScreen : ( ( Vector24.Index, Vector8.Index ), Vector24.Index ) -> Z80Screen -> Vector32.Index -> RawScreenData
mapScreen ( ( row_index, subrow_index ), attr_index ) z80_screen index =
    let
        data =
            z80_screen |> getScreenDataValue row_index subrow_index index

        colour =
            z80_screen |> getScreenAttributeValue attr_index index
    in
    { colour = colour, data = data }


attrIndex : Int -> ( Vector24.Index, Vector32.Index )
attrIndex address =
    let
        rowLine =
            address // 32 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

        colLine =
            address |> remainderBy 32 |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
    in
    ( rowLine, colLine )


dataIndex : Int -> ( Vector24.Index, Vector8.Index, Vector32.Index )
dataIndex address =
    let
        topPart =
            address // 32

        rowLine =
            topPart // 8 |> Vector24.intToIndex |> Maybe.withDefault Vector24.Index0

        midLine =
            topPart |> remainderBy 8 |> Vector8.intToIndex |> Maybe.withDefault Vector8.Index0

        col =
            (address |> remainderBy 32) |> Vector32.intToIndex |> Maybe.withDefault Vector32.Index0
    in
    ( rowLine, midLine, col )


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80screen =
    if addr < 0x1800 then
        setScreenDataValue addr value z80screen

    else
        setScreenAttributeValue (addr - 0x1800) value z80screen


setScreenDataValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenDataValue addr value z80screen =
    let
        ( rowAddress, midAddress, columnAddress ) =
            dataIndex addr

        row =
            z80screen.screenData |> Vector24.get rowAddress

        newMid =
            row.data |> Vector8.get midAddress |> Vector32.set columnAddress value

        newScreenRow =
            { row | data = row.data |> Vector8.set midAddress newMid }
    in
    { z80screen | screenData = z80screen.screenData |> Vector24.set rowAddress newScreenRow }


setScreenAttributeValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenAttributeValue addr value z80screen =
    let
        ( rowAddress, columnAddress ) =
            attrIndex addr

        row =
            z80screen.screenData |> Vector24.get rowAddress

        newScreenRow =
            { row | attributes = row.attributes |> Vector32.set columnAddress value }
    in
    { z80screen | screenData = z80screen.screenData |> Vector24.set rowAddress newScreenRow }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr screen =
    if addr < 0x1800 then
        let
            ( rowAddress, mid, column ) =
                dataIndex addr
        in
        screen |> getScreenDataValue rowAddress mid column

    else
        let
            ( rowAddress, column ) =
                attrIndex (addr - 0x1800)
        in
        screen |> getScreenAttributeValue rowAddress column


getScreenDataValue : Vector24.Index -> Vector8.Index -> Vector32.Index -> Z80Screen -> Int
getScreenDataValue rowAddress subRowIndex column screen =
    screen.screenData |> Vector24.get rowAddress |> .data |> Vector8.get subRowIndex |> Vector32.get column


getScreenAttributeValue : Vector24.Index -> Vector32.Index -> Z80Screen -> Int
getScreenAttributeValue rowAddress column screen =
    screen.screenData |> Vector24.get rowAddress |> .attributes |> Vector32.get column


rawScreenData : Z80Screen -> List (Vector32 RawScreenData)
rawScreenData z80_screen =
    screenOffsets
        |> List.map
            (\line_num ->
                Vector32.indices |> Vector32.map (mapScreen line_num z80_screen)
            )



--refresh_screen : Z80Screen -> Z80Screen
--refresh_screen z80env =
--    z80env
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
