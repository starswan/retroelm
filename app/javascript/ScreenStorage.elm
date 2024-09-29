module ScreenStorage exposing (..)

-- Convert row index into start row data location

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import Z80Memory exposing (Z80Memory, getValue)


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


dataOffsets =
    range0_191 |> List.map calcDataOffset


range0_191 =
    List.range 0 191


type alias Z80Screen =
    { screen : Z80Memory
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


calcDataOffset : Int -> Int
calcDataOffset start =
    let
        bankStart =
            start |> Bitwise.and 0xC0

        bankOffset =
            start |> Bitwise.and 0x3F |> shiftRightBy 3

        data_offset =
            start |> Bitwise.and 0x07 |> shiftLeftBy 3
    in
    bankStart + bankOffset + data_offset


screenOffsets =
    attr_indexes |> List.indexedMap (\index attr_index -> ( calcDataOffset index, attr_index ))


range031 =
    List.range 0 31

mapScreen : ( Int, Int ) -> Z80Screen -> Int -> RawScreenData
mapScreen ( row_index, attr_index ) z80_screen index =
    let
        row_offset =
            row_index * 32

        attr_offset =
            0x1800 + (attr_index * 32)

        data =
            getScreenValue (row_offset + index) z80_screen

        colour =
            getScreenValue (attr_offset + index) z80_screen
    in
    { colour = colour, data = data }


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


rawScreenData : Z80Screen -> List (List RawScreenData)
rawScreenData z80_screen =
    screenOffsets
        |> List.map
            (\line_num ->
                range031 |> List.map (mapScreen line_num z80_screen)
            )

refresh_screen : Z80Screen -> Z80Screen
refresh_screen z80env =
    z80env
