module ScreenStorage exposing (..)

-- Convert row index into start row data location

import Bitwise exposing (shiftLeftBy, shiftRightBy)
import List.Extra exposing (zip)
import Z80Memory exposing (Z80Memory, getMemValue)


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



--dataOffsets =
--    range0_191 |> List.map calcDataOffset
--range0_191 =
--    List.range 0 191


type alias Z80Screen =
    { data : Z80Memory
    , attrs : Z80Memory
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
        screen_data =
            List.repeat 6144 0 |> Z80Memory.constructor

        --for(int i=6144;i<6912;i++) ram[i] = 070; // white
        attributes =
            List.repeat 768 0x38 |> Z80Memory.constructor
    in
    Z80Screen screen_data attributes 7


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



--mapScreen : ( Int, Int ) -> Z80Screen -> Int -> RawScreenData
--mapScreen ( row_index, attr_index ) z80_screen index =
--    let
--        --row_offset =
--        --    row_index * 32
--        --
--        --attr_offset =
--        --    attr_index * 32
--        data =
--            z80_screen.data |> getMemValue (row_index * 32 + index)
--
--        colour =
--            z80_screen.attrs |> getMemValue (attr_index * 32 + index)
--    in
--    { colour = colour, data = data }


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80screen =
    --let
    --    z80screen =
    --        z80s |> refresh_screen
    --in
    if addr < 0x1800 then
        { z80screen | data = z80screen.data |> Z80Memory.setMemValue addr value }

    else
        { z80screen | attrs = z80screen.attrs |> Z80Memory.setMemValue (addr - 0x1800) value }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr screen =
    if addr < 0x1800 then
        screen.data |> getMemValue addr

    else
        screen.attrs |> getMemValue (addr - 0x1800)


rawScreenData : Z80Screen -> List (List RawScreenData)
rawScreenData z80_screen =
    screenOffsets
        |> List.map
            (\( row_index, attr_index ) ->
                --let
                --    x =
                --        range031 |> List.map (mapScreen row_column z80_screen)
                --in
                --x
                let
                    data_row =
                        range031 |> List.map (\index -> z80_screen.data |> getMemValue (row_index * 32 + index))

                    attr_row =
                        range031 |> List.map (\index -> z80_screen.attrs |> getMemValue (attr_index * 32 + index))

                    rows =
                        zip data_row attr_row
                in
                rows |> List.map (\( data, attr ) -> { colour = attr, data = data })
            )



--refresh_screen : Z80Screen -> Z80Screen
--refresh_screen z80env =
--    z80env
