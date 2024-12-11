module Z80Ram exposing (..)

import Bitwise
import ScreenStorage exposing (Z80Screen, getScreenValue, setScreenValue)
import Utils exposing (shiftLeftBy8)
import Z80Memory exposing (Z80Memory, getMemValue, setMemValue)


type alias Z80Ram =
    { screen : Z80Screen
    , non_screen : Z80Memory
    }


constructor : Z80Ram
constructor =
    let
        ram =
            List.repeat (49152 - 6912) 0
    in
    Z80Ram ScreenStorage.constructor (Z80Memory.constructor ram)


getRamValue : Int -> Z80Ram -> Int
getRamValue addr z80ram =
    let
        --x = debugLog "getRamValue" (Hex.toString(addr)) Nothing
        ram_addr =
            addr - 6912
    in
    if ram_addr >= 0 then
        z80ram.non_screen |> getMemValue ram_addr

    else
        z80ram.screen |> getScreenValue addr


getRam16Value : Int -> Z80Ram -> Int
getRam16Value addr z80ram =
    let
        ram_addr =
            addr - 6912

        ram_addr1 =
            addr + 1 - 6912
    in
    let
        low =
            if ram_addr >= 0 then
                z80ram.non_screen |> getMemValue ram_addr

            else
                z80ram.screen |> getScreenValue addr

        high =
            if ram_addr1 >= 0 then
                z80ram.non_screen |> getMemValue ram_addr1

            else
                z80ram.screen |> getScreenValue (addr + 1)
    in
    Bitwise.or low (shiftLeftBy8 high)


setRamValue : Int -> Int -> Z80Ram -> Z80Ram
setRamValue addr value z80ram =
    if addr >= 6912 then
        { z80ram | non_screen = z80ram.non_screen |> setMemValue (addr - 6912) value }

    else
        { z80ram | screen = z80ram.screen |> setScreenValue addr value }
