module Z80Ram exposing (..)

import Bitwise
import Utils exposing (shiftLeftBy8)
import Z80Memory exposing (Z80Memory, getValue)
import Z80Screen exposing (Z80Screen, getScreenValue)
type alias Z80Ram =
    {
        screen: Z80Screen,
        non_screen: Z80Memory
    }

c_FRSTART = -14335
c_FRTIME = 69888
--c_FRTIME = 14350

constructor: Z80Ram
constructor =
    let
       ram = List.repeat (49152 - 6912) 0
    in
        Z80Ram Z80Screen.constructor (Z80Memory.constructor ram)

getRamValue: Int -> Z80Ram -> Int
getRamValue addr z80ram =
  let
      ram_addr = addr - 6912
  in
      if ram_addr >= 0 then
         z80ram.non_screen |> getValue ram_addr
      else
         z80ram.screen |> getScreenValue addr

getRam16Value: Int -> Z80Ram -> Int
getRam16Value addr z80ram =
    let
       ram_addr = addr - 6912
       ram_addr1 = addr + 1 - 6912
    in
      let
         low = if ram_addr >= 0 then
                  z80ram.non_screen |> getValue ram_addr
               else
                  z80ram.screen.screen |> getValue addr
         high = if (ram_addr1) >= 0 then
                  z80ram.non_screen |> getValue (ram_addr1)
               else
                  z80ram.screen.screen |> getValue (addr + 1)
    in
       (Bitwise.or low (shiftLeftBy8 high))

setRamValue: Int -> Int -> Z80Ram-> Z80Ram
setRamValue addr value z80ram =
   if addr >= 6912 then
      { z80ram | non_screen = z80ram.non_screen |> Z80Memory.set_value (addr - 6912) value }
   else
      { z80ram | screen = z80ram.screen |> Z80Screen.setScreenValue addr value }

