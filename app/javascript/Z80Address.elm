module Z80Address exposing (..)


import Bitwise
import Utils exposing (byte, shiftRightBy8)
import Z80WriteableAddress exposing (Z80WriteableAddress(..))
type Z80Address
    = ROMAddress Int
    | RAMAddress Z80WriteableAddress


fromInt: Int -> Z80Address
fromInt addr =
    if addr < 0x3FFF then
        ROMAddress addr
    else
        let
            ramAddr = addr - 0x4000
        in
        if ramAddr <= 6912 then
            RAMAddress (Z80ScreenAddress ramAddr)
        else
            RAMAddress (Z80MemoryAddress (ramAddr - 6912))

toInt: Z80Address -> Int
toInt z80_address =
  case z80_address of
    ROMAddress int -> int
    RAMAddress z80WriteableAddress ->
      case z80WriteableAddress of
        Z80ScreenAddress int -> 0x4000 + int
        Z80MemoryAddress int -> 0x4000 + 6912 + int

incRamAddress: Z80WriteableAddress -> Z80Address
incRamAddress address =
    case address of
        Z80ScreenAddress int ->
            if int == 6912 then
                RAMAddress (Z80MemoryAddress 0)

            else
                RAMAddress (Z80ScreenAddress (int + 1))

        Z80MemoryAddress int ->
            if int == 49152 - 6912 then
                ROMAddress 0

            else
                RAMAddress (Z80MemoryAddress (int + 1))

increment : Z80Address -> Z80Address
increment z80_address =
    case z80_address of
        ROMAddress int ->
            if int == 0x3FFF then
                RAMAddress (Z80ScreenAddress 0)

            else
                ROMAddress (int + 1)

        RAMAddress address ->
            case address of
                Z80ScreenAddress int ->
                    if int == 6912 then
                        RAMAddress (Z80MemoryAddress 0)

                    else
                        RAMAddress (Z80ScreenAddress (int + 1))

                Z80MemoryAddress int ->
                    if int == 49152 - 6912 then
                        ROMAddress 0

                    else
                        RAMAddress (Z80MemoryAddress (int + 1))

increment2: Z80Address -> Z80Address
increment2 z80_address =
  z80_address |> increment |> increment

decrement2: Z80Address -> Z80Address
decrement2 z80_address =
  z80_address |> decrement |> decrement

decrement : Z80Address -> Z80Address
decrement z80_address =
    case z80_address of
        ROMAddress int ->
            if int == 0 then
                RAMAddress (Z80ScreenAddress 0xBFFF)

            else
                ROMAddress (int - 1)

        RAMAddress address ->
            case address of
                Z80ScreenAddress int ->
                    if int == 0 then
                        ROMAddress 0x3FFF

                    else
                        RAMAddress (Z80ScreenAddress (int - 1))

                Z80MemoryAddress int ->
                    if int == 0 then
                        RAMAddress (Z80ScreenAddress 6911)

                    else
                        RAMAddress (Z80MemoryAddress (int - 1))

addIndexOffset: Int -> Z80Address -> Z80Address
addIndexOffset int z80_addr =
    (z80_addr |> toInt) + (byte int) |> fromInt

top8Bits: Z80Address -> Int
top8Bits z80address =
  (z80address |> toInt) |> shiftRightBy8

top8BitsWithoutShift: Z80Address -> Int
top8BitsWithoutShift z80address =
  (z80address |> toInt) |> Bitwise.and 0xFF00

lower8Bits: Z80Address -> Int
lower8Bits z80address =
    (z80address |> toInt) |> Bitwise.and 0xFF