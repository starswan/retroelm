--module Z80Address exposing (Z80Address, addIndexOffset, decrement, decrement2, fromInt, incrementBy1, incrementBy2, incrementBy3, lower8Bits, toInt, top8Bits, top8BitsWithoutShift)


module Z80Address exposing (..)

--import Z80WriteableAddress exposing (Z80WriteableAddress(..))
--type Z80Address
--    = ROMAddress Int
--    | RAMAddress Z80WriteableAddress

import Bitwise
import Utils exposing (byte, shiftRightBy8)


type Z80Address
    = ROM Int
    | Screen Int
    | Lomem Int
    | HiMem Int



--type Z80Address
--    = Z80Address Int AddressType


fromInt : Int -> Z80Address
fromInt addr =
    addr |> Bitwise.and 0xFFFF |> fromSafeInt


fromSafeInt : Int -> Z80Address
fromSafeInt addr =
    if addr <= 0x3FFF then
        addr |> ROM

    else
        let
            ramAddr =
                addr - 0x4000
        in
        if ramAddr < 6912 then
            ramAddr |> Screen

        else
            let
                memAddr =
                    ramAddr - 6912
            in
            if ramAddr <= 0x7FFF then
                memAddr |> Lomem

            else
                memAddr |> HiMem



--fromInt : Int -> Z80Address
--fromInt raw_addr =
--    let
--        ( addr, addrType ) =
--            Bitwise.and raw_addr 0xFFFF |> decodeType
--    in
--    Z80Address addr addrType
--    if addr < 0x3FFF then
--        ROMAddress addr
--    else
--        let
--            ramAddr = addr - 0x4000
--        in
--        if ramAddr <= 6912 then
--            RAMAddress (Z80ScreenAddress ramAddr)
--        else
--            RAMAddress (Z80MemoryAddress (ramAddr - 6912))
--


toInt : Z80Address -> Int
toInt z80_address =
    case z80_address of
        --Z80Address int addr_type ->
        --    case addr_type of
        ROM int ->
            int

        Screen int ->
            int + 0x4000

        Lomem int ->
            int + 0x4000 + 6912

        HiMem int ->
            int + 0x4000 + 6912



--    ROMAddress int -> int
--    RAMAddress z80WriteableAddress ->
--      case z80WriteableAddress of
--        Z80ScreenAddress int -> 0x4000 + int
--        Z80MemoryAddress int -> 0x4000 + 6912 + int
--
--incRamAddress: Z80WriteableAddress -> Z80Address
--incRamAddress address =
--    case address of
--        Z80ScreenAddress int ->
--            if int == 6912 then
--                RAMAddress (Z80MemoryAddress 0)
--
--            else
--                RAMAddress (Z80ScreenAddress (int + 1))
--
--        Z80MemoryAddress int ->
--            if int == 49152 - 6912 then
--                ROMAddress 0
--
--            else
--                RAMAddress (Z80MemoryAddress (int + 1))
--


incrementBy1 : Z80Address -> Z80Address
incrementBy1 z80_address =
    case z80_address of
        --Z80Address addr addressType ->
        --    case addressType of
        ROM addr ->
            if addr == 0x3FFF then
                0 |> Screen

            else
                (addr + 1) |> ROM

        Screen addr ->
            if addr == 6911 then
                0 |> Lomem

            else
                (addr + 1) |> Screen

        Lomem addr ->
            if addr == 49151 - 6912 then
                0 |> HiMem

            else
                (addr + 1) |> Lomem

        HiMem addr ->
            if addr == 49151 - 6912 then
                0 |> ROM

            else
                (addr + 1) |> HiMem


incrementBy2 : Z80Address -> Z80Address
incrementBy2 z80_address =
    case z80_address of
        --Z80Address addr addressType ->
        --    case addressType of
        ROM addr ->
            if addr >= 0x3FFE then
                (addr - 0x3FFE) |> Screen

            else
                (addr + 2) |> ROM

        Screen addr ->
            let
                --( new_addr, new_type ) =
                x =
                    Bitwise.and (addr + 0x4002) 0xFFFF |> fromInt
            in
            x

        Lomem addr ->
            let
                x =
                    Bitwise.and (addr + 0x4002 + 6912) 0xFFFF |> fromInt
            in
            x

        HiMem addr ->
            Bitwise.and (addr + 0x4002 + 6912) 0xFFFF |> fromInt


incrementBy3 : Z80Address -> Z80Address
incrementBy3 z80_address =
    z80_address |> incrementBy1 |> incrementBy2

incrementBy4 : Z80Address -> Z80Address
incrementBy4 z80_address =
    z80_address |> incrementBy2 |> incrementBy2



--    case z80_address of
--        ROMAddress int ->
--            if int == 0x3FFF then
--                RAMAddress (Z80ScreenAddress 0)
--
--            else
--                ROMAddress (int + 1)
--
--        RAMAddress address ->
--            case address of
--                Z80ScreenAddress int ->
--                    if int == 6912 then
--                        RAMAddress (Z80MemoryAddress 0)
--
--                    else
--                        RAMAddress (Z80ScreenAddress (int + 1))
--
--                Z80MemoryAddress int ->
--                    if int == 49152 - 6912 then
--                        ROMAddress 0
--
--                    else
--                        RAMAddress (Z80MemoryAddress (int + 1))
--


decrement2 : Z80Address -> Z80Address
decrement2 z80_address =
    --z80_address |> decrement |> decrement
    case z80_address of
        --Z80Address addr addressType ->
        --    case addressType of
        ROM addr ->
            if addr == 0 then
                0xBFFE |> HiMem

            else if addr == 1 then
                0xBFFF |> HiMem

            else
                (addr - 2) |> ROM

        Screen addr ->
            Bitwise.and (addr + 0x3FFE) 0xFFFF |> fromInt

        Lomem addr ->
            Bitwise.and (addr + 0x3FFE + 6912) 0xFFFF |> fromInt

        HiMem addr ->
            Bitwise.and (addr + 0x3FFE + 6912) 0xFFFF |> fromInt


decrement : Z80Address -> Z80Address
decrement z80_address =
    case z80_address of
        --Z80Address addr addressType ->
        --    case addressType of
        ROM addr ->
            if addr == 0 then
                (0xBFFF - 6912) |> HiMem

            else
                (addr - 1) |> ROM

        Screen addr ->
            --Bitwise.and (addr + 0x3FFF) 0xFFFF |> fromInt
            if addr == 0 then
                0x3FFF |> ROM

            else
                (addr - 1) |> Screen

        Lomem addr ->
            Bitwise.and (addr + 0x3FFF + 6912) 0xFFFF |> fromInt

        HiMem addr ->
            Bitwise.and (addr + 0x3FFF + 6912) 0xFFFF |> fromInt



--        ROMAddress int ->
--            if int == 0 then
--                RAMAddress (Z80ScreenAddress 0xBFFF)
--
--            else
--                ROMAddress (int - 1)
--
--        RAMAddress address ->
--            case address of
--                Z80ScreenAddress int ->
--                    if int == 0 then
--                        ROMAddress 0x3FFF
--
--                    else
--                        RAMAddress (Z80ScreenAddress (int - 1))
--
--                Z80MemoryAddress int ->
--                    if int == 0 then
--                        RAMAddress (Z80ScreenAddress 6911)
--
--                    else
--                        RAMAddress (Z80MemoryAddress (int - 1))
--


addIndexOffset : Int -> Z80Address -> Z80Address
addIndexOffset int z80_addr =
    (z80_addr |> toInt) + byte int |> fromInt


top8Bits : Z80Address -> Int
top8Bits z80address =
    (z80address |> toInt) |> shiftRightBy8


top8BitsWithoutShift : Z80Address -> Int
top8BitsWithoutShift z80address =
    (z80address |> toInt) |> Bitwise.and 0xFF00


lower8Bits : Z80Address -> Int
lower8Bits z80address =
    (z80address |> toInt) |> Bitwise.and 0xFF
