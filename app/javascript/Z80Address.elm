module Z80Address exposing (AddressType, Z80Address, addIndexOffset, decrement, decrement2, fromInt, incrementBy1, incrementBy2, incrementBy3, incrementBy4, lower8Bits, toInt, top8Bits, top8BitsWithoutShift)

--module Z80Address exposing (..)
--import Z80WriteableAddress exposing (Z80WriteableAddress(..))
--type Z80Address
--    = ROMAddress Int
--    | RAMAddress Z80WriteableAddress

import Bitwise
import Utils exposing (byte, shiftRightBy8)


type AddressType
    = ROM
    | Screen
    | Lomem
    | HiMem


type Z80Address
    = Z80Address Int AddressType


decodeType : Int -> ( Int, AddressType )
decodeType addr =
    if addr <= 0x3FFF then
        ( addr, ROM )

    else
        let
            ramAddr =
                addr - 0x4000
        in
        if ramAddr < 6912 then
            ( ramAddr, Screen )

        else
            let
                memAddr =
                    ramAddr - 6912
            in
            if ramAddr <= 0x7FFF then
                ( memAddr, Lomem )

            else
                ( memAddr, HiMem )


fromInt : Int -> Z80Address
fromInt raw_addr =
    Bitwise.and raw_addr 0xFFFF |> fromSafeInt



-- if we know the address is in bounds, we can call this instead


fromSafeInt : Int -> Z80Address
fromSafeInt raw_addr =
    let
        ( addr, addrType ) =
            raw_addr |> decodeType
    in
    Z80Address addr addrType



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
        Z80Address int addr_type ->
            case addr_type of
                ROM ->
                    int

                Screen ->
                    int + 0x4000

                _ ->
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
        Z80Address addr addressType ->
            case addressType of
                ROM ->
                    if addr == 0x3FFF then
                        Z80Address 0 Screen

                    else
                        Z80Address (addr + 1) ROM

                Screen ->
                    if addr == 6911 then
                        Z80Address 0 Lomem

                    else
                        Z80Address (addr + 1) Screen

                Lomem ->
                    if addr == 49151 - 6912 then
                        Z80Address 0 HiMem

                    else
                        Z80Address (addr + 1) Lomem

                HiMem ->
                    if addr == 49151 - 6912 then
                        Z80Address 0 ROM

                    else
                        Z80Address (addr + 1) HiMem


incrementBy2 : Z80Address -> Z80Address
incrementBy2 z80_address =
    case z80_address of
        Z80Address addr addressType ->
            case addressType of
                ROM ->
                    if addr >= 0x3FFE then
                        Z80Address (addr - 0x3FFE) Screen

                    else
                        Z80Address (addr + 2) ROM

                Screen ->
                    if addr >= 6910 then
                        Z80Address (addr - 6910) Lomem

                    else
                        Z80Address (addr + 2) Screen

                Lomem ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x4002 + 6912) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type

                HiMem ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x4002 + 6912) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type


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
        Z80Address addr addressType ->
            case addressType of
                ROM ->
                    if addr == 0 then
                        Z80Address 0xBFFE HiMem

                    else if addr == 1 then
                        Z80Address 0xBFFF HiMem

                    else
                        Z80Address (addr - 2) ROM

                Screen ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x3FFE) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type

                _ ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x3FFE + 6912) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type


decrement : Z80Address -> Z80Address
decrement z80_address =
    case z80_address of
        Z80Address addr addressType ->
            case addressType of
                ROM ->
                    if addr == 0 then
                        Z80Address (0xBFFF - 6912) HiMem

                    else
                        Z80Address (addr - 1) ROM

                Screen ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x3FFF) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type

                _ ->
                    let
                        ( new_addr, new_type ) =
                            Bitwise.and (addr + 0x3FFF + 6912) 0xFFFF |> decodeType
                    in
                    Z80Address new_addr new_type



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
