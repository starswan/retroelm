module Z80Address exposing (..)


type Z80Address
    = ROMAddress Int
    | RAMAddress Z80WriteableAddress


type Z80WriteableAddress
    = Z80ScreenAddress Int
    | Z80MemoryAddress Int


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
