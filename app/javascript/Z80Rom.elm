module Z80Rom exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Utils exposing (listToDict, toHexString)
import Z80Debug exposing (debug_todo)
type Z80ROM = Z80ROM (Maybe (Array Int))

constructor: Z80ROM
constructor =
   let
     rom48k = List.range 0 16384
   in
     --Array.fromList rom48k
     Z80ROM Nothing

getROMValue: Int -> Z80ROM -> Int
getROMValue addr z80rom =
    case z80rom of
        Z80ROM maybe ->
            case maybe of
                Just z80dict ->
                   case Array.get addr z80dict of
                        Just item -> item
                        Nothing -> debug_todo "getROMValue" (String.fromInt addr) -1
                Nothing ->
                   debug_todo "getROMValue" (String.fromInt addr) -1

make_spectrum_rom: Array Int -> Z80ROM
make_spectrum_rom romdata =
   Z80ROM (Just romdata)

-- elm reserves First caps for types, so have to tweak names for constants
c_COMMON_NAMES = Dict.fromList [(0x11DC, "RAM-FILL"), (0x11E2, "RAM-READ"), (0xEE7, "PRB-BYTES"),
                                (0x19B8, "NEXT-ONE"),(0x15E6, "INPUT-AD"), (0x15F7, "CALL-SUB"),
                                (0x19D5, "NEXT-0-3"), (0x08F9, "ME-OLD-VP"), (0x0D76, "PO-CHAR-3"),
                                (0x0AFC, "PO-ST-PR"), (0x0C22, "PO-EACH"), (0x0B03, "PO-FETCH"),
                                (0x0B1D, "PO-F-PR"),  (0x10A8, "KEY-INPUT"), (0x15DE, "WAIT-KEY-1"),
                                (0x03D4, "BE-IX+0"),  (0x03D6, "BE-H&L-LP"),  (0x03F2, "BE-AGAIN"),
                                (0x162C, "CALL-JUMP"), (0x15FE, "0x15FE"), (0x15E1, "0x15E1"), (0x15FF, "0x15FF"),
                                (0x02BF, "KEYBOARD"), (0x028E, "KEY-SCAN"),(0x02AB, "KEY-DONE"), (0x02C2, "0x02C2"),
                                (0x02D1, "K-CH-SET"), (0x02C6, "K-ST-LOOP"), (0x031E, "K-TEST"), (0x02DB, "0x02DB"),
                                (0x0038, "MASK-INT"), (0x0048, "KEY-INT"), (0x004D, "0x004D"),
                                (0x386E, "PATCH-1?"), (0x3879, "PATCH-2?"), (0x387C, "PATCH-3?")
                                ]
c_NAMES = Dict.fromList [(0xE9B, "CL-ADDR"), (0xEDF, "CLEAR-PRB"), (0x046C, "REPORT-B"), (0x0008, "ERROR-1"),
                         (0x16DC, "INDEXER"), (0x1615, "CHAN-FLAG"), (0x12A2, "MAIN-EXEC"),
                         (0x09F4, "PRINT-OUT"),  (0x0C3B, "PO-SAVE"), (0x16B0, "SET-MIN"), (0x2D1B, "NUMERIC"),
                         (0x0DD9, "CL-SET"), (0xDEE, "CL-SET-1"), (0x11EF, "RAM-DONE"), (0x0C0A, "PO-MSG"),
                         (0x0F0C, "COPY-L-2"), (0x03B5, "BEEPER"), (0x1F05, "TEST-ROOM"), (0x110D, "KEY-NEXT"),
                         (0x167F, "PTR-DONE"), (0x1664, "POINTERS"), (0x1F54, "BREAK-KEY"),
                         (0x02A1, "KEY-BITS"),
                         (0x029F, "KEY-3KEYS"),
                         (0x15F2, "PRINT-A-2"), (0x0AD9, "PO-ABLE"), (0x0B24, "PO-ANY"),
                         (0x0B65, "PO-CHAR"), (0x12AC, "MAIN-2"), (0x187D, "OUT-LINE2"), (0x1937, "OUT-CHAR"),
                         (0x0F2C, "EDITOR"), (0x15D4, "WAIT-KEY"), (0x12A9, "MAIN-1"),
                         (0x1615, "CHAN-FLAG"), (0x1601, "CHAN-OPEN"), (0x1610, "CHAN-OP-1"),
                         (0x0EDF, "CLEAR-PRB"), (0x111D, "ED-COPY"), (0x0D4D, "TEMPS"),
                         (0x0B76, "PO-CHAR-3"), (0x0B7F, "PR-ALL"), (0x0BB6, "PR-ALL-3"), (0x0B93, "PR-ALL-1"),
                         (0x0BA4, "PR-ALL-2"), (0x0C55, "PO-SCR"), (0x0BDB, "PO-ATTR"),
                         (0x0BD3, "PR-ALL-6"), (0x0BB7, "PR-ALL-4"), (0x0BC1, "PR-ALL-5"),
                         (0x0010, "PRINT-A-1"), (0x196C, "OUT-CH-3"), (0x1195, "SET-DE"), (0x18E1, "OUT-CURS"),
                         (0x0D6B, "CLS"), (0x53, "ERROR-2"), (0x16C5,"SET-STK"), (0x11CB, "START/NEW")]

-- Look up the name of a Spectrum ROM subroutine
subName: Int -> String
subName addr =
   case (Dict.get addr c_COMMON_NAMES) of
      Just name -> name ++ " (" ++ (addr |> toHexString) ++ ")"
      Nothing -> case (Dict.get addr c_NAMES) of
                       Just name -> name ++ " (" ++ (addr |> toHexString) ++ ")"
                       Nothing -> addr |> toHexString

