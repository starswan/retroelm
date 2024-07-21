module Z80Screen exposing (..)

import Array exposing (Array)
import Array.Extra
import Bitwise exposing (shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict exposing (Dict)
import List.Extra
import Maybe exposing (withDefault)
import ScreenTypes exposing (PaperInkBrightFlash, PaperInkBrightFlashBool, RawScreenData, ScreenColourRun)
import SpectrumColour exposing (InkPaperColour(..), SpectrumColour, spectrumColours)
import Utils exposing (toHexString2)
import Z80Debug exposing (debug_todo)


type alias Z80Screen =
    { --screen: Z80Memory,
      border : Int
    , --flash: Int,
      --refrs_a: Int,
      --refrs_b: Int,
      --refrs_t: Int,
      --refrs_s: Int
      bank_0 : ScreenBank
    , bank_1 : ScreenBank
    , bank_2 : ScreenBank
    }


type alias ScreenBank =
    { charline_0 : CharLine
    , charline_1 : CharLine
    , charline_2 : CharLine
    , charline_3 : CharLine
    , charline_4 : CharLine
    , charline_5 : CharLine
    , charline_6 : CharLine
    , charline_7 : CharLine
    }


type alias CharLine =
    { line_0 : Line
    , line_1 : Line
    , line_2 : Line
    , line_3 : Line
    , line_4 : Line
    , line_5 : Line
    , line_6 : Line
    , line_7 : Line
    , attr_addr : Int
    , attrs : Array Int
    }


type alias Line =
    { start : Int
    , data : Array Int
    }


array32zeroes =
    Array.initialize 32 (always 0)



-- 0x38 === octal 070 === black ink white paper no flash or bright


array32white =
    Array.initialize 32 (always 0x38)


new_charline : Int -> Int -> CharLine
new_charline start attr_addr =
    CharLine (Line start array32zeroes)
        (Line (start + 0x0100) array32zeroes)
        (Line (start + 0x0200) array32zeroes)
        (Line (start + 0x0300) array32zeroes)
        (Line (start + 0x0400) array32zeroes)
        (Line (start + 0x0500) array32zeroes)
        (Line (start + 0x0600) array32zeroes)
        (Line (start + 0x0700) array32zeroes)
        attr_addr
        array32white


new_bank : Int -> Int -> ScreenBank
new_bank start attr_addr =
    ScreenBank (new_charline start attr_addr)
        (new_charline (start + 0x20) attr_addr)
        (new_charline (start + 0x40) attr_addr)
        (new_charline (start + 0x60) attr_addr)
        (new_charline (start + 0x80) attr_addr)
        (new_charline (start + 0xA0) attr_addr)
        (new_charline (start + 0xC0) attr_addr)
        (new_charline (start + 0xE0) attr_addr)


constructor : Z80Screen
constructor =
    --let
    --for(int i=6144;i<6912;i++) ram[i] = 070; // white
    --screen_data =
    --    List.repeat 6144 0
    --attributes =
    --    List.repeat 768 0x38
    -- white
    --screen =
    --    List.concat [ screen_data, attributes ] |> Z80Memory.constructor
    --in
    --Z80Screen screen 7 0 0 0 0 0
    -- A screen is 3 banks with data at 0x4000 etc and attrs at 0x5800 etc
    Z80Screen 7 (new_bank 0x4000 0x5800) (new_bank 0x4800 0x5900) (new_bank 0x5000 0x5A00)


set_line : Int -> Int -> Line -> Line
set_line addr_in value line =
    let
        addr =
            Bitwise.and 0x1F addr_in
    in
    { line | data = line.data |> Array.set addr value }


get_line : Int -> Line -> Int
get_line addr_in line =
    let
        addr =
            Bitwise.and 0x1F addr_in

        value =
            line.data |> Array.get addr
    in
    case value of
        Just a ->
            a

        Nothing ->
            debug_todo "get_line" (addr_in |> toHexString2) -1


set_charline : Int -> Int -> CharLine -> CharLine
set_charline addr_in value charline =
    let
        addr =
            Bitwise.and 0x0F00 addr_in
    in
    case addr of
        0x00 ->
            { charline | line_0 = charline.line_0 |> set_line addr value }

        0x0100 ->
            { charline | line_1 = charline.line_1 |> set_line addr value }

        0x0200 ->
            { charline | line_2 = charline.line_2 |> set_line addr value }

        0x0300 ->
            { charline | line_3 = charline.line_3 |> set_line addr value }

        0x0400 ->
            { charline | line_4 = charline.line_4 |> set_line addr value }

        0x0500 ->
            { charline | line_5 = charline.line_5 |> set_line addr value }

        0x0600 ->
            { charline | line_6 = charline.line_6 |> set_line addr value }

        _ ->
            { charline | line_7 = charline.line_7 |> set_line addr value }


get_charline : Int -> CharLine -> Int
get_charline addr_in charline =
    let
        addr =
            Bitwise.and 0x0F00 addr_in
    in
    case addr of
        0x00 ->
            charline.line_0 |> get_line addr

        0x0100 ->
            charline.line_1 |> get_line addr

        0x0200 ->
            charline.line_2 |> get_line addr

        0x0300 ->
            charline.line_3 |> get_line addr

        0x0400 ->
            charline.line_4 |> get_line addr

        0x0500 ->
            charline.line_5 |> get_line addr

        0x0600 ->
            charline.line_6 |> get_line addr

        _ ->
            charline.line_7 |> get_line addr


set_bank : Int -> Int -> ScreenBank -> ScreenBank
set_bank addr_in value bank =
    let
        addr =
            Bitwise.and addr_in 0x07FF

        line_addr =
            Bitwise.and addr_in 0xFF
    in
    if line_addr < 0x20 then
        { bank | charline_0 = bank.charline_0 |> set_charline addr value }

    else if line_addr < 0x40 then
        { bank | charline_1 = bank.charline_1 |> set_charline addr value }

    else if line_addr < 0x60 then
        { bank | charline_2 = bank.charline_2 |> set_charline addr value }

    else if line_addr < 0x80 then
        { bank | charline_3 = bank.charline_3 |> set_charline addr value }

    else if line_addr < 0xA0 then
        { bank | charline_4 = bank.charline_4 |> set_charline addr value }

    else if line_addr < 0xC0 then
        { bank | charline_5 = bank.charline_5 |> set_charline addr value }

    else if line_addr < 0xE0 then
        { bank | charline_6 = bank.charline_6 |> set_charline addr value }

    else
        { bank | charline_7 = bank.charline_7 |> set_charline addr value }


get_bank : Int -> ScreenBank -> Int
get_bank addr_in bank =
    let
        addr =
            Bitwise.and addr_in 0x07FF

        line_addr =
            Bitwise.and addr_in 0xFF
    in
    if line_addr < 0x20 then
        bank.charline_0 |> get_charline addr

    else if line_addr < 0x40 then
        bank.charline_1 |> get_charline addr

    else if line_addr < 0x60 then
        bank.charline_2 |> get_charline addr

    else if line_addr < 0x80 then
        bank.charline_3 |> get_charline addr

    else if line_addr < 0xA0 then
        bank.charline_4 |> get_charline addr

    else if line_addr < 0xC0 then
        bank.charline_5 |> get_charline addr

    else if line_addr < 0xE0 then
        bank.charline_6 |> get_charline addr

    else
        bank.charline_7 |> get_charline addr


set_charline_attr : Int -> Int -> CharLine -> CharLine
set_charline_attr addr_in value charline =
    let
        addr =
            Bitwise.and 0x1F addr_in
    in
    { charline | attrs = charline.attrs |> Array.set addr value }


get_charline_attr : Int -> CharLine -> Int
get_charline_attr addr_in charline =
    let
        addr =
            Bitwise.and 0x1F addr_in

        value =
            charline.attrs |> Array.get addr
    in
    case value of
        Just a ->
            a

        Nothing ->
            debug_todo "get_charline_attr" (addr_in |> toHexString2) -1


set_bank_attr : Int -> Int -> ScreenBank -> ScreenBank
set_bank_attr addr value bank =
    let
        attr_addr =
            Bitwise.and addr 0xE0
    in
    case attr_addr of
        0x00 ->
            { bank | charline_0 = bank.charline_0 |> set_charline_attr addr value }

        0x20 ->
            { bank | charline_1 = bank.charline_1 |> set_charline_attr addr value }

        0x40 ->
            { bank | charline_2 = bank.charline_2 |> set_charline_attr addr value }

        0x60 ->
            { bank | charline_3 = bank.charline_3 |> set_charline_attr addr value }

        0x80 ->
            { bank | charline_4 = bank.charline_4 |> set_charline_attr addr value }

        0xA0 ->
            { bank | charline_5 = bank.charline_5 |> set_charline_attr addr value }

        0xC0 ->
            { bank | charline_6 = bank.charline_6 |> set_charline_attr addr value }

        _ ->
            { bank | charline_7 = bank.charline_7 |> set_charline_attr addr value }


get_bank_attr : Int -> ScreenBank -> Int
get_bank_attr addr bank =
    let
        attr_addr =
            Bitwise.and addr 0xE0
    in
    case attr_addr of
        0x00 ->
            bank.charline_0 |> get_charline_attr addr

        0x20 ->
            bank.charline_1 |> get_charline_attr addr

        0x40 ->
            bank.charline_2 |> get_charline_attr addr

        0x60 ->
            bank.charline_3 |> get_charline_attr addr

        0x80 ->
            bank.charline_4 |> get_charline_attr addr

        0xA0 ->
            bank.charline_5 |> get_charline_attr addr

        0xC0 ->
            bank.charline_6 |> get_charline_attr addr

        _ ->
            bank.charline_7 |> get_charline_attr addr


setScreenValue : Int -> Int -> Z80Screen -> Z80Screen
setScreenValue addr value z80s =
    let
        z80screen =
            z80s |> refresh_screen
    in
    --{ z80screen | screen = z80screen.screen |> Z80Memory.set_value addr value }
    if addr >= 0x1A00 then
        { z80screen | bank_2 = z80screen.bank_2 |> set_bank_attr addr value }

    else if addr >= 0x1900 then
        { z80screen | bank_1 = z80screen.bank_1 |> set_bank_attr addr value }

    else if addr >= 0x1800 then
        { z80screen | bank_0 = z80screen.bank_0 |> set_bank_attr addr value }

    else if addr >= 0x1000 then
        { z80screen | bank_2 = z80screen.bank_2 |> set_bank addr value }

    else if addr >= 0x0800 then
        { z80screen | bank_1 = z80screen.bank_1 |> set_bank addr value }

    else
        { z80screen | bank_0 = z80screen.bank_0 |> set_bank addr value }


getScreenValue : Int -> Z80Screen -> Int
getScreenValue addr z80screen =
    if addr >= 0x1B00 then
        debug_todo "getScreenValue" (addr |> toHexString2) -1

    else if addr >= 0x1A00 then
        z80screen.bank_2 |> get_bank_attr addr

    else if addr >= 0x1900 then
        z80screen.bank_1 |> get_bank_attr addr

    else if addr >= 0x1800 then
        z80screen.bank_0 |> get_bank_attr addr

    else if addr >= 0x1000 then
        z80screen.bank_2 |> get_bank addr

    else if addr >= 0x0800 then
        z80screen.bank_1 |> get_bank addr

    else
        z80screen.bank_0 |> get_bank addr


isBitSet : Byte -> Int -> Bool
isBitSet value shift =
    getBit shift value


bitsToLines : Byte -> List Bool
bitsToLines datum =
    --[ 7, 6, 5, 4, 3, 2, 1, 0 ] |> List.map (isBitSet datum)
    List.range 0 7 |> List.reverse |> List.map (isBitSet datum)


bytes0to255 =
    List.range 0 255 |> List.map Byte.fromInt



-- optimization of intToBools


intToBoolsCache : Array (List Bool)
intToBoolsCache =
    List.map bitsToLines bytes0to255 |> Array.fromList



--pairToColour : Int -> RunCount -> ScreenColourRun
--pairToColour raw_colour runcount =
--    let
--        colour_byte =
--            Byte.fromInt raw_colour
--
--        bright =
--            colour_byte |> getBit 6
--
--        colour =
--            if runcount.bit then
--                Bitwise.and raw_colour 0x07
--
--            else
--                Bitwise.and raw_colour 0x38 |> shiftRightBy 3
--    in
--    ScreenColourRun runcount.start runcount.count (spectrumColour colour bright)
-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash
--foldRunCounts : Bool -> List RunCount -> List RunCount
--foldRunCounts bit list =
--    case list of
--        runcount :: tail ->
--            if bit == runcount.bit then
--                RunCount runcount.start runcount.bit (runcount.count + 1) :: tail
--
--            else
--                RunCount (runcount.start + runcount.count) bit 1 :: list
--
--        _ ->
--            [ RunCount 0 bit 1 ]
--toDrawn : ScreenData -> List ScreenColourRun -> List ScreenColourRun
--toDrawn screendata linelist =
--    let
--        bools =
--            screendata.data |> List.filterMap (\index -> Array.get index intToBoolsCache) |> List.concat
--
--        run_counts =
--            bools |> List.foldl foldRunCounts []
--
--        new_list =
--            run_counts |> List.reverse |> List.map (pairToColour screendata.colour)
--    in
--    new_list :: List.singleton linelist |> List.concat
-- convert a row of data, colour into a combo of repeated blocks
--foldUp : RawScreenData -> List ScreenData -> List ScreenData
--foldUp raw list =
--    case list of
--        head :: tail ->
--            if head.colour == raw.colour then
--                ScreenData raw.colour (raw.data :: head.data) :: tail
--
--            else
--                ScreenData raw.colour [ raw.data ] :: list
--
--        _ ->
--            [ ScreenData raw.colour [ raw.data ] ]
--fold_lines : List RawScreenData -> List ScreenData
--fold_lines screen =
--    screen |> List.foldr foldUp []


rawToPaperInkFlash : RawScreenData -> PaperInkBrightFlash
rawToPaperInkFlash raw =
    let
        inkBits =
            Bitwise.and raw.colour 0x07

        paperBits =
            Bitwise.and (raw.colour |> shiftRightBy 3) 0x07

        brightBit =
            Bitwise.and raw.colour 0x40

        flashBit =
            Bitwise.and raw.colour 0x80
    in
    PaperInkBrightFlash (Dict.get paperBits spectrumColours |> withDefault White)
        (Dict.get inkBits spectrumColours |> withDefault White)
        (brightBit /= 0)
        (flashBit /= 0)
        raw.data


foldPaperInkFlash : Bool -> PaperInkBrightFlashBool -> List ScreenColourRun -> List ScreenColourRun
foldPaperInkFlash globalFlash item list =
    let
        colour =
            if xor globalFlash item.bit then
                SpectrumColour item.ink item.bright

            else
                SpectrumColour item.paper item.bright
    in
    case list of
        head :: tail ->
            if head.colour == colour then
                { head | length = head.length + 1 } :: tail

            else
                ScreenColourRun (head.start + head.length) 1 colour :: list

        _ ->
            [ ScreenColourRun 0 1 colour ]


boolsWithAttributes : PaperInkBrightFlash -> List PaperInkBrightFlashBool
boolsWithAttributes item =
    let
        bools =
            Array.get item.dataByte intToBoolsCache |> Maybe.withDefault []
    in
    bools |> List.map (\bool -> PaperInkBrightFlashBool item.paper item.ink item.bright item.flash bool)



-- input: a single line of screen data
-- output: a list of 'lines' to draw on the screen


rawToLines : Bool -> List RawScreenData -> List ScreenColourRun
rawToLines globalFlash screen =
    --fold_lines screen |> List.foldr toDrawn []
    let
        -- List of PaperInkBrightFlash (screen attribute + data)
        a =
            screen |> List.map rawToPaperInkFlash

        boolsWithAttrs =
            a |> List.map boolsWithAttributes |> List.concat |> List.reverse

        b =
            boolsWithAttrs |> List.foldr (foldPaperInkFlash globalFlash) []
    in
    b |> List.reverse


screenLines : Z80Screen -> Dict Int (List ScreenColourRun)
screenLines screen =
    let
        --old_rawlines =
        --        screenOffsets |> List.map (\line_num -> singleScreenLine line_num screen.screen)
        rawlines =
            [ screen.bank_0, screen.bank_1, screen.bank_2 ] |> List.map bankScreenLines |> List.concat

        lines2 =
            -- default globalFlash to false for now
            List.map (rawToLines False) rawlines
    in
    --List.foldl Dict.union Dict.empty rawlines
    lines2 |> List.indexedMap (\index linelist -> ( index, linelist )) |> Dict.fromList



-- Convert row index into start row data location and (colour-ish) attribute memory location
--calcOffsets : Int -> ( Int, Int )
--calcOffsets start =
--    let
--        bank =
--            start // 64
--
--        bankOffset =
--            start |> modBy 64
--
--        startDiv8 =
--            bankOffset // 8
--
--        data_offset =
--            start |> modBy 8 |> shiftLeftBy 3
--
--        row_index =
--            64 * bank + startDiv8 + data_offset
--
--        attr_index =
--            (bank * 8) + (bankOffset |> modBy 8)
--
--        row_offset =
--            row_index * 32
--
--        attr_offset =
--            0x1800 + (attr_index * 32)
--    in
--    ( row_offset, attr_offset )
--range0192 =
--    List.range 0 191
--screenOffsets =
--    range0192 |> List.map (\line_num -> calcOffsets line_num)
--mapScreen : ( Int, Int ) -> Z80Memory -> Int -> RawScreenData
--mapScreen ( row_offset, attr_offset ) z80env_ram index =
--    let
--        data =
--            getValue (row_offset + index) z80env_ram
--
--        colour =
--            getValue (attr_offset + index) z80env_ram
--    in
--    { colour = colour, data = data }
--range031 =
--    List.range 0 31


intPairsToStruct : ( Int, Int ) -> RawScreenData
intPairsToStruct ( line, attr ) =
    { data = line, colour = attr }


lineAndAttrsToColour : ( Line, Array Int ) -> List RawScreenData
lineAndAttrsToColour ( line, attrs ) =
    let
        d =
            Array.Extra.zip line.data attrs

        colours =
            d |> Array.map intPairsToStruct
    in
    colours |> Array.toList


charLineToScreenLines : CharLine -> List (List RawScreenData)
charLineToScreenLines charline =
    let
        datalines =
            [ charline.line_0
            , charline.line_1
            , charline.line_2
            , charline.line_3
            , charline.line_4
            , charline.line_5
            , charline.line_6
            , charline.line_7
            ]

        attrlines =
            List.repeat 8 charline.attrs
    in
    List.Extra.zip datalines attrlines |> List.map lineAndAttrsToColour


bankScreenLines : ScreenBank -> List (List RawScreenData)
bankScreenLines bank =
    [ bank.charline_0
    , bank.charline_1
    , bank.charline_2
    , bank.charline_3
    , bank.charline_4
    , bank.charline_5
    , bank.charline_6
    , bank.charline_7
    ]
        |> List.map charLineToScreenLines
        |> List.concat



--singleScreenLine : ( Int, Int ) -> Z80Memory -> List RawScreenData
--singleScreenLine line_num z80env =
--    List.map (mapScreen line_num z80env) range031
--Dict.empty
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


refresh_screen : Z80Screen -> Z80Screen
refresh_screen z80env =
    z80env
