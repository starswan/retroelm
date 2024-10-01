module Z80Screen exposing (..)

import Array exposing (Array)
import Bitwise exposing (shiftRightBy)
import Byte exposing (Byte, getBit)
import Dict exposing (Dict)
import Maybe
import ScreenStorage exposing (RawScreenData, Z80Screen, rawScreenData)
import SpectrumColour exposing (SpectrumColour, spectrumColour)
import Vector32


type alias ScreenData =
    { colour : Int
    , data : List Int
    }



-- line definition - length colour (3 bits) and brightness
-- ignore flash for now


type alias ScreenColourRun =
    { start : Int
    , length : Int
    , colour : SpectrumColour
    , flash : Bool
    }


isBitSet : Byte -> Int -> Bool
isBitSet value shift =
    getBit shift value


bitsToLines : Byte -> List Bool
bitsToLines datum =
    [ 7, 6, 5, 4, 3, 2, 1, 0 ] |> List.map (isBitSet datum)


type alias RunCount =
    { start : Int
    , value : Bool
    , count : Int
    }


pairToColour : Int -> RunCount -> ScreenColourRun
pairToColour raw_colour runcount =
    let
        colour_byte =
            Byte.fromInt raw_colour

        bright =
            colour_byte |> getBit 6

        flash = (colour_byte |> getBit 7)

        colour =
            if runcount.value then
                Bitwise.and raw_colour 0x07

            else
                Bitwise.and raw_colour 0x38 |> shiftRightBy 3
    in
    ScreenColourRun runcount.start runcount.count (spectrumColour colour bright) flash


bytes0to255 =
    List.range 0 255 |> List.map Byte.fromInt



-- optimization of intToBools


intToBoolsCache : Array (List Bool)
intToBoolsCache =
    List.map bitsToLines bytes0to255 |> Array.fromList


intsToBools : List Int -> List Bool
intsToBools data =
    data
        |> List.map (\index -> Array.get index intToBoolsCache)
        |> List.map (Maybe.withDefault [])
        |> List.concat


foldRunCounts : Bool -> List RunCount -> List RunCount
foldRunCounts item list =
    case list of
        runcount :: tail ->
            if item == runcount.value then
                RunCount runcount.start runcount.value (runcount.count + 1) :: tail

            else
                RunCount (runcount.start + runcount.count) item 1 :: list

        _ ->
            [ RunCount 0 item 1 ]



-- need to work out how to filter out lines of background colour
-- This isn't quite right - the colour is the attribute value
--single_line |> List.filter (\item -> item.colour /= z80env.border)
-- screenline is start length colour (0-7) and flash


toDrawn : ScreenData -> List ScreenColourRun -> List ScreenColourRun
toDrawn screendata linelist =
    let
        newList =
            screendata.data
                |> intsToBools
                |> List.foldl foldRunCounts []
                |> List.reverse
                |> List.map (pairToColour screendata.colour)
    in
    newList :: List.singleton linelist |> List.concat



-- convert a row of data, colour into a combo of repeated blocks


foldUp : RawScreenData -> List ScreenData -> List ScreenData
foldUp raw list =
    case list of
        head :: tail ->
            if head.colour == raw.colour then
                ScreenData raw.colour (raw.data :: head.data) :: tail

            else
                ScreenData raw.colour [ raw.data ] :: list

        _ ->
            [ ScreenData raw.colour [ raw.data ] ]


screenLines : Z80Screen -> List (List ScreenColourRun)
screenLines z80_screen =
    z80_screen
        |> rawScreenData
        |> List.map (\x -> x |> List.foldr foldUp [])
        |> List.map (\x -> x |> List.foldr toDrawn [])
