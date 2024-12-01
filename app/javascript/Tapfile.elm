module Tapfile exposing (..)

import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode exposing (Decoder, Step(..), andThen, fail, loop, map, map2, map3, map4, map5, string, succeed, unsignedInt16, unsignedInt8)
import Utils exposing (toPlainHexString2)
import Z80Debug exposing (debugLog)


parseTapFile : Bytes -> List Tapfile
parseTapFile bytes =
    let
        result =
            Bytes.Decode.decode (tapfileListDecoder (bytes |> width) tapfileDecoder) bytes
    in
    case result of
        Just list ->
            list

        Nothing ->
            []


type alias TapeHeaderEnd =
    { filename : String
    , blockLength : Int
    , parameter_1 : Int
    , parameter_2 : Int
    , checksum : Int
    }


type alias ProgramTapeHeader =
    { filename : String
    , blockLength : Int
    , autoStartLineNumber : Int
    , startOfVariablesArea : Int
    , checksum : Int
    }


type alias CodeTapeHeader =
    { filename : String
    , blockLength : Int
    , startOfCodeBlock : Int
    , threeTwoSevenSixEight : Int
    , checksum : Int
    }


type TapfileData
    = Program ProgramTapeHeader
    | NumberArray TapeHeaderEnd
    | CharArray TapeHeaderEnd
    | Code CodeTapeHeader


type alias TapfileHeader =
    { length : Int
    , flagByte : Int
    , data : TapfileData
    }


type alias TapfileBlock =
    { dataLength : Int
    , blockFlagByte : Int
    , data : List Int
    , checksum : Int
    }


type alias Tapfile =
    { length : Int
    , flagByte : Int
    , data : TapfileData
    , block : TapfileBlock
    }


spectrumUnsigned16Bit =
    unsignedInt16 LE


tapfileListDecoder : Int -> Decoder Tapfile -> Decoder (List Tapfile)
tapfileListDecoder len decoder =
    let
        y =
            debugLog "TAP file size" len Nothing
    in
    loop ( len, [] ) (tapfileStepDecoder decoder)


tapfileStepDecoder : Decoder Tapfile -> ( Int, List Tapfile ) -> Decoder (Step ( Int, List Tapfile ) (List Tapfile))
tapfileStepDecoder decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - x.length - 2 - (x.block.data |> List.length) - 4, x :: xs )) decoder


listWithLengthDecoder : Int -> Decoder a -> Decoder (List a)
listWithLengthDecoder len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done (xs |> List.reverse))

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder


tapFilename : TapfileData -> String
tapFilename tapFileHeader =
    case tapFileHeader of
        Program end ->
            end.filename

        NumberArray end ->
            end.filename

        CharArray end ->
            end.filename

        Code end ->
            end.filename


tapBlockLength : TapfileData -> Int
tapBlockLength tapFileHeader =
    case tapFileHeader of
        Program end ->
            end.blockLength

        NumberArray end ->
            end.blockLength

        CharArray end ->
            end.blockLength

        Code end ->
            end.blockLength


tapeheaderEndDecoder : Decoder TapeHeaderEnd
tapeheaderEndDecoder =
    map5 TapeHeaderEnd (string 10) spectrumUnsigned16Bit spectrumUnsigned16Bit spectrumUnsigned16Bit unsignedInt8


programHeaderDecoder : Decoder ProgramTapeHeader
programHeaderDecoder =
    map5 ProgramTapeHeader (string 10) spectrumUnsigned16Bit spectrumUnsigned16Bit spectrumUnsigned16Bit unsignedInt8


codeHeaderDecoder : Decoder CodeTapeHeader
codeHeaderDecoder =
    map5 CodeTapeHeader (string 10) spectrumUnsigned16Bit spectrumUnsigned16Bit spectrumUnsigned16Bit unsignedInt8


tapfileDataDecoder : Int -> Decoder TapfileData
tapfileDataDecoder headerType =
    case headerType of
        0 ->
            map Program programHeaderDecoder

        1 ->
            map NumberArray tapeheaderEndDecoder

        2 ->
            map CharArray tapeheaderEndDecoder

        3 ->
            map Code codeHeaderDecoder

        _ ->
            fail



-- programs are <line num> <length> <data of length length>
-- which might be better done using a decoder?


debugProgram : List Int -> String
debugProgram list =
    list
        |> List.map toPlainHexString2
        |> List.foldl
            (\i s ->
                if (s |> String.length) == 0 then
                    i

                else
                    s ++ " " ++ i
            )
            ""


decodeTapBody : TapfileHeader -> Decoder Tapfile
decodeTapBody tapfileheader =
    let
        x =
            debugLog "filename blocklen" ( tapfileheader.data |> tapFilename, tapfileheader.data |> tapBlockLength ) Nothing

        block_decoder =
            tapFileBlockDecoder (tapfileheader.data |> tapBlockLength)
    in
    block_decoder |> andThen (grabWholeThingDecoder tapfileheader)


grabWholeThingDecoder : TapfileHeader -> TapfileBlock -> Decoder Tapfile
grabWholeThingDecoder tapfileheader tapfile_body =
    let
        x =
            case tapfileheader.data of
                Program _ ->
                    debugLog "Program " (tapfile_body.data |> debugProgram) Nothing

                NumberArray _ ->
                    Nothing

                CharArray _ ->
                    Nothing

                Code _ ->
                    Nothing
    in
    succeed (Tapfile tapfileheader.length tapfileheader.flagByte tapfileheader.data tapfile_body)


tapfileDecoder : Decoder Tapfile
tapfileDecoder =
    decodeTapeHeader |> andThen decodeTapBody


decodeTapeHeader : Decoder TapfileHeader
decodeTapeHeader =
    let
        length =
            spectrumUnsigned16Bit

        flagByte =
            unsignedInt8

        headerType =
            unsignedInt8
    in
    map3 TapfileHeader length flagByte (headerType |> andThen tapfileDataDecoder)


tapFileBlockDecoder : Int -> Decoder TapfileBlock
tapFileBlockDecoder blockLength =
    map4 TapfileBlock spectrumUnsigned16Bit unsignedInt8 (listWithLengthDecoder blockLength unsignedInt8) unsignedInt8
