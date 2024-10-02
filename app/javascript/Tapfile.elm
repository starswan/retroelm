module Tapfile exposing (..)

import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode exposing (Decoder, Step(..), andThen, fail, loop, map, map2, map3, map4, map5, string, succeed, unsignedInt16, unsignedInt8)
import Z80Debug exposing (debugLog)

parseTapFile : Bytes -> List Tapfile
parseTapFile bytes =
    let
        result =
            Bytes.Decode.decode (tapfile_list_decoder (bytes |> width) tapfile_decoder) bytes
    in
    case result of
        Just list ->
            list

        Nothing ->
            []


type HeaderType
    = PROGRAM
    | NUMBER_ARRAY
    | CHAR_ARRAY
    | CODE


type alias TapeHeaderStart =
    { header_length : Int
    , flag_byte : Int
    , header_type : HeaderType
    }


type alias TapeHeaderEnd =
    { filename : String
    , block_length : Int
    , parameter_1 : Int
    , parameter_2 : Int
    , checksum : Int
    }


type alias TapfileHeader =
    { start : TapeHeaderStart
    , end : TapeHeaderEnd
    }


type alias TapfileBlock =
    { data_length : Int
    , flag_byte : Int
    , data : List Int
    , checksum : Int
    }


type alias Tapfile =
    { header : TapfileHeader
    , data : TapfileBlock
    }


spectrumUnsigned16Bit =
    unsignedInt16 LE


tapfile_list_decoder : Int -> Decoder Tapfile -> Decoder (List Tapfile)
tapfile_list_decoder len decoder =
    let
        y = debugLog "TAP file size" len Nothing
    in
    loop ( len, [] ) (tapfile_step_decoder decoder)


tapfile_step_decoder : Decoder Tapfile -> ( Int, List Tapfile ) -> Decoder (Step ( Int, List Tapfile ) (List Tapfile))
tapfile_step_decoder decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - x.header.start.header_length - 2 - (x.data.data |> List.length) - 4, x :: xs )) decoder


list_with_length_decoder : Int -> Decoder a -> Decoder (List a)
list_with_length_decoder len decoder =
    loop ( len, [] ) (listStep decoder)


listStep : Decoder a -> ( Int, List a ) -> Decoder (Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, x :: xs )) decoder


tapfile_decoder : Decoder Tapfile
tapfile_decoder =
    tapeHeader |> andThen decodeTapBody


decodeTapBody : TapfileHeader -> Decoder Tapfile
decodeTapBody tapfileheader =
    let
        x =
            debugLog "filename blocklen" ( tapfileheader.end.filename, tapfileheader.end.block_length ) Nothing

        block_decoder =
            tapFileBlock tapfileheader.end.block_length
    in
    block_decoder |> andThen (grabWholeThing tapfileheader)


grabWholeThing : TapfileHeader -> TapfileBlock -> Decoder Tapfile
grabWholeThing tapfileheader tapfile_body =
    succeed (Tapfile tapfileheader tapfile_body)


headerTypeFromInt : Int -> Decoder HeaderType
headerTypeFromInt headerType =
    let
        header_int = debugLog "Header Type" headerType headerType
    in
    case header_int of
        0 ->
            succeed PROGRAM

        1 ->
            succeed NUMBER_ARRAY

        2 ->
            succeed CHAR_ARRAY

        3 ->
            succeed CODE

        _ ->
            fail


tapeheaderStart : Decoder TapeHeaderStart
tapeheaderStart =
    map3 TapeHeaderStart spectrumUnsigned16Bit unsignedInt8 (unsignedInt8 |> andThen headerTypeFromInt)


tapeheaderEnd : Decoder TapeHeaderEnd
tapeheaderEnd =
    map5 TapeHeaderEnd (string 10) spectrumUnsigned16Bit spectrumUnsigned16Bit spectrumUnsigned16Bit unsignedInt8


tapeHeader : Decoder TapfileHeader
tapeHeader =
    map2 TapfileHeader tapeheaderStart tapeheaderEnd


tapFileBlock : Int -> Decoder TapfileBlock
tapFileBlock block_length =
    map4 TapfileBlock spectrumUnsigned16Bit unsignedInt8 (list_with_length_decoder block_length unsignedInt8) unsignedInt8
