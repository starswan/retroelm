module MessageHandler exposing (..)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode exposing (Decoder, Step(..), loop, map, succeed)
import Http exposing (Error, Expect, Metadata, Response)
import Tapfile exposing (Tapfile, parseTapFile)
import Z80Debug exposing (debug_log)


bytesToTap : Response Bytes -> Result Error (List Tapfile)
bytesToTap httpResponse =
    case httpResponse of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            let
                -- would be nice to parse headers - but we seem to get a
                -- gzipped body size not an actual size which makes things tough
                --x = debug_log "got tap headers" metadata.headers Nothing
                --length = metadata.headers |> Dict.get "content-length" |> Maybe.withDefault "0" |> String.toInt |> Maybe.withDefault 0
                -- This seems to be the gzip size which isn't that useful
                length =
                    body |> width

                y =
                    debug_log "TAP file size" length Nothing

                --z = debug_log "received bytes of size" (body |> width) Nothing
            in
            Ok (body |> parseTapFile)



--list_decoder : Int -> Decoder Int -> Decoder (List Int)
--list_decoder size decoder =
--   loop (size, []) (listStep decoder)
--listStep : Decoder Int -> (Int, List Int) -> Decoder (Step (Int, List Int) (List Int))
--listStep decoder (n, xs) =
--   if n <= 0 then
--     succeed (Done xs)
--   else
--     map (\x -> Loop (n - 1, x :: xs)) decoder


array_decoder : Int -> Decoder Int -> Decoder (Array Int)
array_decoder size decoder =
    loop ( size, Array.empty ) (arrayStep decoder)


arrayStep : Decoder Int -> ( Int, Array Int ) -> Decoder (Step ( Int, Array Int ) (Array Int))
arrayStep decoder ( n, xs ) =
    if n <= 0 then
        succeed (Done xs)

    else
        map (\x -> Loop ( n - 1, Array.push x xs )) decoder
