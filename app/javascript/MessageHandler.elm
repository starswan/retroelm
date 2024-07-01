module MessageHandler exposing (..)

import Array exposing (Array)
import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode exposing (Decoder, Step(..), loop, map, succeed, unsignedInt8)
import Char
import Http exposing (Error, Expect, Metadata, Response)
import Http.Detailed
import Loader exposing (LoadAction(..), Loader)
import String
import Tapfile exposing (Tapfile, parseTapFile)
import Time
import Z80Debug exposing (debug_log)

type Message
  = GotTAP (Result Http.Error (List Tapfile))
  | GotRom (Result (Http.Detailed.Error Bytes) (Metadata, Array Int))
  | Tick Time.Posix
  | Pause
  | CharacterKey Char
  | CharacterUnKey Char
  | ControlKeyDown String
  | ControlUnKey String
  | KeyRepeat
  | LoadTape

--loadRom: String -> Cmd Message
--loadRom fileName =
--    Http.get { url = String.concat ["http://localhost:3000/", fileName],
--               expect = Http.expectBytes GotRom (list_decoder 16384 unsignedInt8)
--              }
loadRom: String -> Cmd Message
loadRom url =
   debug_log "loadRom" url Http.get {
                                      url = url,
                                      expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
                                    }

-- Not sure if this is helping - we want to pick out 16384 from the metadata so we know how many bytes to consume
-- as TAP files will not always be the same size...
loadTap: String -> Cmd Message
loadTap url =
    --Http.request { method = "GET",
    --               headers = [],
    --               body = emptyBody,
    --               timeout = Nothing,
    --               tracker = Nothing,
    --               url = String.concat ["http://localhost:3000/", fileName],
    --               expect = Http.expectBytesResponse GotTAP convertResponse
    --          }
    debug_log "loadTap" url Http.get { url = url,
               expect = Http.expectBytesResponse GotTAP bytesToTap
              }

bytesToTap : Response Bytes -> Result Error (List(Tapfile))
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
              length = body |> width
              y = debug_log "TAP file size" length Nothing
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
   loop (size, Array.empty) (arrayStep decoder)

arrayStep : Decoder Int -> (Int, Array Int) -> Decoder (Step (Int, Array Int) (Array Int))
arrayStep decoder (n, xs) =
   if n <= 0 then
     succeed (Done xs)
   else
     map (\x -> Loop (n - 1, Array.push x xs)) decoder

