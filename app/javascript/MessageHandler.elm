module MessageHandler exposing (..)

import Array exposing (Array)
import Bitwise exposing (complement)
import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode exposing (Decoder, Step(..), loop, map, succeed, unsignedInt8)
import Char
import Http exposing (Error, Expect, Metadata, Response)
import Http.Detailed
import Loader exposing (LoadAction(..), Loader, trimActionList)
import Qaop exposing (Qaop)
import Spectrum exposing (frames, new_tape, set_rom)
import String
import Tapfile exposing (Tapfile, parseTapFile)
import Time
import Z80Debug exposing (debug_log)


type Message
    = GotTAP (Result Http.Error (List Tapfile))
    | GotRom (Result (Http.Detailed.Error Bytes) ( Metadata, Array Int ))
    | Tick Time.Posix
    | Pause
    | CharacterKey Char
    | CharacterUnKey Char
    | ControlKeyDown String
    | ControlUnKey String
    | KeyRepeat
    | LoadTape


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


actionToCmd : LoadAction -> Cmd Message
actionToCmd action =
    case action of
        LoadTAP url ->
            -- Not sure if this is helping - we want to pick out 16384 from the metadata so we know how many bytes to consume
            -- as TAP files will not always be the same size...
            --Http.request { method = "GET",
            --               headers = [],
            --               body = emptyBody,
            --               timeout = Nothing,
            --               tracker = Nothing,
            --               url = String.concat ["http://localhost:3000/", fileName],
            --               expect = Http.expectBytesResponse GotTAP convertResponse
            --          }
            debug_log "loadTap"
                url
                Http.get
                { url = url
                , expect = Http.expectBytesResponse GotTAP bytesToTap
                }

        LoadROM url ->
            --loadRom: String -> Cmd Message
            --loadRom fileName =
            --    Http.get { url = String.concat ["http://localhost:3000/", fileName],
            --               expect = Http.expectBytes GotRom (list_decoder 16384 unsignedInt8)
            --              }
            debug_log "loadRom"
                url
                Http.get
                { url = url
                , expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
                }

gotRom: Qaop -> Result (Http.Detailed.Error Bytes) (Http.Metadata, Array Int) -> (Qaop, Cmd Message)
gotRom qaop result =
    case result of
        Ok (_, value) ->
           { qaop | spectrum = qaop.spectrum |> set_rom value } |> run
        Err _ ->
            (qaop, Cmd.none)

gotTap: Qaop -> Result Http.Error (List Tapfile) -> (Qaop, Cmd Message)
gotTap qaop result =
    case result of
      Ok value ->
         -- THe infinite recursion issue goes away if tape is a Dict rather than a List or Array
         -- it only happens when the VM starts running - if this is replaced with Cmd.none
         -- and we unpause manually, it crashes on the unpause - so something to do with Qaop.run
         -- and copying the Array
         { qaop | spectrum = qaop.spectrum |> new_tape value } |> run
      Err _ ->
         (qaop, Cmd.none)

--	public void run() {
--		Loader l;
--		for(;;) try {
--			synchronized(queue) {
--				if(queue.isEmpty()) {
--					state &= ~2;
--					spectrum.pause(010);
--					queue.wait();
--					continue;
--				}
--				state |= 2;
--				l = (Loader)queue.remove(0);
--			}
--			l.exec();
--		} catch(InterruptedException x) {
--			break;
--		} catch(Exception x) {
--			x.printStackTrace();
--		}
--	}
run: Qaop -> (Qaop, Cmd Message)
run qaop =
   if qaop.spectrum.paused then
      let
        loader = qaop.loader
        nextAction = List.head loader.actions
        qaop_1 = { qaop | loader = { loader | actions = (List.tail loader.actions) |> trimActionList } }

        (q2, cmd) = case nextAction of
                      Just action ->
                        (qaop_1, (actionToCmd action))
                      Nothing ->
                        ({ qaop_1 | state = (Bitwise.and qaop.state (complement 2)), spectrum = qaop_1.spectrum |> Spectrum.pause 0x08 }, Cmd.none)
      in
         (q2, cmd)
   else
      ({ qaop | spectrum = qaop.spectrum |> frames qaop.keys}, Cmd.none)

