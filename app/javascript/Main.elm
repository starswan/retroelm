--
-- $Id$
--


module Main exposing (..)

import Array exposing (Array)
import Bitwise exposing (complement)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Bytes exposing (Bytes)
import Bytes.Decode exposing (unsignedInt8)
import Dict
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (disabled, id, style, tabindex)
import Html.Events exposing (onClick)
import Http exposing (Metadata)
import Http.Detailed
import Json.Decode as Decode
import Keyboard exposing (ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent)
import Loader exposing (LoadAction(..), trimActionList)
import MessageHandler exposing (array_decoder, bytesToTap)
import Params exposing (StringPair, valid_params)
import Qaop exposing (Qaop, pause)
import Spectrum exposing (Spectrum, frames, loadTapfile, new_tape, set_rom)
import SpectrumColour exposing (colourToString, spectrumColour)
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, stroke, viewBox, width, x1, x2, y1, y2)
import Tapfile exposing (Tapfile)
import Time exposing (posixToMillis)
import Utils exposing (speed_in_hz, time_display)
import Z80Debug exposing (debugLog)
import Z80Screen exposing (ScreenColourRun, screenLines)



-- meant to be run every 20 msec(50Hz)
-- arthur timings:
--  6th Nov 2024 Chromium debug 59.9ms (16.76 Hz)  64 sec live 29.9ms (32.2 Hz)  60 sec

--  4th Oct 2024 Chromium debug 61.9ms (16.1 Hz) 174 sec live 31.7ms (31.5 Hz)
--  4th Oct 2024 Firefox  debug 81.0ms (12.3 Hz) 105 sec live 56.8ms (17.6 Hz)
-- 20th Jul 2024 Chromium                                live 37.9ms (28.5 Hz)
-- 12th Jul 2024 Firefox  debug        (12.4 Hz)         live        (17.9 Hz)
--
-- 23rd Jun 2024 Chromium debug 70.5ms (14.2 Hz) 106 sec live 36.3ms (27.6 Hz) 163 sec
-- 23rd Jun 2024 Firefox                                 live 61.5ms (16.2 Hz) 160 sec
--
-- 29th Jan 2024 Chromium debug 69.9ms (14.3 Hz) 365 sec live 37.8ms (26.6 Hz)
-- 29th Jan 2024 Firefox  debug 95.1ms (10.5 Hz)         live 59.3ms (16.8 Hz) 900 sec
-- Run at 25 (40Hz) - i7 laptop can do 20Hz in firefox dev mode


c_TICKTIME =
    20



-- I'm currently unsure whether scaling the display results in a significant slowdown or not
-- what it does show is that changing the screen makes everything slower, which probably means in practice
-- that the display code will need some optimisation


c_SCALEFACTOR =
    2


type alias Model =
    { qaop : Qaop
    , -- This doesn't look like a variable, but think it might be useful to adjust it at runtime
      tickInterval : Int
    , count : Int
    , elapsed_millis : Int
    , time : Maybe Time.Posix
    }


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


init : String -> ( Model, Cmd Message )
init data =
    let
        params =
            valid_params data

        ( newQaop, cmd ) =
            Qaop.new params |> run
    in
    ( Model newQaop c_TICKTIME 0 0 Nothing, cmd )


lineToSvg : Int -> ScreenColourRun -> Svg Message
lineToSvg y_index linedata =
    line
        [ x1 (48 + linedata.start |> String.fromInt)
        , y1 (40 + y_index |> String.fromInt)
        , x2 ((48 + linedata.start + linedata.length) |> String.fromInt)
        , y2 (40 + y_index |> String.fromInt)
        , stroke (linedata.colour |> colourToString)
        ]
        []


lineListToSvg : Int -> List ScreenColourRun -> List (Svg Message)
lineListToSvg y_index linelist =
    linelist |> List.map (lineToSvg y_index)


view : Model -> Html Message
view model =
    let
        screen =
            model.qaop.spectrum.cpu.env.ram.screen

        screen_data =
            screen
                |> screenLines
                |> List.indexedMap lineListToSvg

        -- border colour is never bright
        border_colour =
            spectrumColour screen.border False |> colourToString

        background =
            [ rect [ height "100%", width "100%", fill border_colour, rx "15" ] [] ]

        screen_data_list =
            background :: screen_data |> List.concat

        load_disabled =
            case model.qaop.spectrum.tape of
                Just _ ->
                    False

                Nothing ->
                    True

        speed =
            speed_in_hz model.elapsed_millis model.count

        time_disp =
            time_display model.elapsed_millis model.count
    in
    -- The inline style is being used for example purposes in order to keep this example simple and
    -- avoid loading additional resources. Use a proper stylesheet when building your own app.
    div []
        [ h2 [] [ text ("Refresh Interval " ++ (model.tickInterval |> String.fromInt) ++ "ms ") ]
        , div [ style "display" "flex", style "justify-content" "center" ]
            [ div [] [ text (String.fromInt model.count), text " in ", text time_disp, span [ id "hz" ] [ text speed ], text " Hz" ]
            , button [ onClick Pause ]
                [ text
                    (if model.qaop.spectrum.paused then
                        "Unpause"

                     else
                        "Pause"
                    )
                ]
            , button [ onClick LoadTape, disabled load_disabled ] [ text "Load Tape" ]
            ]
        , div [ tabindex 0, id "spectrum" ]
            [ svg
                [ height (272 * c_SCALEFACTOR |> String.fromInt), width (352 * c_SCALEFACTOR |> String.fromInt), viewBox "0 0 352 272" ]
                --<rect width="100%" height="100%" fill="green" />
                screen_data_list
            ]

        --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
        ]

update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadTape ->
            let
                (first, rest) = case model.qaop.spectrum.tape of
                                  Just a_tape -> (a_tape.tapfiles |> Dict.get 0, a_tape.tapfiles |> Dict.remove 0)
                                  Nothing -> (Nothing, Dict.empty)
                -- here we push the tapfile into Qoap and execute appropriately
                speccy = case first of
                    Just tapfile -> model.qaop.spectrum |> loadTapfile tapfile
                    Nothing -> model.qaop.spectrum

                qaop =
                    debugLog "load_tape" "into model" model.qaop

            in
            ( {model | qaop = { qaop | spectrum = speccy }}, Cmd.none )

        GotRom result ->
            let
                ( qaop, cmd ) =
                    gotRom model.qaop result
            in
            ( { model | qaop = qaop, count = model.count + 1 }, cmd )

        GotTAP result ->
            let
                ( qaop, cmd ) =
                    gotTap model.qaop result

                --run_after_30_sec = delay 30000 LoadTape
            in
            ( { model | qaop = qaop, count = model.count + 1 }, cmd )

        Tick posix ->
            let
                state =
                    if model.qaop.spectrum.paused then
                        { qaop = model.qaop, cmd = Cmd.none, count = model.count, elapsed = 0 }

                    else
                        let
                            elapsed =
                                case model.time of
                                    Just time ->
                                        posixToMillis posix - posixToMillis time

                                    Nothing ->
                                        0

                            ( q, cmd ) =
                                model.qaop |> run
                        in
                        { qaop = q, cmd = cmd, count = model.count + 1, elapsed = elapsed }
            in
            ( { model | count = state.count, elapsed_millis = model.elapsed_millis + state.elapsed, time = Just posix, qaop = state.qaop }, state.cmd )

        Pause ->
            ( { model | time = Nothing, qaop = model.qaop |> pause (not model.qaop.spectrum.paused) }, Cmd.none )

        CharacterKey char ->
            let
                qaop =
                    model.qaop
            in
            ( { model | qaop = { qaop | keys = qaop.keys |> keyDownEvent char } }, Cmd.none )

        ControlKeyDown str ->
            let
                qaop =
                    model.qaop
            in
            ( { model | qaop = { qaop | keys = qaop.keys |> ctrlKeyDownEvent str } }, Cmd.none )

        CharacterUnKey char ->
            let
                qaop =
                    model.qaop
            in
            ( { model | qaop = { qaop | keys = qaop.keys |> keyUpEvent char } }, Cmd.none )

        ControlUnKey str ->
            let
                qaop =
                    model.qaop
            in
            ( { model | qaop = { qaop | keys = qaop.keys |> ctrlKeyUpEvent str } }, Cmd.none )

        KeyRepeat ->
            -- do nothing on repeating keys
            ( model, Cmd.none )


subscriptions : Model -> Sub Message
subscriptions model =
    let
        keysubs =
            [ onKeyDown keyDownDecoder, onKeyUp keyUpDecoder ]

        subs =
            if model.qaop.spectrum.paused || not (List.isEmpty model.qaop.loader.actions) then
                keysubs

            else
                let
                    tick =
                        Time.every (model.tickInterval |> toFloat) Tick
                in
                tick :: keysubs
    in
    Sub.batch subs


keyDownDecoder : Decode.Decoder Message
keyDownDecoder =
    Decode.map2 toKey (Decode.field "key" Decode.string) (Decode.field "repeat" Decode.bool)


keyUpDecoder : Decode.Decoder Message
keyUpDecoder =
    Decode.map toUnKey (Decode.field "key" Decode.string)


toKey : String -> Bool -> Message
toKey keyValue repeat =
    if repeat then
        KeyRepeat

    else
        case String.uncons keyValue of
            Just ( char, "" ) ->
                CharacterKey char

            _ ->
                ControlKeyDown keyValue


toUnKey : String -> Message
toUnKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterUnKey char

        _ ->
            ControlUnKey keyValue


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
            debugLog "loadTap"
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
            debugLog "loadRom"
                url
                Http.get
                { url = url
                , expect = Http.Detailed.expectBytes GotRom (array_decoder 16384 unsignedInt8)
                }


gotRom : Qaop -> Result (Http.Detailed.Error Bytes) ( Http.Metadata, Array Int ) -> ( Qaop, Cmd Message )
gotRom qaop result =
    case result of
        Ok ( _, value ) ->
            { qaop | spectrum = qaop.spectrum |> set_rom value } |> run

        Err _ ->
            ( qaop, Cmd.none )


gotTap : Qaop -> Result Http.Error (List Tapfile) -> ( Qaop, Cmd Message )
gotTap qaop result =
    case result of
        Ok value ->
            -- THe infinite recursion issue goes away if tape is a Dict rather than a List or Array
            -- it only happens when the VM starts running - if this is replaced with Cmd.none
            -- and we unpause manually, it crashes on the unpause - so something to do with Qaop.run
            -- and copying the Array
            { qaop | spectrum = qaop.spectrum |> new_tape value } |> run

        Err _ ->
            ( qaop, Cmd.none )



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


run : Qaop -> ( Qaop, Cmd Message )
run qaop =
    if qaop.spectrum.paused then
        let
            loader =
                qaop.loader

            nextAction =
                List.head loader.actions

            qaop_1 =
                { qaop | loader = { loader | actions = List.tail loader.actions |> trimActionList } }

            ( q2, cmd ) =
                case nextAction of
                    Just action ->
                        ( qaop_1, actionToCmd action )

                    Nothing ->
                        ( { qaop_1 | state = Bitwise.and qaop.state (complement 2), spectrum = qaop_1.spectrum |> Spectrum.pause 0x08 }, Cmd.none )
        in
        ( q2, cmd )

    else
        ( { qaop | spectrum = qaop.spectrum |> frames qaop.keys }, Cmd.none )
    --else
    --    case qaop.spectrum.tape of
    --        Just tape ->
    --            let
    --                newenv = qaop.spectrum.cpu.env
    --        Nothing -> ( { qaop | spectrum = qaop.spectrum |> frames qaop.keys }, Cmd.none )


main : Program String Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
