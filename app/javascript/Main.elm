--
-- $Id$
--


module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (values)
import Html exposing (Html, button, div, h2, span, text)
import Html.Attributes exposing (disabled, id, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Keyboard exposing (ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent)
import Loader exposing (LoadAction(..))
import MessageHandler exposing (Message(..), gotRom, gotTap, run)
import Params exposing (StringPair, valid_params)
import Qaop exposing (Qaop, pause)
import Spectrum
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (fill, height, rx, stroke, viewBox, width, x1, x2, y1, y2)
import Time exposing (posixToMillis)
import Utils exposing (speed_in_hz, time_display)
import Z80Debug exposing (debug_log)
import Z80Screen exposing (ScreenLine, screenLines, spectrumColour)



-- meant to be run every 20 msec(50Hz)
-- arthur timings:
-- 23rd Jun 2024 Chromium debug 70.5ms (14.2 Hz) 106 sec live 36.3ms (27.6 Hz) 163 sec
-- 23rd Jun 2024 Firefox                                 live 61.5ms (16.2 Hz) 160 sec
--
-- 14th Feb 2024 Chromium debug 70.3ms (14.2 Hz) 184     live 42.1ms (23.7 Hz)  73 sec
-- 14th Feb 2024 Firefox  debug 96.3ms (10.4 Hz) 217     live 72.0ms (13.9 Hz) 355 sec
-- 29th Jan 2024 Chromium debug 69.9ms (14.3 Hz) 365 sec live 37.8ms (26.6 Hz)
-- 29th Jan 2024 Firefox  debug 95.1ms (10.5 Hz)         live 59.3ms (16.8 Hz) 900 sec
-- Run at 25 (40Hz) - i7 laptop can do 20Hz in firefox dev mode


c_TICKTIME =
    25



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


init : String -> ( Model, Cmd Message )
init data =
    let
        params =
            valid_params data

        ( newQaop, cmd ) =
            Qaop.new params |> run
    in
    ( Model newQaop c_TICKTIME 0 0 Nothing, cmd )


lineToSvg : Int -> ScreenLine -> Svg Message
lineToSvg y_index linedata =
    line
        [ x1 (48 + linedata.start |> String.fromInt)
        , y1 (40 + y_index |> String.fromInt)
        , x2 ((48 + linedata.start + linedata.length) |> String.fromInt)
        , y2 (40 + y_index |> String.fromInt)
        , stroke linedata.colour
        ]
        []


lineListToSvg : Int -> List ScreenLine -> List (Svg Message)
lineListToSvg y_index linelist =
    List.map (lineToSvg y_index) linelist


view : Model -> Html Message
view model =
    let
        screen =
            model.qaop.spectrum.cpu.env.ram.screen

        lines =
            screen |> screenLines

        screen_data =
            Dict.map lineListToSvg lines |> values

        -- border colour is never bright
        border_colour =
            spectrumColour screen.border False

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
        , svg
            [ height (272 * c_SCALEFACTOR |> String.fromInt), width (352 * c_SCALEFACTOR |> String.fromInt), viewBox "0 0 352 272" ]
            --<rect width="100%" height="100%" fill="green" />
            screen_data_list

        --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
        ]


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        LoadTape ->
            let
                newmodel =
                    debug_log "load_tape" "into model" model

                -- here we push the tapfile into Qoap and execute appropriately
            in
            ( newmodel, Cmd.none )

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
        keypress =
            onKeyDown keyDownDecoder

        keyunpress =
            onKeyUp keyUpDecoder

        subs =
            if model.qaop.spectrum.paused || not (List.isEmpty model.qaop.loader.actions) then
                [ keypress, keyunpress ]

            else
                let
                    tick =
                        Time.every (model.tickInterval |> toFloat) Tick
                in
                [ tick, keypress, keyunpress ]
    in
    Sub.batch subs


keyDownDecoder : Decode.Decoder Message
keyDownDecoder =
    Decode.map2 toKey (Decode.field "key" Decode.string) (Decode.field "repeat" Decode.bool)


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


keyUpDecoder : Decode.Decoder Message
keyUpDecoder =
    Decode.map toUnKey (Decode.field "key" Decode.string)


toUnKey : String -> Message
toUnKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterUnKey char

        _ ->
            ControlUnKey keyValue


main : Program String Model Message
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
