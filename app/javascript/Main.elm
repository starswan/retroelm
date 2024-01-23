--
-- $Id$
--
module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Bytes exposing (Bytes)
import Html.Events exposing (onClick)
import Http
import Http.Detailed
import Json.Decode as Decode
import Screen exposing (ScreenLine, screenLines)
import Spectrum exposing (set_rom)
import Svg exposing (Svg, line, svg)
import Svg.Attributes exposing (height, stroke, viewBox, width, x1, x2, y1, y2)
import Time exposing (posixToMillis)
import Html exposing (Html, button, div, h2, text)
import Html.Attributes exposing (style)
import Params exposing (StringPair, valid_params)
import Qaop exposing (Message(..), Qaop, ctrlKeyDownEvent, ctrlKeyUpEvent, keyDownEvent, keyUpEvent, pause)
import Utils exposing (digitToString)

-- meant to be run every 20 msec(50Hz)
-- arthur timings:
-- Chromium debug 74.2ms(13.4 Hz) live 37.8ms(26.4 Hz)
-- firefox debug 104.6ms (9.5 Hz) live 59.9ms(16.6 Hz)
c_TICKTIME = 33

-- I'm currently unsure whether scaling the display results in a significant slowdown or not
-- what it does show is that changing the screen makes everything slower, which probably means in practice
-- that the display code will need some optimisation
c_SCALEFACTOR = 3

type alias Model =
  {
    qaop: Qaop,
    -- This doesn't look like a variable, but think it might be useful to adjust it at runtime
    tickInterval: Int,
    count: Int,
    elapsed_millis: Int,
    time: Maybe Time.Posix
  }

init : String -> (Model, Cmd Message)
init data =
   let
      params = valid_params data
      (newQaop, cmd) = Qaop.init params |> Qaop.run
   in
      ((Model newQaop c_TICKTIME 0 0 Nothing), cmd)

c_DECIMAL_PLACES = 3

time_display: Model -> String
time_display model =
   let
      elapsed_string = (model.elapsed_millis // 1000) |> String.fromInt
      loop_time_in_ms = (10 ^ c_DECIMAL_PLACES) * model.elapsed_millis // model.count
      speed_in_mhz = 1000000 / (loop_time_in_ms |> toFloat) * 1000 |> round
      time_string = loop_time_in_ms |> String.fromInt |> String.reverse |> String.toList |> List.drop c_DECIMAL_PLACES |> String.fromList |> String.reverse
      last = loop_time_in_ms |> modBy (10 ^ c_DECIMAL_PLACES) |> digitToString c_DECIMAL_PLACES
      speed_in_hz = speed_in_mhz // 1000
      speed_in_hz_frac = speed_in_mhz |> modBy 1000
   in
      elapsed_string ++ " sec, time " ++ time_string ++ "." ++ last ++ " ms " ++
      (speed_in_hz |> String.fromInt) ++ "." ++ (speed_in_hz_frac |> String.fromInt |> String.padLeft 3 '0') ++ " Hz "

lineToSvg: Int -> ScreenLine -> Svg Message
lineToSvg y_index linedata =
   line [
         x1 (linedata.start |> String.fromInt) ,
         y1 (y_index |> String.fromInt),
         x2 ((linedata.start + linedata.length) |> String.fromInt),
         y2 (y_index |> String.fromInt),
         stroke linedata.colour
         ]
         []

lineListToSvg: Int -> List ScreenLine -> List (Svg Message)
lineListToSvg y_index linelist =
   List.map (lineToSvg y_index) linelist

view : Model -> Html Message
view model =
   let
      lines = model.qaop.spectrum.cpu.env |> screenLines
   in
     -- The inline style is being used for example purposes in order to keep this example simple and
     -- avoid loading additional resources. Use a proper stylesheet when building your own app.
     div []
     [
        div [style "display" "flex", style "justify-content" "center"]
        [
            h2 [] [text ("Refresh Interval " ++ (model.tickInterval |> String.fromInt) ++ "ms ")]
            ,text ((String.fromInt model.count) ++ " in " ++ (model |> time_display))
           ,button [ onClick Pause ] [ text (if model.qaop.spectrum.paused then "Unpause" else "Pause") ]
        ]
        ,svg [height (192 * c_SCALEFACTOR |> String.fromInt), width (256 * c_SCALEFACTOR |> String.fromInt), viewBox "0 0 256 192"] (List.indexedMap lineListToSvg lines |> List.concat)
        --,svg [style "height" "192px", style "width" "256px"] (List.indexedMap lineListToSvg lines |> List.concat)
     ]

--posixToString: Maybe Time.Posix -> String
--posixToString maybe_time =
--   case maybe_time of
--      Just time ->
--         digitToString 2 (toHour utc time)
--           ++ ":" ++
--           digitToString 2 (toMinute utc time)
--           ++ ":" ++
--           digitToString 2 (toSecond utc time)
--           ++ "." ++
--           digitToString 3 (toMillis utc time)
--           ++ " (UTC)"
--      Nothing ->
--         "Time N/A"

gotRom: Qaop -> Result (Http.Detailed.Error Bytes) (Http.Metadata, Array Int) -> (Qaop, Cmd Message)
gotRom qaop result =
    case result of
        Ok (metadata, value) ->
           { qaop | spectrum = qaop.spectrum |> set_rom value } |> Qaop.run
        Err error ->
            (qaop, Cmd.none)

update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
       GotRom result ->
            let
               (qaop, cmd) = gotRom model.qaop result
            in
               ({ model | qaop = qaop, count = model.count + 1 }, cmd)
       GotTAP result ->
            let
                (qaop, cmd) = Qaop.gotTap model.qaop result
            in
                ({ model | qaop = qaop, count = model.count + 1 }, cmd)
       Tick posix ->
          let
             state = if model.qaop.spectrum.paused then
                         {qaop=model.qaop, cmd=Cmd.none, count=model.count,elapsed=0}
                     else
                        let
                           elapsed = case model.time of
                                        Just time ->
                                           posixToMillis posix -  posixToMillis time
                                        Nothing ->
                                           0
                           (q, cmd) = model.qaop |> Qaop.run
                        in
                           { qaop = q, cmd = cmd, count = model.count + 1, elapsed = elapsed }
          in
            ({ model | count = state.count, elapsed_millis = model.elapsed_millis + state.elapsed, time = Just posix, qaop = state.qaop }, state.cmd)
       Pause ->
          ({ model | time = Nothing, qaop = (model.qaop |> pause (not model.qaop.spectrum.paused))}, Cmd.none)
       CharacterKey char ->
          ({ model | qaop = (model.qaop |> keyDownEvent char) }, Cmd.none)
       ControlKeyDown str ->
           ({model | qaop = (model.qaop |> ctrlKeyDownEvent str) }, Cmd.none)
       CharacterUnKey char ->
           ({model | qaop = (model.qaop |> keyUpEvent char) }, Cmd.none)
       ControlUnKey string ->
           ({model | qaop = (model.qaop |> ctrlKeyUpEvent string) }, Cmd.none)
       KeyRepeat ->
            -- do nothing on repeating keys
            (model, Cmd.none)

subscriptions : Model -> Sub Message
subscriptions model =
   let
      keypress = onKeyDown keyDownDecoder
      keyunpress = onKeyUp keyUpDecoder
      subs = if model.qaop.spectrum.paused || not (List.isEmpty model.qaop.loader.actions) then
               [keypress, keyunpress]
             else
               let
                  tick = Time.every (model.tickInterval |> toFloat) Tick
               in
                  [tick, keypress, keyunpress]
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
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }
