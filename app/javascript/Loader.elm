--
-- $Id$
--
module Loader exposing (..)

import Params exposing (StringPair)
type LoadAction = LoadROM String | LoadTAP String

type alias Loader =
    {
        actions: List LoadAction
    }

trimActionList: Maybe(List LoadAction) -> List LoadAction
trimActionList tail =
    case tail of
        Just a ->
            a
        Nothing ->
            []

paramHandler: StringPair -> Maybe(LoadAction)
paramHandler item =
    if item.first == "rom" then
       Just (LoadROM item.second)
    else if item.first == "tape" then
       Just (LoadTAP item.second)
    else
       Nothing