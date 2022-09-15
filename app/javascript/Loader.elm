--
-- $Id$
--
module Loader exposing (..)

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