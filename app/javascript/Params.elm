--
-- $Id$
--
module Params exposing (..)

import Utils exposing (compact)
type alias StringPair =
    {
        first: String,
        second: String
    }

secondParam: Maybe(List String) -> Maybe(String)
secondParam tail =
    case tail of
        Just a ->
            List.head a
        Nothing ->
            Nothing

stringPair: Maybe(String) -> Maybe(String) -> Maybe(StringPair)
stringPair first second =
    case first of
        Just first_value ->
            case second of
                Just second_value ->
                    Just(StringPair first_value second_value)
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

splitOnEquals: String -> Maybe StringPair
splitOnEquals data =
    let
        splitData = String.split "=" data
        first = List.head splitData
        second = secondParam(List.tail splitData)
    in
        stringPair first second

valid_params: String -> List StringPair
valid_params data =
   compact(List.map splitOnEquals (data |> String.split " "))
