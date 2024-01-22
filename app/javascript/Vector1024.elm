module Vector1024 exposing (..)

import Array exposing (Array)
import Maybe.Extra exposing (combine)
import Vector16 exposing (Vector16)
import Vector256 exposing (Vector256)
import Vector4 exposing (Vector4)
type alias Vector1024 = Vector4 Vector256

fromArray: Array Int -> Maybe Vector1024
fromArray array =
    let
        first1024 = array |> Array.slice 0 256
        v1k = Vector256.fromArray first1024
        second1024 = array |> Array.slice 256 512
        v2k = Vector256.fromArray second1024
        third1024 = array |> Array.slice 512 768
        v3k = Vector256.fromArray third1024
        fourth1024 = array |> Array.slice 768 1024
        v4k = Vector256.fromArray fourth1024

        maybe_list = [v1k, v2k, v3k, v4k] |> combine

    in
        case maybe_list of
            Just list ->
                let
                    maybepair = Vector4.fromList list
                in
                    case maybepair of
                        Just (_, vector) ->
                            Just vector
                        Nothing ->
                            Nothing
            Nothing -> Nothing

