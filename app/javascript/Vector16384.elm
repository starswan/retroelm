module Vector16384 exposing (..)

import Array exposing (Array)
-- convert list of Maybe a  into a Maybe List a
import Maybe.Extra exposing (combine)
import Vector1024 exposing (Vector1024)
import Vector16 exposing (Vector16)
import Vector4 exposing (Vector4)
type alias Index =
    {
        v16: Vector16.Index,
        v256: Vector16.Index,
        v1024: Vector4.Index,
        v16k: Vector16.Index
    }

type alias Vector16384 = Vector16 Vector1024

fromArray: Array Int -> Maybe Vector16384
fromArray array =
    let
        first1024 = array |> Array.slice 0 1024
        v1k = Vector1024.fromArray first1024
        second1024 = array |> Array.slice 1024 2048
        v2k = Vector1024.fromArray second1024
        third1024 = array |> Array.slice 2048 3072
        v3k = Vector1024.fromArray third1024
        fourth1024 = array |> Array.slice 3072 4096
        v4k = Vector1024.fromArray fourth1024
        fifth1024 = array |> Array.slice 4096 5120
        v5k = Vector1024.fromArray fifth1024
        sixth1024 = array |> Array.slice 5120 6144
        v6k = Vector1024.fromArray sixth1024
        seventh1024 = array |> Array.slice 6144 7168
        v7k = Vector1024.fromArray seventh1024
        eight1024 = array |> Array.slice 7168 8192
        v8k = Vector1024.fromArray eight1024
        nine1024 = array |> Array.slice 8192 9216
        v9k = Vector1024.fromArray nine1024
        ten1024 = array |> Array.slice 9216 10240
        v10k = Vector1024.fromArray ten1024
        eleevn1024 = array |> Array.slice 10240 11264
        v11k = Vector1024.fromArray eleevn1024
        twelve1024 = array |> Array.slice 11264 12288
        v12k = Vector1024.fromArray twelve1024
        thirteen1024 = array |> Array.slice 12288 13312
        v13k = Vector1024.fromArray thirteen1024
        fourteen1024 = array |> Array.slice 13312 14336
        v14k = Vector1024.fromArray fourteen1024
        fifteen1024 = array |> Array.slice 14336 15360
        v15k = Vector1024.fromArray fifteen1024
        sixteen1024 = array |> Array.slice 15360 16384
        v16k = Vector1024.fromArray sixteen1024

        maybe_list = [v1k, v2k, v3k, v4k, v5k, v6k, v7k, v8k, v9k, v10k, v11k, v12k, v13k, v14k, v15k, v16k] |> combine
    in
        case maybe_list of
            Just list ->
                let
                    maybepair = Vector16.fromList list
                in
                    case maybepair of
                        Just (_, vector) ->
                            Just vector
                        Nothing ->
                            Nothing
            Nothing -> Nothing
