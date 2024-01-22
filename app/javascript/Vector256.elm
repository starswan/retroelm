module Vector256 exposing (..)

import Array exposing (Array)
import Maybe.Extra exposing (combine)
import Vector16 exposing (Vector16)
type alias Vector256 = Vector16 (Vector16 Int)

fromArray: Array Int -> Maybe Vector256
fromArray array =
    let
        first1024 = array |> Array.slice 0 16|> Array.toList
        v1k = Vector16.fromList first1024
        second1024 = array |> Array.slice 16 32|> Array.toList
        v2k = Vector16.fromList second1024
        third1024 = array |> Array.slice 32 48 |> Array.toList
        v3k = Vector16.fromList third1024
        fourth1024 = array |> Array.slice 48  64 |> Array.toList
        v4k = Vector16.fromList fourth1024
        fifth1024 = array |> Array.slice 64 80 |> Array.toList
        v5k = Vector16.fromList fifth1024
        sixth1024 = array |> Array.slice 80 96 |> Array.toList
        v6k = Vector16.fromList sixth1024
        seventh1024 = array |> Array.slice 96 112 |> Array.toList
        v7k = Vector16.fromList seventh1024
        eight1024 = array |> Array.slice 112 128 |> Array.toList
        v8k = Vector16.fromList eight1024
        nine1024 = array |> Array.slice 128 144 |> Array.toList
        v9k = Vector16.fromList nine1024
        ten1024 = array |> Array.slice 144 160 |> Array.toList
        v10k = Vector16.fromList ten1024
        eleevn1024 = array |> Array.slice 160 176 |> Array.toList
        v11k = Vector16.fromList eleevn1024
        twelve1024 = array |> Array.slice 176 192 |> Array.toList
        v12k = Vector16.fromList twelve1024
        thirteen1024 = array |> Array.slice 192 208 |> Array.toList
        v13k = Vector16.fromList thirteen1024
        fourteen1024 = array |> Array.slice 208 224 |> Array.toList
        v14k = Vector16.fromList fourteen1024
        fifteen1024 = array |> Array.slice 224 240 |> Array.toList
        v15k = Vector16.fromList fifteen1024
        sixteen1024 = array |> Array.slice 240 256|> Array.toList
        v16k = Vector16.fromList sixteen1024

        maybe_list = [v1k, v2k, v3k, v4k, v5k, v6k, v7k, v8k, v9k, v10k, v11k, v12k, v13k, v14k, v15k, v16k] |> combine
    in
        case maybe_list of
            Just list_list ->
                let
                    vector_list = list_list |> List.map Tuple.second
                    vector256 = Vector16.fromList vector_list
                in
                    case vector256 of
                        Just (_, actual_vector) -> Just actual_vector
                        Nothing -> Nothing
            Nothing -> Nothing
