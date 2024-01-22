module Vector16384 exposing (..)

type alias Index =
    {
        v16: Vector16.Index,
        v256: Vector16.Index,
        v1024: Vector4.Index,
        v16k: Vector16.Index
    }

type alias V16 = Vector16 Int
type alias V256 = Vector16 V16
type alias V1024 = Vector4 V256
type alias Vector16384 = Vector16 V1024

