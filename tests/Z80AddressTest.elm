module Z80AddressTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Z80Address exposing (decrement, fromInt, incrementBy1, incrementBy2, toInt)


suite : Test
suite =
    describe "Z80 Address"
        -- Nest as many descriptions as you like.
        [ describe "increment"
            [ test "ROM boundary" <|
                \_ ->
                    let
                        b =
                            fromInt 0x3FFF

                        c =
                            b |> incrementBy1
                    in
                    (c |> toInt) |> Expect.equal 0x4000
            , test "Screen boundary" <|
                \_ ->
                    let
                        b =
                            fromInt (16384 + 6911)

                        c =
                            b |> incrementBy1
                    in
                    (c |> toInt) |> Expect.equal (16384 + 6912)
            , test "Lomem boundary" <|
                \_ ->
                    let
                        b =
                            fromInt 0x7FFF

                        c =
                            b |> incrementBy1
                    in
                    (c |> toInt) |> Expect.equal 0x8000
            , test "Himem boundary" <|
                \_ ->
                    let
                        b =
                            fromInt 0xFFFF

                        c =
                            b |> incrementBy1
                    in
                    (c |> toInt) |> Expect.equal 0
            , describe "increment2"
                [ test "ROM boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0x3FFE

                            c =
                                b |> incrementBy2
                        in
                        (c |> toInt) |> Expect.equal 0x4000
                , test "Screen boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt (16384 + 6910)

                            c =
                                b |> incrementBy2
                        in
                        (c |> toInt) |> Expect.equal (16384 + 6912)
                , test "Lomem boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0x7FFE

                            c =
                                b |> incrementBy2
                        in
                        (c |> toInt) |> Expect.equal 0x8000
                , test "Himem boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0xFFFE

                            c =
                                b |> incrementBy2
                        in
                        (c |> toInt) |> Expect.equal 0
                ]
            , describe "decrement"
                [ test "ROM boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0x4000

                            c =
                                b |> decrement
                        in
                        (c |> toInt) |> Expect.equal 0x3FFF
                , test "Screen boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt (16384 + 6912)

                            c =
                                b |> decrement
                        in
                        (c |> toInt) |> Expect.equal (16384 + 6911)
                , test "Lomem boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0x8000

                            c =
                                b |> decrement
                        in
                        (c |> toInt) |> Expect.equal 0x7FFF
                , test "Himem boundary" <|
                    \_ ->
                        let
                            b =
                                fromInt 0

                            c =
                                b |> decrement
                        in
                        (c |> toInt) |> Expect.equal 0xFFFF
                ]
            ]
        ]
