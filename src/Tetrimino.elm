module Tetrimino exposing (Tetrimino(..), color, random, rotateAround, spawn)

import Random


type Tetrimino
    = I
    | O
    | T
    | S
    | Z
    | J
    | L


random : Random.Generator Tetrimino
random =
    Random.uniform I [ O, T, S, Z, J, L ]


color : Tetrimino -> String
color tetrimino =
    case tetrimino of
        I ->
            "text-teal-400"

        O ->
            "text-yellow-400"

        T ->
            "text-pink-400"

        S ->
            "text-green-400"

        Z ->
            "text-red-400"

        J ->
            "text-blue-400"

        L ->
            "text-orange-400"


spawn : Tetrimino -> List ( ( Int, Int ), ( Tetrimino, Bool ) )
spawn tetrimino =
    case tetrimino of
        I ->
            [ ( ( 3, 0 ), ( I, False ) )
            , ( ( 4, 0 ), ( I, True ) )
            , ( ( 5, 0 ), ( I, False ) )
            , ( ( 6, 0 ), ( I, False ) )
            ]

        O ->
            [ ( ( 4, 0 ), ( O, False ) )
            , ( ( 5, 0 ), ( O, False ) )
            , ( ( 4, 1 ), ( O, False ) )
            , ( ( 5, 1 ), ( O, False ) )
            ]

        T ->
            [ ( ( 4, 0 ), ( T, False ) )
            , ( ( 5, 0 ), ( T, True ) )
            , ( ( 6, 0 ), ( T, False ) )
            , ( ( 5, 1 ), ( T, False ) )
            ]

        S ->
            [ ( ( 5, 0 ), ( S, False ) )
            , ( ( 6, 0 ), ( S, False ) )
            , ( ( 4, 1 ), ( S, False ) )
            , ( ( 5, 1 ), ( S, True ) )
            ]

        Z ->
            [ ( ( 4, 0 ), ( Z, False ) )
            , ( ( 5, 0 ), ( Z, False ) )
            , ( ( 5, 1 ), ( Z, True ) )
            , ( ( 6, 1 ), ( Z, False ) )
            ]

        J ->
            [ ( ( 3, 0 ), ( J, False ) )
            , ( ( 4, 0 ), ( J, True ) )
            , ( ( 5, 0 ), ( J, False ) )
            , ( ( 5, 1 ), ( J, False ) )
            ]

        L ->
            [ ( ( 5, 0 ), ( L, False ) )
            , ( ( 6, 0 ), ( L, True ) )
            , ( ( 7, 0 ), ( L, False ) )
            , ( ( 5, 1 ), ( L, False ) )
            ]


rotateAround : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
rotateAround ( x, y ) ( cx, cy ) =
    ( ((y - cy) * -1) + cx, (x - cx) + cy )
