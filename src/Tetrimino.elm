module Tetrimino exposing (Tetrimino(..), color, random, spawn)

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


spawn : Tetrimino -> List ( ( Int, Int ), Tetrimino )
spawn tetrimino =
    case tetrimino of
        I ->
            [ ( ( 3, 0 ), I )
            , ( ( 4, 0 ), I )
            , ( ( 5, 0 ), I )
            , ( ( 6, 0 ), I )
            ]

        O ->
            [ ( ( 4, 0 ), O )
            , ( ( 5, 0 ), O )
            , ( ( 4, 1 ), O )
            , ( ( 5, 1 ), O )
            ]

        T ->
            [ ( ( 4, 0 ), T )
            , ( ( 5, 0 ), T )
            , ( ( 6, 0 ), T )
            , ( ( 5, 1 ), T )
            ]

        S ->
            [ ( ( 5, 0 ), S )
            , ( ( 6, 0 ), S )
            , ( ( 4, 1 ), S )
            , ( ( 5, 1 ), S )
            ]

        Z ->
            [ ( ( 4, 0 ), Z )
            , ( ( 5, 0 ), Z )
            , ( ( 5, 1 ), Z )
            , ( ( 6, 1 ), Z )
            ]

        J ->
            [ ( ( 3, 0 ), J )
            , ( ( 4, 0 ), J )
            , ( ( 5, 0 ), J )
            , ( ( 5, 1 ), J )
            ]

        L ->
            [ ( ( 5, 0 ), L )
            , ( ( 6, 0 ), L )
            , ( ( 7, 0 ), L )
            , ( ( 5, 1 ), L )
            ]


down : List ( ( Int, Int ), Tetrimino ) -> List ( ( Int, Int ), Tetrimino )
down group =
    List.map (\( ( x, y ), tet ) -> ( ( x, y + 1 ), tet )) group
