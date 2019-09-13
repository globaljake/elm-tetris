module Cell exposing (Cell, Tetromino(..), color, isActive, spawn)


type Cell
    = Cell Internal


type alias Internal =
    { active : Bool, tetromino : Tetromino }


type Tetromino
    = I
    | O
    | T
    | S
    | Z
    | J
    | L


color : Cell -> String
color (Cell { tetromino }) =
    case tetromino of
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


spawn : Tetromino -> List ( ( Int, Int ), Cell )
spawn tetromino =
    case tetromino of
        I ->
            [ ( ( 3, 0 ), Cell { active = True, tetromino = I } )
            , ( ( 4, 0 ), Cell { active = True, tetromino = I } )
            , ( ( 5, 0 ), Cell { active = True, tetromino = I } )
            , ( ( 6, 0 ), Cell { active = True, tetromino = I } )
            ]

        O ->
            [ ( ( 4, 0 ), Cell { active = True, tetromino = O } )
            , ( ( 5, 0 ), Cell { active = True, tetromino = O } )
            , ( ( 4, 1 ), Cell { active = True, tetromino = O } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = O } )
            ]

        T ->
            [ ( ( 4, 0 ), Cell { active = True, tetromino = T } )
            , ( ( 5, 0 ), Cell { active = True, tetromino = T } )
            , ( ( 6, 0 ), Cell { active = True, tetromino = T } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = T } )
            ]

        S ->
            [ ( ( 5, 0 ), Cell { active = True, tetromino = S } )
            , ( ( 6, 0 ), Cell { active = True, tetromino = S } )
            , ( ( 4, 1 ), Cell { active = True, tetromino = S } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = S } )
            ]

        Z ->
            [ ( ( 4, 0 ), Cell { active = True, tetromino = Z } )
            , ( ( 5, 0 ), Cell { active = True, tetromino = Z } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = Z } )
            , ( ( 6, 1 ), Cell { active = True, tetromino = Z } )
            ]

        J ->
            [ ( ( 3, 0 ), Cell { active = True, tetromino = J } )
            , ( ( 4, 0 ), Cell { active = True, tetromino = J } )
            , ( ( 5, 0 ), Cell { active = True, tetromino = J } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = J } )
            ]

        L ->
            [ ( ( 5, 0 ), Cell { active = True, tetromino = L } )
            , ( ( 6, 0 ), Cell { active = True, tetromino = L } )
            , ( ( 7, 0 ), Cell { active = True, tetromino = L } )
            , ( ( 5, 1 ), Cell { active = True, tetromino = L } )
            ]


isActive : Cell -> Bool
isActive (Cell { active }) =
    active
