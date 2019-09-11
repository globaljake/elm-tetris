module Grid exposing (Grid, coordinates, dimentions, fromKey, get, init, insert, toList)

import Dict exposing (Dict)
import Tetromino exposing (Tetromino)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { x : Int, y : Int, squares : Dict String a }


empty : ( Int, Int ) -> Grid a
empty ( x, y ) =
    Grid { x = x, y = y, squares = Dict.empty }


init : ( Int, Int ) -> List ( ( Int, Int ), a ) -> Grid a
init ( x, y ) locVals =
    Grid
        { x = x
        , y = y
        , squares =
            locVals
                |> List.map (Tuple.mapFirst key)
                |> Dict.fromList
        }


dimentions : Grid a -> ( Int, Int )
dimentions (Grid { x, y }) =
    ( x, y )


coordinates : Grid a -> List ( Int, Int )
coordinates (Grid { x, y }) =
    List.range 0 ((x * y) - 1)
        |> List.map (\cellNumber -> ( modBy x cellNumber, cellNumber // x ))


key : ( Int, Int ) -> String
key ( x, y ) =
    [ x, y ]
        |> List.map String.fromInt
        |> String.join "-"


fromKey : String -> ( Int, Int )
fromKey k =
    case List.map (Maybe.withDefault 0 << String.toInt) (String.split "-" k) of
        [ x, y ] ->
            ( x, y )

        _ ->
            ( 1000, 1000 )


get : ( Int, Int ) -> Grid a -> Maybe a
get location (Grid grid) =
    Dict.get (key location) grid.squares


insert : ( Int, Int ) -> a -> Grid a -> Result String (Grid a)
insert location value grid =
    batchInsert [ ( location, value ) ] grid


batchInsert : List ( ( Int, Int ), a ) -> Grid a -> Result String (Grid a)
batchInsert locVals (Grid grid) =
    if List.member False <| List.map (validate grid.squares << Tuple.first) locVals then
        Err "already taken"

    else
        Ok <|
            Grid
                { grid
                    | squares =
                        locVals
                            |> List.map (Tuple.mapFirst key)
                            |> Dict.fromList
                            |> Dict.union grid.squares
                }


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid grid) =
    grid.squares
        |> Dict.toList
        |> List.map (Tuple.mapFirst fromKey)


validate : Dict String a -> ( Int, Int ) -> Bool
validate squares location =
    squares
        |> Dict.toList
        |> List.map Tuple.first
        |> List.member (key location)
        |> not



-- type alias Internal =
--     { x : Int, y : Int, color : String }
-- moveLeft : List Square -> List Square
-- moveLeft squares =
--     if listIncludes (\{ x } -> x == 0) squares then
--         squares
--     else
--         listMap (\square -> { square | x = square.x + 1 }) squares
-- moveRight : List Square -> List Square
-- moveRight squares =
--     if listIncludes (\{ x } -> x == 0) squares then
--         squares
--     else
--         listMap (\square -> { square | x = square.x + 1 }) squares
-- map : (Internal -> Internal) -> Square -> Square
-- map f (Square square) =
--     Square (f square)
-- listMap : (Internal -> Internal) -> List Square -> List Square
-- listMap f squares =
--     List.map (map f) squares
-- listIncludes : (Internal -> Bool) -> List Square -> Bool
-- listIncludes condition squares =
--     List.foldl (\(Square square) b -> condition square) False squares
-- listFromTetromino : Tetromino -> List Square
-- listFromTetromino tetromino =
--     case tetromino of
--         I ->
--             [ { location = ( 3, 0 ), block = I }
--             , { location = ( 4, 0 ), block = I }
--             , { location = ( 5, 0 ), block = I }
--             , { location = ( 6, 0 ), block = I }
--             ]
--         O ->
--             [ { location = ( 4, 0 ), block = O }
--             , { location = ( 5, 0 ), block = O }
--             , { location = ( 4, 1 ), block = O }
--             , { location = ( 5, 1 ), block = O }
--             ]
--         T ->
--             [ { location = ( 4, 0 ), block = T }
--             , { location = ( 5, 0 ), block = T }
--             , { location = ( 6, 0 ), block = T }
--             , { location = ( 5, 1 ), block = T }
--             ]
--         S ->
--             [ { location = ( 5, 0 ), block = S }
--             , { location = ( 6, 0 ), block = S }
--             , { location = ( 4, 1 ), block = S }
--             , { location = ( 5, 1 ), block = S }
--             ]
--         Z ->
--             [ { location = ( 4, 0 ), block = Z }
--             , { location = ( 5, 0 ), block = Z }
--             , { location = ( 5, 1 ), block = Z }
--             , { location = ( 6, 1 ), block = Z }
--             ]
--         J ->
--             [ { location = ( 3, 0 ), block = J }
--             , { location = ( 4, 0 ), block = J }
--             , { location = ( 5, 0 ), block = J }
--             , { location = ( 5, 1 ), block = J }
--             ]
--         L ->
--             [ { location = ( 5, 0 ), block = L }
--             , { location = ( 6, 0 ), block = L }
--             , { location = ( 7, 0 ), block = L }
--             , { location = ( 5, 1 ), block = L }
--             ]
-- type Tetromino
--     = I
--     | O
--     | T
--     | S
--     | Z
--     | J
--     | L
-- color : Tetromino -> String
-- color block =
--     case block of
--         I ->
--             "text-teal-400"
--         O ->
--             "text-yellow-400"
--         T ->
--             "text-pink-400"
--         S ->
--             "text-green-400"
--         Z ->
--             "text-red-400"
--         J ->
--             "text-blue-400"
--         L ->
--             "text-orange-400"
-- add : Tetromino -> List { location : ( Int, Int ), block : Tetromino }
-- add tetromino =
--     case tetromino of
--         I ->
--             [ { location = ( 3, 0 ), block = I }
--             , { location = ( 4, 0 ), block = I }
--             , { location = ( 5, 0 ), block = I }
--             , { location = ( 6, 0 ), block = I }
--             ]
--         O ->
--             [ { location = ( 4, 0 ), block = O }
--             , { location = ( 5, 0 ), block = O }
--             , { location = ( 4, 1 ), block = O }
--             , { location = ( 5, 1 ), block = O }
--             ]
--         T ->
--             [ { location = ( 4, 0 ), block = T }
--             , { location = ( 5, 0 ), block = T }
--             , { location = ( 6, 0 ), block = T }
--             , { location = ( 5, 1 ), block = T }
--             ]
--         S ->
--             [ { location = ( 5, 0 ), block = S }
--             , { location = ( 6, 0 ), block = S }
--             , { location = ( 4, 1 ), block = S }
--             , { location = ( 5, 1 ), block = S }
--             ]
--         Z ->
--             [ { location = ( 4, 0 ), block = Z }
--             , { location = ( 5, 0 ), block = Z }
--             , { location = ( 5, 1 ), block = Z }
--             , { location = ( 6, 1 ), block = Z }
--             ]
--         J ->
--             [ { location = ( 3, 0 ), block = J }
--             , { location = ( 4, 0 ), block = J }
--             , { location = ( 5, 0 ), block = J }
--             , { location = ( 5, 1 ), block = J }
--             ]
--         L ->
--             [ { location = ( 5, 0 ), block = L }
--             , { location = ( 6, 0 ), block = L }
--             , { location = ( 7, 0 ), block = L }
--             , { location = ( 5, 1 ), block = L }
--             ]
