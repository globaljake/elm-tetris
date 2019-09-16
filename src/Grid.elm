module Grid exposing
    ( Grid
    , coordinates
    , dimensions
    , down
    , empty
    , filter
    , fromKey
    , get
    , inBounds
    , init
    , insert
    , keys
    , left
    , member
    , remove
    , right
    , toList
    , up
    )

import Dict exposing (Dict)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { x : Int, y : Int, cells : Dict String a }


empty : ( Int, Int ) -> Grid a
empty ( x, y ) =
    Grid { x = x, y = y, cells = Dict.empty }


left : ( Int, Int ) -> ( Int, Int )
left ( x, y ) =
    ( x - 1, y )


right : ( Int, Int ) -> ( Int, Int )
right ( x, y ) =
    ( x + 1, y )


down : ( Int, Int ) -> ( Int, Int )
down ( x, y ) =
    ( x, y + 1 )


up : ( Int, Int ) -> ( Int, Int )
up ( x, y ) =
    ( x, y - 1 )


init : ( Int, Int ) -> List ( ( Int, Int ), a ) -> Grid a
init ( x, y ) positions =
    Grid
        { x = x
        , y = y
        , cells =
            positions
                |> List.map (Tuple.mapFirst key)
                |> Dict.fromList
        }


dimensions : Grid a -> ( Int, Int )
dimensions (Grid { x, y }) =
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


get : ( Int, Int ) -> Grid v -> Maybe v
get position (Grid grid) =
    Dict.get (key position) grid.cells


member : ( Int, Int ) -> Grid a -> Bool
member position (Grid grid) =
    Dict.member (key position) grid.cells


filter : (( Int, Int ) -> v -> Bool) -> Grid v -> Grid v
filter f (Grid grid) =
    Grid { grid | cells = Dict.filter (\k v -> f (fromKey k) v) grid.cells }


insert : ( Int, Int ) -> v -> Grid v -> Grid v
insert position value (Grid grid) =
    Grid { grid | cells = Dict.insert (key position) value grid.cells }


remove : ( Int, Int ) -> Grid v -> Grid v
remove position (Grid grid) =
    Grid { grid | cells = Dict.remove (key position) grid.cells }


inBounds : ( Int, Int ) -> Grid a -> Bool
inBounds ( x, y ) (Grid grid) =
    x >= 0 && x < grid.x && y >= 0 && y < grid.y


keys : Grid a -> List ( Int, Int )
keys (Grid grid) =
    grid.cells
        |> Dict.keys
        |> List.map fromKey


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid grid) =
    grid.cells
        |> Dict.toList
        |> List.map (Tuple.mapFirst fromKey)
