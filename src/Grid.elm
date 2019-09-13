module Grid exposing
    ( Grid
    , batchInsert
    , batchUpdate
    , coordinates
    , dimentions
    , down
    , empty
    , filter
    , fromKey
    , get
    , init
    , insert
    , toList
    )

import Dict exposing (Dict)
import Tetromino exposing (Tetromino)


type Grid a
    = Grid (Internal a)


type alias Internal a =
    { x : Int, y : Int, cells : Dict String a }


empty : ( Int, Int ) -> Grid a
empty ( x, y ) =
    Grid { x = x, y = y, cells = Dict.empty }


down : ( Int, Int ) -> ( Int, Int )
down ( x, y ) =
    ( x, y + 1 )


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
get position (Grid grid) =
    Dict.get (key position) grid.cells


filter : (( Int, Int ) -> a -> Bool) -> Grid a -> Grid a
filter f (Grid grid) =
    Grid { grid | cells = Dict.filter (\k v -> f (fromKey k) v) grid.cells }


insert : ( Int, Int ) -> a -> Grid a -> Result String (Grid a)
insert position value grid =
    batchInsert [ ( position, value ) ] grid


batchInsert : List ( ( Int, Int ), a ) -> Grid a -> Result String (Grid a)
batchInsert positions (Grid grid) =
    if List.member False <| List.map (validate grid.cells << Tuple.first) positions then
        Err "already taken"

    else
        Ok <|
            Grid
                { grid
                    | cells =
                        positions
                            |> List.map (Tuple.mapFirst key)
                            |> Dict.fromList
                            |> Dict.union grid.cells
                }


update : ( Int, Int ) -> ( Int, Int ) -> Grid a -> Result String (Grid a)
update position newPosition grid =
    batchUpdate [ ( position, newPosition ) ] grid


batchUpdate : List ( ( Int, Int ), ( Int, Int ) ) -> Grid a -> Result String (Grid a)
batchUpdate positions (Grid grid) =
    let
        ---- update req
        -- give old position and new position
        -- only update if :
        -- old position is in the grid
        -- new position isn't taken in grid filtering out old position
        positionNotFound =
            List.foldl (\( pos, _ ) y -> Nothing == Dict.get (key pos) grid.cells) False positions

        newPositionAlreadyTaken =
            List.foldl
                (\( pos, newPos ) y ->
                    Nothing /= Dict.get (key newPos) (Dict.filter (\k _ -> key pos /= k) grid.cells)
                )
                False
                positions
    in
    if positionNotFound then
        Err "current position isnt in grid"

    else if newPositionAlreadyTaken then
        Err "already taken"

    else
        Ok <|
            Grid
                { grid
                    | cells =
                        positions
                            |> List.foldl
                                (\( position, newPosition ) acc ->
                                    case Dict.get (key position) grid.cells of
                                        Nothing ->
                                            acc

                                        Just value ->
                                            Dict.remove (key position) acc
                                )
                                grid.cells
                            |> (\cells ->
                                    positions
                                        |> List.foldl
                                            (\( position, newPosition ) acc ->
                                                case Dict.get (key position) grid.cells of
                                                    Nothing ->
                                                        acc

                                                    Just value ->
                                                        Dict.insert (key newPosition) value acc
                                            )
                                            cells
                               )
                }


toList : Grid a -> List ( ( Int, Int ), a )
toList (Grid grid) =
    grid.cells
        |> Dict.toList
        |> List.map (Tuple.mapFirst fromKey)


validate : Dict String a -> ( Int, Int ) -> Bool
validate cells location =
    cells
        |> Dict.toList
        |> List.map Tuple.first
        |> List.member (key location)
        |> not
