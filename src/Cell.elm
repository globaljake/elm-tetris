module Cell exposing (Cell, color, inactivate, isActive, spawn)

import Tetrimino exposing (Tetrimino)


type Cell
    = Cell Internal


type alias Internal =
    { active : Bool, tetrimino : Tetrimino }


color : Cell -> String
color (Cell { tetrimino }) =
    Tetrimino.color tetrimino


inactivate : Cell -> Cell
inactivate (Cell cell) =
    Cell { cell | active = False }


spawn : Tetrimino -> List ( ( Int, Int ), Cell )
spawn tetrimino =
    Tetrimino.spawn tetrimino
        |> (List.map << Tuple.mapSecond) (\t -> Cell { active = True, tetrimino = t })


isActive : Cell -> Bool
isActive (Cell { active }) =
    active
