module Cell exposing (Cell, color, isCenter, isSettled, settle, size, spawn)

import Tetrimino exposing (Tetrimino)


type Cell
    = Cell Internal


type State
    = Settled
    | Active
    | Center


type alias Internal =
    { state : State, tetrimino : Tetrimino }


color : Cell -> String
color (Cell { tetrimino }) =
    Tetrimino.color tetrimino


settle : Cell -> Cell
settle (Cell cell) =
    Cell { cell | state = Settled }


spawn : Tetrimino -> List ( ( Int, Int ), Cell )
spawn tetrimino =
    Tetrimino.spawn tetrimino
        |> (List.map << Tuple.mapSecond)
            (\( t, center ) ->
                Cell
                    { state =
                        if center then
                            Center

                        else
                            Active
                    , tetrimino = t
                    }
            )


isSettled : Cell -> Bool
isSettled (Cell { state }) =
    state == Settled


isCenter : Cell -> Bool
isCenter (Cell { state }) =
    state == Center


size : Int
size =
    1
