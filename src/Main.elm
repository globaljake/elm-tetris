module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Cell exposing (Cell)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Value)
import Process
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Tetrimino exposing (Tetrimino)
import Time


type alias Model =
    { gridState : Grid Cell
    , gameover : Bool
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { gridState = Grid.empty ( 10, 20 )
      , gameover = False
      }
    , Random.generate Spawn Tetrimino.random
    )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Tetris"
    , body = [ viewRoot model ]
    }


viewRoot : Model -> Html Msg
viewRoot model =
    Html.div [ Attributes.class "container text-center" ]
        [ Html.h1 [ Attributes.class "font-semibold text-xl sm:text-3xl m-2 sm:m-5" ]
            [ Html.text "This is going to be Tetris"
            ]
        , viewBoard model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Html.div [ Attributes.class "flex justify-center" ]
        [ Html.div [ Attributes.class "fill-current w-full max-w-xs mx-12 relative" ]
            [ Svg.svg
                [ Grid.dimentions model.gridState
                    |> (\( x, y ) -> [ 0, 0, x * cellSize, y * cellSize ])
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , Svg.Attributes.width "100%"
                , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
                ]
                (List.map (viewCell model.gridState) (Grid.coordinates model.gridState))
            , Html.div [ Attributes.class "absolute inset-0 flex flex-col opacity-25 sm:hidden" ]
                [ Html.div [ Attributes.class "flex flex-1" ]
                    [ Html.button
                        [ Attributes.class "h-full w-1/2"
                        , Events.onMouseDown MoveLeft
                        ]
                        [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "←" ]
                        ]
                    , Html.button
                        [ Attributes.class "h-full w-1/2"
                        , Events.onMouseDown MoveRight
                        ]
                        [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "→" ]
                        ]
                    ]
                , Html.div [ Attributes.class "flex h-24" ]
                    [ Html.button
                        [ Attributes.class "h-full w-full"
                        , Events.onMouseDown MoveDown
                        ]
                        [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "↓" ]
                        ]
                    ]
                ]
            , if model.gameover then
                Html.div [ Attributes.class "absolute inset-0 flex flex-col justify-center items-center bg-black text-white" ]
                    [ Html.button
                        [ Attributes.class "p-10"
                        , Events.onClick Restart
                        ]
                        [ Html.span [ Attributes.class "flex flex-col" ]
                            [ Html.span [ Attributes.class "text-3xl font-semibold" ]
                                [ Html.text "Game Over"
                                ]
                            , Html.span [] [ Html.text "Restart?" ]
                            ]
                        ]
                    ]

              else
                Html.text ""
            ]
        ]


viewCell : Grid Cell -> ( Int, Int ) -> Svg Msg
viewCell gridState ( x, y ) =
    Svg.rect
        [ Svg.Attributes.class
            (case Grid.get ( x, y ) gridState of
                Just cell ->
                    "fill-current " ++ Cell.color cell

                Nothing ->
                    "fill-current text-gray-300"
            )
        , Svg.Attributes.width (String.fromInt cellSize)
        , Svg.Attributes.height (String.fromInt cellSize)
        , Svg.Attributes.strokeWidth (String.fromFloat (0.05 * toFloat cellSize))
        , Svg.Attributes.stroke "#edf2f7"
        , Svg.Attributes.x (String.fromInt (x * cellSize))
        , Svg.Attributes.y (String.fromInt (y * cellSize))
        ]
        []



-- UPDATE


type Msg
    = Spawn Tetrimino
    | MoveLeft
    | MoveRight
    | MoveDown
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spawn tetrimino ->
            case spawn tetrimino (inactivateCells model.gridState) of
                Ok grid ->
                    ( { model | gridState = grid }
                    , Cmd.none
                    )

                Err err ->
                    ( { model
                        | gridState = inactivateCells model.gridState
                        , gameover = True
                      }
                    , Cmd.none
                    )

        MoveLeft ->
            case moveActiveCells Grid.left model.gridState of
                Ok grid ->
                    ( { model | gridState = grid }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        MoveRight ->
            case moveActiveCells Grid.right model.gridState of
                Ok grid ->
                    ( { model | gridState = grid }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        MoveDown ->
            case moveActiveCells Grid.down model.gridState of
                Ok grid ->
                    ( { model | gridState = grid }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | gridState = inactivateCells model.gridState }
                    , Random.generate Spawn Tetrimino.random
                    )

        Restart ->
            ( { gridState = Grid.empty ( 10, 20 )
              , gameover = False
              }
            , Random.generate Spawn Tetrimino.random
            )


spawn : Tetrimino -> Grid Cell -> Result String (Grid Cell)
spawn tetrimino grid =
    let
        positions =
            Cell.spawn tetrimino

        positionsMovedUp =
            List.map (Tuple.mapFirst Grid.up) positions
    in
    if List.any (\pos -> Grid.member pos grid) (List.map Tuple.first positions) then
        if List.any (\pos -> Grid.member pos grid) (List.map Tuple.first positionsMovedUp) then
            Err "Game Over"

        else
            Ok (List.foldl (\( pos, cell ) -> Grid.insert pos cell) grid positionsMovedUp)

    else
        Ok (List.foldl (\( pos, cell ) -> Grid.insert pos cell) grid positions)


updatePosition : List ( ( Int, Int ), ( Int, Int ) ) -> Grid Cell -> Result String (Grid Cell)
updatePosition positions grid =
    let
        oldPositions =
            List.map Tuple.first positions

        newPositions =
            List.map Tuple.second positions

        filteredGrid =
            Grid.filter (\k _ -> not <| List.member k oldPositions) grid

        updatedGrid =
            positions
                |> List.foldl
                    (\( position, newPosition ) acc ->
                        case
                            ( Grid.get position grid, List.member position newPositions )
                        of
                            ( Just value, True ) ->
                                Grid.insert newPosition value acc

                            ( Just value, False ) ->
                                Grid.remove position acc
                                    |> Grid.insert newPosition value

                            ( Nothing, _ ) ->
                                acc
                    )
                    grid
    in
    if not <| List.all (\pos -> Grid.member pos grid) oldPositions then
        Err "Update: Not all positions are currently in grid"

    else if List.any (\pos -> Grid.member pos filteredGrid) newPositions then
        Err "Update: Any of the new positions are taken"

    else if List.any (\pos -> not <| Grid.inBounds pos grid) newPositions then
        Err "Update: Out of bounds"

    else
        Ok updatedGrid


moveActiveCells : (( Int, Int ) -> ( Int, Int )) -> Grid Cell -> Result String (Grid Cell)
moveActiveCells move grid =
    updatePosition
        (Grid.filter (\_ cell -> Cell.isActive cell) grid
            |> Grid.keys
            |> List.map (\pos -> ( pos, move pos ))
        )
        grid


inactivateCells : Grid Cell -> Grid Cell
inactivateCells grid =
    Grid.filter (\_ cell -> Cell.isActive cell) grid
        |> Grid.toList
        |> List.foldl (\( pos, cell ) -> Grid.insert pos (Cell.inactivate cell)) grid


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameover then
        Sub.none

    else
        Sub.batch
            [ Time.every 500 (\_ -> MoveDown)
            , Browser.Events.onKeyDown
                (Decode.field "key" Decode.string
                    |> Decode.andThen
                        (\key ->
                            case key of
                                "ArrowLeft" ->
                                    Decode.succeed MoveLeft

                                "ArrowRight" ->
                                    Decode.succeed MoveRight

                                "ArrowDown" ->
                                    Decode.succeed MoveDown

                                _ ->
                                    Decode.fail ""
                        )
                )
            ]


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INTERNAL


cellSize : Int
cellSize =
    1
