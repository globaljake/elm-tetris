module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Cell exposing (Cell)
import Dict
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
    , state : State
    , lines : Int
    }


type State
    = Playing
    | Settling
    | GameOver


type Foul
    = OutOfBounds
    | CellTaken


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { gridState = Grid.empty ( 10, 20 )
      , state = Playing
      , lines = 0
      }
    , Random.generate Spawn Tetrimino.random
    )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Elm Tetris"
    , body = [ viewRoot model ]
    }


viewRoot : Model -> Html Msg
viewRoot model =
    Html.div [ Attributes.class "container text-center h-full" ]
        [ Html.h1 [ Attributes.class "font-semibold text-xl sm:text-3xl m-2 sm:m-5" ]
            [ Html.text "Elm Tetris"
            ]
        , viewBoard model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    Html.div [ Attributes.class "flex justify-center font-mono" ]
        [ Html.div [ Attributes.class "fill-current w-full max-w-xs mx-12 relative" ]
            [ Svg.svg
                [ Grid.dimensions model.gridState
                    |> (\( x, y ) -> [ 0, 0, x * Cell.size, y * Cell.size ])
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , Svg.Attributes.width "100%"
                , Svg.Attributes.preserveAspectRatio "xMidYMid meet"
                ]
                (List.map (viewCell model.gridState) (Grid.coordinates model.gridState))
            , viewMobileControls
            , viewOverlay model
            ]
        ]


viewCell : Grid Cell -> ( Int, Int ) -> Svg Msg
viewCell gridState ( x, y ) =
    Svg.rect
        [ Svg.Attributes.class
            (String.join " "
                [ "fill-current"
                , Grid.get ( x, y ) gridState
                    |> Maybe.map Cell.color
                    |> Maybe.withDefault "text-gray-300"
                ]
            )
        , Svg.Attributes.width (String.fromInt Cell.size)
        , Svg.Attributes.height (String.fromInt Cell.size)
        , Svg.Attributes.strokeWidth (String.fromFloat (0.05 * toFloat Cell.size))
        , Svg.Attributes.stroke "#edf2f7"
        , Svg.Attributes.x (String.fromInt (x * Cell.size))
        , Svg.Attributes.y (String.fromInt (y * Cell.size))
        ]
        []


viewMobileControls : Html Msg
viewMobileControls =
    Html.div [ Attributes.class "absolute inset-0 flex flex-col opacity-75 sm:hidden" ]
        [ Html.div [ Attributes.class "flex flex-1" ]
            [ Html.button
                [ Attributes.class "h-full w-full"
                , Events.onMouseDown Rotate
                ]
                []
            ]
        , Html.div [ Attributes.class "flex h-32" ]
            [ Html.button
                [ Attributes.class "h-full w-2/5"
                , Events.onMouseDown MoveLeft
                ]
                [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "👈" ]
                ]
            , Html.button
                [ Attributes.class "h-full w-1/5"
                , Events.onMouseDown Advance
                ]
                [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "👇" ]
                ]
            , Html.button
                [ Attributes.class "h-full w-2/5"
                , Events.onMouseDown MoveRight
                ]
                [ Html.span [ Attributes.class "flex flex-col text-3xl" ] [ Html.text "👉" ]
                ]
            ]
        ]


viewOverlay : Model -> Html Msg
viewOverlay model =
    case model.state of
        GameOver ->
            Html.div
                [ Attributes.class "absolute inset-0 flex flex-col justify-center items-center bg-black text-white"
                ]
                [ Html.button
                    [ Attributes.class "p-10"
                    , Events.onClick Restart
                    ]
                    [ Html.span [ Attributes.class "flex flex-col" ]
                        [ Html.span [ Attributes.class "text-xl font-medium" ]
                            [ Html.text <|
                                String.join " " [ String.fromInt model.lines, "Lines" ]
                            ]
                        , Html.span [ Attributes.class "text-3xl font-medium" ]
                            [ Html.text "Game Over"
                            ]
                        , Html.span [] [ Html.text "Restart?" ]
                        ]
                    ]
                ]

        _ ->
            Html.div
                [ Attributes.class "absolute top-0 left-0"
                ]
                [ Html.span [ Attributes.class "leading-none text-2xl flex p-4" ]
                    [ Html.text (String.fromInt model.lines)
                    ]
                ]



-- UPDATE


type Msg
    = Spawn Tetrimino
    | MoveLeft
    | MoveRight
    | Rotate
    | Advance
    | Place
    | SettleBoard (List Int)
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spawn tetrimino ->
            ( spawn tetrimino model, Cmd.none )

        MoveLeft ->
            ( move Grid.left model, Cmd.none )

        MoveRight ->
            ( move Grid.right model, Cmd.none )

        Rotate ->
            ( rotate model, Cmd.none )

        Advance ->
            advance model

        Place ->
            place model

        SettleBoard linesToClear ->
            ( { model
                | gridState = List.foldl settle model.gridState linesToClear
                , state = Playing
                , lines = model.lines + List.length linesToClear
              }
            , Random.generate Spawn Tetrimino.random
            )

        Restart ->
            ( { gridState = Grid.empty ( 10, 20 )
              , state = Playing
              , lines = 0
              }
            , Random.generate Spawn Tetrimino.random
            )


spawn : Tetrimino -> Model -> Model
spawn tetrimino model =
    case spawnHelp 1 (Cell.spawn tetrimino) model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err foul ->
            { model | state = GameOver }


spawnHelp : Int -> List ( ( Int, Int ), Cell ) -> Grid Cell -> Result Foul (Grid Cell)
spawnHelp count cells grid =
    case groupInsert cells grid of
        ( Just CellTaken, _ ) ->
            if count > 0 then
                spawnHelp (count - 1) (List.map (Tuple.mapFirst Grid.up) cells) grid

            else
                Err CellTaken

        ( _, newGrid ) ->
            Ok newGrid


move : (( Int, Int ) -> ( Int, Int )) -> Model -> Model
move f model =
    case moveHelp f model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err foul ->
            model


place : Model -> ( Model, Cmd Msg )
place model =
    let
        noActive =
            Grid.filter (\_ -> not << Cell.isSettled) model.gridState
                |> Grid.positions
                |> List.isEmpty
    in
    if noActive then
        ( model, Cmd.none )

    else
        clearLines { model | gridState = placeHelp model.gridState }


placeHelp : Grid Cell -> Grid Cell
placeHelp grid =
    case moveHelp Grid.down grid of
        Ok newGrid ->
            placeHelp newGrid

        Err foul ->
            grid


advance : Model -> ( Model, Cmd Msg )
advance model =
    case moveHelp Grid.down model.gridState of
        Ok newGrid ->
            ( { model | gridState = newGrid }, Cmd.none )

        Err foul ->
            clearLines model


moveHelp : (( Int, Int ) -> ( Int, Int )) -> Grid Cell -> Result Foul (Grid Cell)
moveHelp f grid =
    let
        activePositionMap =
            Grid.filter (\_ -> not << Cell.isSettled) grid
                |> Grid.positions
                |> List.map (\pos -> ( pos, f pos ))
    in
    case groupUpdate activePositionMap grid of
        ( Nothing, newGrid ) ->
            Ok newGrid

        ( Just foul, _ ) ->
            Err foul


clearLines : Model -> ( Model, Cmd Msg )
clearLines model =
    let
        ( gridX, _ ) =
            Grid.dimensions model.gridState

        linesToClear =
            List.foldl
                (\( _, y ) -> Dict.update y (\mv -> Just (Maybe.withDefault 0 mv + 1)))
                Dict.empty
                (Grid.positions model.gridState)
                |> Dict.toList
                |> List.filter (\( _, count ) -> count == gridX)
                |> List.map Tuple.first
                |> List.sort

        gridAfterClear =
            List.foldl
                (\lineNumber -> Grid.filter (\pos _ -> Tuple.second pos /= lineNumber))
                model.gridState
                linesToClear
    in
    ( { model
        | gridState = Grid.map (\_ -> Cell.settle) gridAfterClear
        , state = Settling
      }
    , Process.sleep 200
        |> Task.perform (\_ -> SettleBoard linesToClear)
    )


settle : Int -> Grid Cell -> Grid Cell
settle yPos grid =
    let
        aboveClearedPositionMap =
            Grid.positions grid
                |> List.filterMap
                    (\pos ->
                        if Tuple.second pos < yPos then
                            Just ( pos, Grid.down pos )

                        else
                            Nothing
                    )
    in
    case groupUpdate aboveClearedPositionMap grid of
        ( Nothing, newGrid ) ->
            newGrid

        ( Just foul, _ ) ->
            grid


rotate : Model -> Model
rotate model =
    case rotateHelp model.gridState of
        Ok newGrid ->
            { model | gridState = newGrid }

        Err _ ->
            model


rotateHelp : Grid Cell -> Result Foul (Grid Cell)
rotateHelp grid =
    let
        activeCells =
            Grid.filter (\_ -> not << Cell.isSettled) grid
                |> Grid.toList

        center =
            List.filter (Cell.isCenter << Tuple.second) activeCells
                |> List.head

        rotatePositionMap =
            activeCells
                |> List.map
                    (\( pos, _ ) ->
                        ( pos
                        , case center of
                            Just ( cPos, _ ) ->
                                Tetrimino.rotateAround pos cPos

                            Nothing ->
                                pos
                        )
                    )
    in
    case groupUpdate rotatePositionMap grid of
        ( Nothing, newGrid ) ->
            Ok newGrid

        ( Just foul, _ ) ->
            Err foul


groupInsert : List ( ( Int, Int ), Cell ) -> Grid Cell -> ( Maybe Foul, Grid Cell )
groupInsert cells grid =
    ( foulCheck (List.map Tuple.first cells) grid
    , List.foldl (\( pos, cell ) -> Grid.insert pos cell) grid cells
    )


groupUpdate : List ( ( Int, Int ), ( Int, Int ) ) -> Grid Cell -> ( Maybe Foul, Grid Cell )
groupUpdate positionMap grid =
    let
        ( oldPositions, newPositions ) =
            List.unzip positionMap

        gridWithoutOld =
            Grid.filter (\pos _ -> not <| List.member pos oldPositions) grid
    in
    ( foulCheck newPositions gridWithoutOld
    , positionMap
        |> List.foldl
            (\( pos, newPos ) newGrid ->
                case
                    ( Grid.get pos grid, List.member pos newPositions )
                of
                    ( Just cell, True ) ->
                        Grid.insert newPos cell newGrid

                    ( Just cell, False ) ->
                        Grid.remove pos newGrid
                            |> Grid.insert newPos cell

                    ( Nothing, _ ) ->
                        newGrid
            )
            grid
    )


foulCheck : List ( Int, Int ) -> Grid Cell -> Maybe Foul
foulCheck positions grid =
    let
        ( gridX, gridY ) =
            Grid.dimensions grid

        outOfBounds ( x, y ) =
            x < 0 || x >= gridX || y >= gridY
    in
    if List.any (\pos -> Grid.member pos grid) positions then
        Just CellTaken

    else if List.any outOfBounds positions then
        Just OutOfBounds

    else
        Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        gameSpeed =
            700 - (model.lines * 4)
    in
    Sub.batch
        [ Time.every (toFloat gameSpeed) (\_ -> Advance)
        , Browser.Events.onKeyDown
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowLeft" ->
                                Decode.succeed MoveLeft

                            "ArrowRight" ->
                                Decode.succeed MoveRight

                            "ArrowUp" ->
                                Decode.succeed Rotate

                            "ArrowDown" ->
                                Decode.succeed Advance

                            " " ->
                                Decode.succeed Place

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
