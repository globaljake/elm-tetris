module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Cell exposing (Cell)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode exposing (Value)
import Process
import Svg exposing (Svg)
import Svg.Attributes
import Task
import Time


type alias Model =
    { gridState : Grid Cell
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { gridState = Grid.empty ( 10, 20 )
      }
    , Process.sleep 1
        |> Task.perform (\_ -> Spawn)
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
        [ Html.h1 [ Attributes.class "font-semibold text-xl sm:text-3xl m-6" ]
            [ Html.text "This is going to be Tetris"
            ]
        , viewBoard model.gridState
        ]


viewBoard : Grid Cell -> Html Msg
viewBoard gridState =
    Html.div [ Attributes.class "flex justify-center" ]
        [ Html.div [ Attributes.class "fill-current w-full max-w-xs" ]
            [ Svg.svg
                [ Grid.dimentions gridState
                    |> (\( x, y ) -> [ 0, 0, x * cellSize, y * cellSize ])
                    |> List.map String.fromInt
                    |> String.join " "
                    |> Svg.Attributes.viewBox
                , Svg.Attributes.width "100%"
                , Svg.Attributes.preserveAspectRatio "xMidYMin meet"
                ]
                (List.map (viewCell gridState) (Grid.coordinates gridState))
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
    = Spawn
    | MoveDown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Spawn ->
            case Grid.batchInsert (Cell.spawn Cell.T) model.gridState of
                Ok grid ->
                    ( { model | gridState = grid }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        MoveDown ->
            case
                model.gridState
                    |> Grid.batchUpdate
                        (Grid.filter (\_ cell -> Cell.isActive cell) model.gridState
                            |> Grid.toList
                            |> List.map (\( pos, _ ) -> ( pos, Grid.down pos ))
                        )
            of
                Ok grid ->
                    ( { model | gridState = grid }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "MoveDown" err
                    in
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 499 (\_ -> MoveDown)
        , Time.every 5000 (\_ -> Spawn)
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
