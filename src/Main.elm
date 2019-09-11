module Main exposing (main)

import Browser exposing (Document)
import Grid exposing (Grid)
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode exposing (Value)
import Square exposing (Square(..))
import Svg exposing (Svg)
import Svg.Attributes
import Tetromino exposing (Tetromino)


type alias Model =
    { gridState : Grid Tetromino
    }


init : Value -> ( Model, Cmd Msg )
init flags =
    ( { gridState = Grid.init ( 10, 20 ) (Tetromino.add Tetromino.Z) }
    , Cmd.none
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


viewBoard : Grid Tetromino -> Html Msg
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


viewCell : Grid Tetromino -> ( Int, Int ) -> Svg Msg
viewCell gridState ( x, y ) =
    Svg.rect
        [ Svg.Attributes.class
            (case Grid.get ( x, y ) gridState of
                Just t ->
                    "fill-current " ++ Tetromino.color t

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
    = Ignored


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignored ->
            ( model, Cmd.none )


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- INTERNAL


cellSize : Int
cellSize =
    100
