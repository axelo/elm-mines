module Main exposing (main)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Set exposing (Set)


type alias Model =
    { grid : Grid
    , cheat : Bool
    }


type alias Grid =
    Dict Coord ( Square, Status )


type alias Coord =
    ( Int, Int )


type Square
    = Square Int
    | Bomb


type Status
    = NotCleared
    | Cleared
    | IsBomb


gridSquareSize =
    32


gridSize =
    9


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd msg )
init flags =
    ( { grid = makeGrid gridSize (Set.fromList [ ( 3, 3 ), ( 7, 7 ), ( 6, 7 ), ( 5, 7 ), ( 5, 6 ) ])
      , cheat = True
      }
    , Cmd.none
    )



-- HELPERS


adjacentCoords : Coord -> Set Coord
adjacentCoords ( x, y ) =
    Set.fromList
        [ ( x - 1, y - 1 )
        , ( x, y - 1 )
        , ( x + 1, y - 1 )
        , ( x - 1, y )
        , ( x + 1, y )
        , ( x - 1, y + 1 )
        , ( x, y + 1 )
        , ( x + 1, y + 1 )
        ]
        -- TODO remove hard coded gridSize
        |> Set.filter (\( x1, y1 ) -> x1 >= 0 && x1 < gridSize && y1 >= 0 && y1 < gridSize)


adjacentBombs : Coord -> Set Coord -> Int
adjacentBombs coord bombs =
    Set.intersect bombs (adjacentCoords coord) |> Set.size


makeGrid : Int -> Set Coord -> Grid
makeGrid size bombs =
    List.range 0 (size * size - 1)
        |> List.map
            (\i ->
                let
                    coord =
                        ( modBy size i, i // size )

                    square =
                        if Set.member coord bombs then
                            Bomb

                        else
                            Square (adjacentBombs coord bombs)
                in
                ( coord, ( square, NotCleared ) )
            )
        |> Dict.fromList


isSquareZeroAt grid coord =
    case Dict.get coord grid of
        Just ( Square 0, _ ) ->
            True

        _ ->
            False


connectedSquareZeros : Grid -> Set Coord -> Set Coord -> Set Coord
connectedSquareZeros grid coords visited =
    let
        nextVisisted =
            Set.union coords visited

        allAdjacentCoords =
            Set.foldl (\c a -> Set.union a (adjacentCoords c)) Set.empty coords
                |> Set.filter (isSquareZeroAt grid)

        nextCoords =
            Set.diff allAdjacentCoords nextVisisted
    in
    if Set.isEmpty nextCoords then
        nextVisisted

    else
        connectedSquareZeros grid nextCoords nextVisisted


type Msg
    = SquaredClicked Coord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SquaredClicked coord ->
            case Dict.get coord model.grid of
                Just ( square, status ) ->
                    case square of
                        Square 0 ->
                            let
                                squareZeros : Grid
                                squareZeros =
                                    connectedSquareZeros model.grid (Set.singleton coord) Set.empty
                                        |> Set.toList
                                        |> List.map (\c -> ( c, ( Square 0, Cleared ) ))
                                        |> Dict.fromList

                                grid =
                                    Dict.union squareZeros model.grid
                            in
                            ( { model | grid = grid }, Cmd.none )

                        _ ->
                            let
                                grid =
                                    Dict.insert coord ( square, Cleared ) model.grid
                            in
                            ( { model | grid = grid }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewGrid model.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    let
        addSquare coord ( square, status ) views =
            viewAtGrid coord
                (case status of
                    NotCleared ->
                        viewNotClearedSquare coord square

                    Cleared ->
                        viewClearedSquare square

                    IsBomb ->
                        div [] [ text "todo!" ]
                )
                :: views
    in
    div [ class "relative p-2 cursor-default" ] (Dict.foldl addSquare [] grid)


viewAtGrid : Coord -> Html msg -> Html msg
viewAtGrid ( x, y ) viewChild =
    let
        translate =
            "translate(" ++ String.fromInt (x * gridSquareSize) ++ "px," ++ String.fromInt (y * gridSquareSize) ++ "px)"
    in
    div
        [ class "absolute w-8 h-8"
        , style "transform" translate
        ]
        [ viewChild ]


viewNotClearedSquare : Coord -> Square -> Html Msg
viewNotClearedSquare coord square =
    let
        d =
            case square of
                Square 0 ->
                    ""

                Square n ->
                    String.fromInt n

                Bomb ->
                    "B"
    in
    div
        [ class "w-full h-full bg-grey-light hover:bg-grey-lighter"
        , style "border-width" "2px"
        , style "border-color" "white"
        , style "border-right-color" "#8795a1"
        , style "border-bottom-color" "#8795a1"
        , onClick (SquaredClicked coord)
        ]
        [ text d ]


viewClearedSquare : Square -> Html msg
viewClearedSquare square =
    let
        numToColor num =
            case num of
                1 ->
                    "text-blue"

                2 ->
                    "text-green-dark"

                3 ->
                    "text-red"

                4 ->
                    "text-blue-darker"

                5 ->
                    "text-red-darker"

                _ ->
                    "text-black"

        viewSquareContainer caption color =
            div [ class ("flex items-center justify-center w-full h-full bg-grey-light text-2xl font-mono font-bold border-grey-dark border-t-2 border-l-2 " ++ color) ] [ text caption ]
    in
    case square of
        Square 0 ->
            viewSquareContainer "" "text-black"

        Square num ->
            viewSquareContainer (String.fromInt num) (numToColor num)

        Bomb ->
            viewSquareContainer "💣" "text-black"
