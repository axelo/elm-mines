module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Html.Events as Events exposing (onClick, onDoubleClick, preventDefaultOn)
import Json.Decode as Decode
import Set exposing (Set)
import Task
import Time


type alias Model =
    { grid : Grid
    , cheat : Bool
    , playState : PlayState
    , startTime : Time.Posix
    , currentTime : Time.Posix
    }


type PlayState
    = Running
    | GameOver
    | Won


type alias Coord =
    ( Int, Int )


type alias GridSquare =
    { kind : SquareKind
    , status : SquareStatus
    }


type alias Grid =
    Dict Coord GridSquare


type SquareKind
    = Square0
    | Square1
    | Square2
    | Square3
    | Square4
    | Square5
    | Square6
    | Square7
    | Square8
    | ABomb BombStatus


type SquareStatus
    = Revealed
    | RevealedAndMarked
    | Unrevealed
    | UnrevealedAndMarked


type BombStatus
    = Ticking
    | Detonated


gridSquareSize =
    32


gridSize =
    9


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { -- grid = makeGrid gridSize gridSize (Set.fromList [ ( 3, 4 ), ( 5, 4 ) ])
        grid = makeGrid gridSize gridSize (Set.fromList [ ( 0, 4 ), ( 3, 4 ), ( 4, 4 ), ( 5, 4 ), ( 7, 6 ), ( 9, 9 ), ( 9, 12 ), ( 6, 15 ), ( 3, 15 ), ( 0, 15 ), ( 6, 1 ) ])
      , cheat = False
      , playState = Running
      , startTime = Time.millisToPosix 0
      , currentTime = Time.millisToPosix 0
      }
    , Task.perform GotStartTime Time.now
    )



-- HELPERS


timeSpent : Model -> Int
timeSpent model =
    (Time.posixToMillis model.currentTime - Time.posixToMillis model.startTime) // 1000


adjacentCoords8 : Coord -> Set Coord
adjacentCoords8 ( x, y ) =
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
    Set.intersect bombs (adjacentCoords8 coord) |> Set.size


makeGrid : Int -> Int -> Set Coord -> Grid
makeGrid width height bombs =
    List.range 0 (width * height - 1)
        |> List.map
            (\i ->
                let
                    coord =
                        ( modBy width i, i // width )

                    kind =
                        if Set.member coord bombs then
                            ABomb Ticking

                        else
                            case adjacentBombs coord bombs of
                                1 ->
                                    Square1

                                2 ->
                                    Square2

                                3 ->
                                    Square3

                                4 ->
                                    Square4

                                5 ->
                                    Square5

                                6 ->
                                    Square6

                                7 ->
                                    Square7

                                8 ->
                                    Square8

                                _ ->
                                    Square0
                in
                ( coord, GridSquare kind Unrevealed )
            )
        |> Dict.fromList


isSquareZeroUnrevealedAt : Grid -> Coord -> Bool
isSquareZeroUnrevealedAt grid coord =
    case Dict.get coord grid of
        Just { kind, status } ->
            kind == Square0 && status == Unrevealed

        _ ->
            False


isSquareZeroRevealedAt : Grid -> Coord -> Bool
isSquareZeroRevealedAt grid coord =
    case Dict.get coord grid of
        Just { kind, status } ->
            kind == Square0 && status == Revealed

        _ ->
            False


countUnrevealedSquared : Grid -> Int
countUnrevealedSquared grid =
    Dict.foldl
        (\_ v count ->
            if v.status == Unrevealed || v.status == UnrevealedAndMarked then
                count + 1

            else
                count
        )
        0
        grid


countBombs : Grid -> Int
countBombs grid =
    Dict.foldl
        (\_ v count ->
            case v.kind of
                ABomb _ ->
                    count + 1

                _ ->
                    count
        )
        0
        grid


countMarked : Grid -> Int
countMarked grid =
    Dict.foldl
        (\_ v count ->
            case v.status of
                RevealedAndMarked ->
                    count + 1

                UnrevealedAndMarked ->
                    count + 1

                _ ->
                    count
        )
        0
        grid


allBombsFound : Grid -> Bool
allBombsFound grid =
    countBombs grid == countUnrevealedSquared grid



-- Stackframe version
-- floodFill : Coord -> Grid -> Grid
-- floodFill coord grid =
--     if isSquareZeroNotClearedAt grid coord then
--         let
--             ( x, y ) =
--                 coord
--         in
--         Dict.insert coord ( Square 0, Cleared ) grid
--             |> floodFill ( x - 1, y )
--             |> floodFill ( x + 1, y )
--             |> floodFill ( x, y - 1 )
--             |> floodFill ( x, y + 1 )
--     else
--         grid


floodFillScanline : Coord -> Grid -> Grid
floodFillScanline coord grid =
    if isSquareZeroUnrevealedAt grid coord then
        floodFillScanlineHelp grid [ coord ]

    else
        grid


floodFillScanlineHelp : Grid -> List Coord -> Grid
floodFillScanlineHelp grid queue =
    case queue of
        [] ->
            grid

        coord :: rest ->
            let
                westCoord =
                    findScanlineEndpoint coord -1 grid

                eastCoord =
                    findScanlineEndpoint coord 1 grid

                y =
                    Tuple.second coord

                westToEastCoords =
                    List.range (Tuple.first westCoord) (Tuple.first eastCoord)
                        |> List.map (\x -> ( x, y ))

                westToEastCoordsNorth =
                    westToEastCoords
                        |> List.map (Tuple.mapSecond ((+) -1))
                        |> List.filter (isSquareZeroUnrevealedAt grid)
                        |> startOfConnectedCoords

                westToEastCoordsSouth =
                    westToEastCoords
                        |> List.map (Tuple.mapSecond ((+) 1))
                        |> List.filter (isSquareZeroUnrevealedAt grid)
                        |> startOfConnectedCoords

                updatedGrid : Grid
                updatedGrid =
                    Dict.union
                        (westToEastCoords
                            |> List.map (\wecoord -> ( wecoord, GridSquare Square0 Revealed ))
                            |> Dict.fromList
                        )
                        grid
            in
            floodFillScanlineHelp updatedGrid (rest ++ westToEastCoordsNorth ++ westToEastCoordsSouth)


findScanlineEndpoint : Coord -> Int -> Grid -> Coord
findScanlineEndpoint ( x, y ) direction grid =
    if isSquareZeroUnrevealedAt grid ( x + direction, y ) then
        findScanlineEndpoint ( x + direction, y ) direction grid

    else
        ( x, y )


startOfConnectedCoords : List Coord -> List Coord
startOfConnectedCoords coords =
    case coords of
        [] ->
            []

        [ first ] ->
            [ first ]

        ( firstX, _ ) :: rest ->
            let
                foldStartCoords ( x, y ) ( prevX, startCoords ) =
                    ( x
                    , if x - prevX <= 0 then
                        startCoords

                      else
                        ( x, y ) :: startCoords
                    )
            in
            rest
                |> List.foldl foldStartCoords ( firstX, [] )
                |> Tuple.second


clearNonEmptySquaresAtBorderOfEmpty : Grid -> Grid
clearNonEmptySquaresAtBorderOfEmpty grid =
    let
        tryReveal ( x, y ) gridSquare =
            if
                gridSquare.kind
                    == Square0
                    || gridSquare.status
                    == Revealed
                    || gridSquare.status
                    == UnrevealedAndMarked
            then
                gridSquare

            else if
                isSquareZeroRevealedAt grid ( x - 1, y )
                    || isSquareZeroRevealedAt grid ( x + 1, y )
                    || isSquareZeroRevealedAt grid ( x, y + 1 )
                    || isSquareZeroRevealedAt grid ( x, y - 1 )
            then
                GridSquare gridSquare.kind Revealed

            else
                gridSquare
    in
    Dict.map tryReveal grid


markAllUnrevealedBombs : Grid -> Grid
markAllUnrevealedBombs grid =
    Dict.map
        (\c v ->
            if v.kind == ABomb Ticking && v.status == Unrevealed then
                GridSquare v.kind UnrevealedAndMarked

            else
                v
        )
        grid


type KeyPressDirection
    = KeyUp
    | KeyDown


type Msg
    = SquaredClicked Coord Bool
    | KeyPressed KeyPressDirection String
    | GotStartTime Time.Posix
    | GotCurrentTime Time.Posix


updatePlaystate : Model -> Grid -> Model
updatePlaystate model grid =
    if allBombsFound grid then
        { model | grid = markAllUnrevealedBombs grid, playState = Won }

    else
        { model | grid = grid, playState = Running }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        GotStartTime startTime ->
            ( { model | startTime = startTime, currentTime = startTime }, Cmd.none )

        GotCurrentTime currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )

        KeyPressed dir key ->
            let
                updatedModel =
                    case String.toLower key of
                        "c" ->
                            { model | cheat = dir == KeyDown }

                        _ ->
                            model
            in
            ( updatedModel, Cmd.none )

        SquaredClicked coord withShiftKey ->
            case model.playState of
                Running ->
                    case withShiftKey of
                        False ->
                            case Dict.get coord model.grid of
                                Just { kind, status } ->
                                    if status == Unrevealed then
                                        case kind of
                                            Square0 ->
                                                let
                                                    clearedGrid =
                                                        floodFillScanline coord model.grid

                                                    updatedGrid =
                                                        clearNonEmptySquaresAtBorderOfEmpty clearedGrid
                                                in
                                                ( updatePlaystate model updatedGrid, Cmd.none )

                                            ABomb _ ->
                                                let
                                                    updatedGrid : Grid
                                                    updatedGrid =
                                                        Dict.map
                                                            (\c v ->
                                                                if c == coord then
                                                                    GridSquare (ABomb Detonated) Revealed

                                                                else if v.kind == ABomb Ticking && v.status == Unrevealed then
                                                                    GridSquare v.kind Revealed

                                                                else if v.kind /= ABomb Ticking && v.status == UnrevealedAndMarked then
                                                                    GridSquare v.kind RevealedAndMarked

                                                                else
                                                                    v
                                                            )
                                                            model.grid
                                                in
                                                ( { model | grid = updatedGrid, playState = GameOver }, Cmd.none )

                                            _ ->
                                                let
                                                    updatedGrid =
                                                        Dict.insert coord (GridSquare kind Revealed) model.grid
                                                in
                                                ( updatePlaystate model updatedGrid, Cmd.none )

                                    else
                                        ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        True ->
                            case Dict.get coord model.grid of
                                Just gridSquare ->
                                    case gridSquare.status of
                                        Unrevealed ->
                                            ( { model | grid = Dict.insert coord { gridSquare | status = UnrevealedAndMarked } model.grid }, Cmd.none )

                                        UnrevealedAndMarked ->
                                            ( { model | grid = Dict.insert coord { gridSquare | status = Unrevealed } model.grid }, Cmd.none )

                                        _ ->
                                            ( model, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                GameOver ->
                    ( model, Cmd.none )

                Won ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| Decode.map (KeyPressed KeyDown) (Decode.field "key" Decode.string)
        , Browser.Events.onKeyUp <| Decode.map (KeyPressed KeyUp) (Decode.field "key" Decode.string)
        , if model.playState == Running then
            Time.every 1000 GotCurrentTime

          else
            Sub.none
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "h-full flex flex-col items-center justify-center" ]
        [ div [ style "width" (String.fromInt (gridSquareSize * gridSize + 2) ++ "px") ]
            [ viewHud model
            , viewGrid model.grid model.cheat model.playState
            ]
        ]


viewHud : Model -> Html msg
viewHud model =
    div
        [ class "flex items-center justify-between bg-grey-light"
        ]
        [ viewBombsLeftAccordingToThePlayer model
        , viewPlayer model
        , viewTimer model
        ]


viewPlayer : Model -> Html msg
viewPlayer model =
    let
        avatar =
            case model.playState of
                Running ->
                    case modBy 10 (timeSpent model) of
                        0 ->
                            "😑"

                        _ ->
                            "😐"

                GameOver ->
                    "😵"

                Won ->
                    "\u{1F929}"
    in
    div [ class "text-3xl emoji-valign h-8" ] [ text avatar ]


viewBombsLeftAccordingToThePlayer model =
    viewDigits (countBombs model.grid - countMarked model.grid)


viewTimer model =
    viewDigits (timeSpent model)


viewDigits : Int -> Html msg
viewDigits digits =
    div
        [ class "relative w-16 h-8 flex items-center bg-black"
        , style "font-family" "DSEG7Classic"
        , style "font-size" "1.5rem"
        ]
        [ span
            [ class "absolute pin-r text-red-dark opacity-25"
            , style "padding-right" "0.125rem"
            ]
            [ text "888" ]
        , span
            [ class "absolute pin-r text-red"
            , style "padding-right" "0.125rem"
            ]
            [ clamp -99 999 digits |> String.fromInt |> text ]
        ]


viewGrid : Grid -> Bool -> PlayState -> Html Msg
viewGrid grid cheat playState =
    let
        addSquare coord { kind, status } views =
            viewAtGrid coord
                (case status of
                    Revealed ->
                        viewClearedSquare kind False

                    RevealedAndMarked ->
                        viewClearedSquare kind True

                    Unrevealed ->
                        viewNotClearedSquare coord kind False cheat playState

                    UnrevealedAndMarked ->
                        viewNotClearedSquare coord kind True cheat playState
                )
                :: views
    in
    div
        [ class "relative cursor-default border-grey-dark border-r-2 border-b-2 select-none"
        , style "width" (String.fromInt (gridSquareSize * gridSize + 2) ++ "px")
        , style "height" (String.fromInt (gridSquareSize * gridSize + 2) ++ "px")
        ]
        (Dict.foldl addSquare [] grid)


viewAtGrid : Coord -> Html msg -> Html msg
viewAtGrid ( x, y ) viewChild =
    let
        translate =
            "translate(" ++ String.fromInt (x * gridSquareSize) ++ "px," ++ String.fromInt (y * gridSquareSize) ++ "px)"

        width =
            gridSquareSize
    in
    div
        [ class "absolute"
        , style "transform" translate
        , style "width" (String.fromInt width ++ "px")
        , style "height" (String.fromInt gridSquareSize ++ "px")
        ]
        [ viewChild ]


viewNotClearedSquare : Coord -> SquareKind -> Bool -> Bool -> PlayState -> Html Msg
viewNotClearedSquare coord kind marked cheat playState =
    let
        viewCheat =
            if cheat then
                div [ class "absolute pin flex items-center justify-center" ]
                    [ text
                        (case kind of
                            Square0 ->
                                ""

                            Square1 ->
                                "1"

                            Square2 ->
                                "2"

                            Square3 ->
                                "3"

                            Square4 ->
                                "4"

                            Square5 ->
                                "5"

                            Square6 ->
                                "6"

                            Square7 ->
                                "7"

                            Square8 ->
                                "8"

                            ABomb _ ->
                                "B"
                        )
                    ]

            else
                text ""
    in
    div
        [ class
            ("w-full h-full bg-grey-light text-2xl relative"
                ++ (if playState == Running then
                        " hover:bg-grey-lighter"

                    else
                        ""
                   )
            )
        , style "border-width" "2px"
        , style "border-color" "white"
        , style "border-right-color" "#8795a1"
        , style "border-bottom-color" "#8795a1"
        , Events.on "click" (Decode.map (SquaredClicked coord) (Decode.field "shiftKey" Decode.bool))
        ]
        [ if marked then
            div [ style "margin-top" "0.125rem", style "margin-left" "0.125rem" ] [ text "⛳" ]

          else
            text ""
        , viewCheat
        ]


viewClearedSquare : SquareKind -> Bool -> Html Msg
viewClearedSquare kind marked =
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
            div
                [ class ("relative border-grey-dark border-t-2 border-l-2 flex items-center justify-center w-full h-full bg-grey-light text-2xl font-mono font-bold " ++ color)

                -- Prevent dbl tap zoom on iOS
                , preventDefaultOn "click" (Decode.succeed ( KeyPressed KeyDown "!", True ))
                ]
                [ span [] [ text caption ]
                , span [ class "absolute flex items-center justify-center ml-1 mt-1 pin text-base" ]
                    [ text
                        (if marked then
                            "❌"

                         else
                            ""
                        )
                    ]
                ]
    in
    case kind of
        Square0 ->
            viewSquareContainer "" "text-black"

        Square1 ->
            viewSquareContainer "1" (numToColor 1)

        Square2 ->
            viewSquareContainer "2" (numToColor 2)

        Square3 ->
            viewSquareContainer "3" (numToColor 3)

        Square4 ->
            viewSquareContainer "4" (numToColor 4)

        Square5 ->
            viewSquareContainer "5" (numToColor 5)

        Square6 ->
            viewSquareContainer "6" (numToColor 6)

        Square7 ->
            viewSquareContainer "7" (numToColor 7)

        Square8 ->
            viewSquareContainer "8" (numToColor 8)

        ABomb Ticking ->
            viewSquareContainer "💣" "text-black"

        ABomb Detonated ->
            viewSquareContainer "💥" "text-black"
