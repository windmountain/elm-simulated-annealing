module Sudoku exposing (main)

{-| A third live example, chosen to be unlike either routing example: pure
constraint satisfaction, no notion of distance or time at all.

What's different here:

  - **The energy function only measures conflicts.** A solution is a full
    81-cell grid where every 3x3 box already contains each digit 1-9
    exactly once — by construction, never checked. Energy is just the
    count of row/column duplicates. It reaches exactly 0 when solved.
  - **The neighbor move enforces structure, not just the energy function
    does.** Every move swaps two non-given cells within the same box, so
    the box constraint can never be violated no matter how the search
    wanders — only rows and columns are ever in tension, which is exactly
    what `energy` measures.
  - **Randomness threads through problem setup, not just the search.**
    Unlike the other two examples, the starting solution itself needs a
    random choice: which shuffle of the missing digits goes into each
    box's empty cells. So a single seed is drawn once via `Cmd`
    (`GotSeed`), then used *purely* twice — first with `Random.step` to
    build the starting grid, then handed to `SimulatedAnnealing.init` to
    drive the search from there.
  - **It can get stuck.** Plain exponential cooling doesn't guarantee a
    solution for every puzzle in a finite run — annealing can settle into
    a local optimum with a few conflicts left. That's a real property of
    the algorithm, not a bug in this example; "Restart" draws a fresh seed
    and a fresh random fill.

A second panel implements the specific SA scheme from Rhyd Lewis's 2007
paper "Metaheuristics can solve sudoku puzzles" (see `lewis2007.pdf`,
doi:10.1007/s10732-007-9012-8), which differs from the first panel's
annealing in three ways the paper is precise about:

  - **The starting temperature is measured, not guessed.** `t0` is set to
    the standard deviation of the cost observed over a short unconditional
    random walk from the initial fill — no hand-picked constant.
  - **Cooling happens between Markov chains, not every iteration.**
    Temperature is held fixed for `ml = (non-fixed cells)^2` moves (a
    "chain"), then multiplied by a fixed cooling rate. That's still
    expressible as our library's `temperature : Int -> Float`, just as
    `t0 * rate ^ (iteration // ml)` instead of `t0 * rate ^ iteration`.
  - **Reheating is a host-level policy, not a library feature.** After 20
    chains with no improvement in the best solution, the paper resets the
    temperature to the original `t0` *and* generates a brand-new random
    initial solution. `SimulatedAnnealing.State` has no way to do either of
    those on its own — regenerating a solution is problem-specific, and
    "stagnant for N chains" isn't something a `State` tracks. So this panel
    watches for it in its own `Model` and, when triggered, just calls
    `SimulatedAnnealing.init` again with the same `Config` and a fresh
    grid — the library only ever needed to expose one pure `step` function
    for all of this to be buildable on top of it.

A third panel runs a plain backtracking solver on the same puzzle for
contrast: Sudoku has a well-known complete algorithm, so unlike the routing
examples, annealing isn't actually the right tool for it in practice —
backtracking finds the (unique) answer in one synchronous call, no
temperature and no chance of getting stuck. It's here to make that contrast
visible, not to suggest annealing is how you'd solve Sudoku for real.

-}

import Array exposing (Array)
import Browser
import Browser.Events
import Dict
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import List.Extra
import Random
import Set exposing (Set)
import SimulatedAnnealing exposing (Config, State)
import SimulatedAnnealing.Schedule as Schedule
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- PUZZLE


{-| Row-major, `.` for blank. 27 givens, unique solution (verified with a
backtracking solver before hardcoding it here).
-}
puzzleStringHard : String
puzzleStringHard =
    "..6.182....5...864.3....9...8...27......3......95...8...2....5.641...3....839.6.."

{-| Row-major, `.` for blank. 27 givens, unique solution (verified with a
backtracking solver before hardcoding it here).
-}
puzzleStringEasy : String
puzzleStringEasy =
    "5..4673.99.381.4271742.3...231976854857124.9.4963.8172....8926.782641..5.1....7.8"

puzzleString : String
puzzleString =
    puzzleStringEasy


type alias Grid =
    Array Int


parseCell : Char -> Int
parseCell c =
    if c == '.' then
        0

    else
        Char.toCode c - Char.toCode '0'


givenGrid : Grid
givenGrid =
    puzzleString |> String.toList |> List.map parseCell |> Array.fromList


isGiven : Array Bool
isGiven =
    Array.map (\v -> v /= 0) givenGrid


cellAt : Int -> Int -> Grid -> Int
cellAt r c grid =
    Array.get (r * 9 + c) grid |> Maybe.withDefault 0



-- BOXES


{-| The 9 boxes, each as its list of 9 flat cell positions (0-80).
-}
boxPositions : List (List Int)
boxPositions =
    List.range 0 2
        |> List.concatMap
            (\boxRow ->
                List.range 0 2
                    |> List.map
                        (\boxCol ->
                            List.range 0 2
                                |> List.concatMap
                                    (\dr ->
                                        List.range 0 2
                                            |> List.map (\dc -> (boxRow * 3 + dr) * 9 + (boxCol * 3 + dc))
                                    )
                        )
            )


freeCellsIn : List Int -> List Int
freeCellsIn positions =
    List.filter (\p -> not (Array.get p isGiven |> Maybe.withDefault False)) positions


missingDigitsIn : List Int -> Grid -> List Int
missingDigitsIn positions grid =
    let
        used =
            positions |> List.map (\p -> Array.get p grid |> Maybe.withDefault 0)
    in
    List.range 1 9 |> List.filter (\d -> not (List.member d used))


{-| Boxes with at least two free cells — the only ones a swap move can pick
from. (A box entirely full of givens contributes nothing to move on.)
-}
swappableBoxes : List (List Int)
swappableBoxes =
    boxPositions |> List.map freeCellsIn |> List.filter (\free -> List.length free >= 2)



-- RANDOM: filling in a legal starting grid


{-| Fisher-Yates via `List.Extra.getAt` / `removeAt` — box sizes are at most
9, so there's no need to reach for a dedicated shuffle package for this.
-}
shuffle : List a -> Random.Generator (List a)
shuffle list =
    case list of
        [] ->
            Random.constant []

        _ ->
            Random.int 0 (List.length list - 1)
                |> Random.andThen
                    (\i ->
                        case List.Extra.getAt i list of
                            Nothing ->
                                Random.constant list

                            Just picked ->
                                shuffle (List.Extra.removeAt i list) |> Random.map ((::) picked)
                    )


{-| Fill every box's empty cells with a random permutation of that box's
missing digits. Folding `Random.andThen` over the 9 boxes is the standard
way to chain a *different* generator for each box — `Random.list` only
helps when every element comes from the same generator.
-}
initialGridGenerator : Random.Generator Grid
initialGridGenerator =
    boxPositions
        |> List.foldl
            (\positions genSoFar ->
                genSoFar
                    |> Random.andThen
                        (\gridSoFar ->
                            shuffle (missingDigitsIn positions gridSoFar)
                                |> Random.map
                                    (\shuffled ->
                                        List.map2 Tuple.pair (freeCellsIn positions) shuffled
                                            |> List.foldl (\( pos, v ) g -> Array.set pos v g) gridSoFar
                                    )
                        )
            )
            (Random.constant givenGrid)



-- SEARCH


rowValues : Int -> Grid -> List Int
rowValues r grid =
    List.range 0 8 |> List.map (\c -> cellAt r c grid)


colValues : Int -> Grid -> List Int
colValues c grid =
    List.range 0 8 |> List.map (\r -> cellAt r c grid)


{-| Count of row/column conflicts. A line of 9 cells with `k` distinct
values has `9 - k` duplicate conflicts; summing that over all 9 rows and 9
columns is 0 exactly when the puzzle is solved. Box conflicts never appear
here because the neighbor move can't create any.
-}
energy : Grid -> Float
energy grid =
    let
        lines =
            (List.range 0 8 |> List.map (\r -> rowValues r grid))
                ++ (List.range 0 8 |> List.map (\c -> colValues c grid))
    in
    lines
        |> List.map (\values -> 9 - Set.size (Set.fromList values))
        |> List.sum
        |> toFloat


neighbor : Grid -> Random.Generator Grid
neighbor grid =
    case swappableBoxes of
        [] ->
            Random.constant grid

        first :: rest ->
            Random.uniform first rest
                |> Random.andThen pickTwoDistinct
                |> Random.map (\( posA, posB ) -> swapCells posA posB grid)


pickTwoDistinct : List Int -> Random.Generator ( Int, Int )
pickTwoDistinct cells =
    let
        n =
            List.length cells

        asArray =
            Array.fromList cells

        at i =
            Array.get i asArray |> Maybe.withDefault 0
    in
    Random.int 0 (n - 1)
        |> Random.andThen
            (\i ->
                Random.int 0 (n - 2)
                    |> Random.map
                        (\j0 ->
                            let
                                j =
                                    if j0 >= i then
                                        j0 + 1

                                    else
                                        j0
                            in
                            ( at i, at j )
                        )
            )


swapCells : Int -> Int -> Grid -> Grid
swapCells posA posB grid =
    let
        a =
            Array.get posA grid |> Maybe.withDefault 0

        b =
            Array.get posB grid |> Maybe.withDefault 0
    in
    grid |> Array.set posA b |> Array.set posB a


config : Config Grid
config =
    { energy = energy
    , neighbor = neighbor
    , temperature = Schedule.exponential { initial = 1.0, alpha = 0.9997 }
    , stoppingTemperature = 0.000
    }



-- DETERMINISTIC SOLVER


boxValues : Int -> Int -> Grid -> List Int
boxValues r c grid =
    boxPositions
        |> List.Extra.find (List.member (r * 9 + c))
        |> Maybe.withDefault []
        |> List.map (\p -> Array.get p grid |> Maybe.withDefault 0)


candidatesFor : Int -> Int -> Grid -> List Int
candidatesFor r c grid =
    let
        used =
            rowValues r grid ++ colValues c grid ++ boxValues r c grid
    in
    List.range 1 9 |> List.filter (\v -> not (List.member v used))


firstEmptyCell : Grid -> Maybe Int
firstEmptyCell grid =
    List.range 0 80 |> List.Extra.find (\pos -> Array.get pos grid == Just 0)


{-| Textbook backtracking: find the first empty cell, try every digit that
doesn't immediately conflict, recurse, and undo if that path dead-ends.
`List.Extra.findMap` is exactly "try candidates in order, stop at the first
one that leads somewhere" — the search *is* the fold.
-}
solveByBacktracking : Grid -> Maybe Grid
solveByBacktracking grid =
    case firstEmptyCell grid of
        Nothing ->
            Just grid

        Just pos ->
            candidatesFor (pos // 9) (modBy 9 pos) grid
                |> List.Extra.findMap (\v -> solveByBacktracking (Array.set pos v grid))



-- LEWIS (2007): calibrated temperature, chain-based cooling, reheats


{-| Lewis uses α = 0.99 for order-3 puzzles specifically (Fig. 4 of the
paper); higher orders use the same scheme with different constants.
-}
coolingRate : Float
coolingRate =
    0.99


{-| "In all tests this was set to 20" — the number of consecutive
non-improving Markov chains that triggers a reheat.
-}
reheatPatience : Int
reheatPatience =
    20


{-| The paper doesn't name an exact sample size for calibrating `t0`, only
"a small number of neighbourhood moves" — 50 is our own reasonable reading
of that.
-}
calibrationSampleSize : Int
calibrationSampleSize =
    50


nonFixedCount : Int
nonFixedCount =
    isGiven |> Array.toList |> List.filter not |> List.length


{-| Eq. (4) in the paper: `ml = (sum of non-fixed cells per square)^2`,
which for a single global grid is just the total non-fixed cell count
squared.
-}
markovChainLength : Int
markovChainLength =
    nonFixedCount ^ 2


{-| Population standard deviation — used to turn a sample of observed costs
into the initial temperature `t0`.
-}
stdDev : List Float -> Float
stdDev values =
    let
        n =
            toFloat (List.length values)

        mean =
            List.sum values / n
    in
    sqrt ((values |> List.map (\v -> (v - mean) ^ 2) |> List.sum) / n)


{-| "Before starting the SA part... a small number of neighbourhood moves
are performed." No temperature exists yet at this point, so every move is
taken unconditionally — this is a plain random walk, not hill-climbing.
-}
calibrationWalk : Grid -> Int -> Random.Generator (List Float)
calibrationWalk startGrid steps =
    List.range 1 steps
        |> List.foldl
            (\_ genSoFar ->
                genSoFar
                    |> Random.andThen
                        (\( grid, costs ) ->
                            neighbor grid
                                |> Random.map (\nextGrid -> ( nextGrid, energy nextGrid :: costs ))
                        )
            )
            (Random.constant ( startGrid, [ energy startGrid ] ))
        |> Random.map Tuple.second


{-| A geometric schedule that only changes once per Markov chain of length
`markovChainLength`, rather than every iteration — "homogeneous SA" in the
paper's terms. Still just an `Int -> Float`, so it fits `Config` without any
change to the library.
-}
lewisTemperature : Float -> Int -> Float
lewisTemperature t0 iteration =
    t0 * coolingRate ^ toFloat (iteration // markovChainLength)


lewisConfigFor : Float -> Config Grid
lewisConfigFor t0 =
    { energy = energy
    , neighbor = neighbor
    , temperature = lewisTemperature t0
    , stoppingTemperature = 1.0e-6
    }


{-| Builds the initial grid and calibrates `t0` from a walk starting at that
same grid — one generator, so the one seed drawn on startup covers both.
-}
lewisSetupGenerator : Random.Generator ( Grid, Float )
lewisSetupGenerator =
    initialGridGenerator
        |> Random.andThen
            (\grid ->
                calibrationWalk grid calibrationSampleSize
                    |> Random.map (\costs -> ( grid, stdDev costs ))
            )


{-| Everything this panel needs beyond what `SimulatedAnnealing.State`
already tracks: the `Config` it was calibrated with (so a reheat can reuse
the same `t0` and schedule), and the chain index at which `best` last
improved (so a reheat can tell when to trigger).
-}
type alias LewisRun =
    { config : Config Grid
    , state : State Grid
    , bestChainIndex : Int
    , reheatCount : Int
    }


lewisChainIndex : State Grid -> Int
lewisChainIndex state =
    SimulatedAnnealing.iteration state // markovChainLength



-- MODEL


type alias Model =
    { saState : Maybe (State Grid)
    , running : Bool
    , stepsPerFrame : Int
    , deterministicSolution : Maybe (Maybe Grid)
    , lewis : Maybe LewisRun
    , lewisRunning : Bool
    }


type Msg
    = GotSeed Random.Seed
    | Tick
    | ToggleRunning
    | Restart
    | RunBacktracking
    | GotLewisSeed Random.Seed
    | LewisTick
    | LewisToggleRunning
    | LewisRestart
    | GotReheatSeed Random.Seed


lewisStepsPerFrame : Int
lewisStepsPerFrame =
    600


init : () -> ( Model, Cmd Msg )
init _ =
    ( { saState = Nothing
      , running = False
      , stepsPerFrame = 25
      , deterministicSolution = Nothing
      , lewis = Nothing
      , lewisRunning = False
      }
    , Cmd.batch
        [ Random.generate GotSeed Random.independentSeed
        , Random.generate GotLewisSeed Random.independentSeed
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            let
                ( startingGrid, searchSeed ) =
                    Random.step initialGridGenerator seed
            in
            ( { model | saState = Just (SimulatedAnnealing.init config searchSeed startingGrid), running = True }
            , Cmd.none
            )

        Tick ->
            case model.saState of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    ( { model | saState = Just (SimulatedAnnealing.stepN config model.stepsPerFrame state) }
                    , Cmd.none
                    )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Restart ->
            ( { model | saState = Nothing }, Random.generate GotSeed Random.independentSeed )

        RunBacktracking ->
            ( { model | deterministicSolution = Just (solveByBacktracking givenGrid) }, Cmd.none )

        GotLewisSeed seed ->
            let
                ( ( startingGrid, t0 ), searchSeed ) =
                    Random.step lewisSetupGenerator seed

                lewisConfig =
                    lewisConfigFor t0

                state =
                    SimulatedAnnealing.init lewisConfig searchSeed startingGrid
            in
            ( { model
                | lewis = Just { config = lewisConfig, state = state, bestChainIndex = 0, reheatCount = 0 }
                , lewisRunning = True
              }
            , Cmd.none
            )

        LewisTick ->
            case model.lewis of
                Nothing ->
                    ( model, Cmd.none )

                Just run ->
                    let
                        steppedState =
                            SimulatedAnnealing.stepN run.config lewisStepsPerFrame run.state

                        improved =
                            SimulatedAnnealing.bestEnergy steppedState < SimulatedAnnealing.bestEnergy run.state

                        updatedRun =
                            if improved then
                                { run | state = steppedState, bestChainIndex = lewisChainIndex steppedState }

                            else
                                { run | state = steppedState }

                        stagnantChains =
                            lewisChainIndex steppedState - updatedRun.bestChainIndex

                        alreadySolved =
                            SimulatedAnnealing.bestEnergy steppedState == 0
                    in
                    if not alreadySolved && stagnantChains >= reheatPatience then
                        ( { model | lewis = Just updatedRun }, Random.generate GotReheatSeed Random.independentSeed )

                    else
                        ( { model | lewis = Just updatedRun }, Cmd.none )

        GotReheatSeed seed ->
            case model.lewis of
                Nothing ->
                    ( model, Cmd.none )

                Just run ->
                    let
                        ( startingGrid, searchSeed ) =
                            Random.step initialGridGenerator seed

                        freshState =
                            SimulatedAnnealing.init run.config searchSeed startingGrid
                    in
                    ( { model | lewis = Just { run | state = freshState, bestChainIndex = 0, reheatCount = run.reheatCount + 1 } }
                    , Cmd.none
                    )

        LewisToggleRunning ->
            ( { model | lewisRunning = not model.lewisRunning }, Cmd.none )

        LewisRestart ->
            ( { model | lewis = Nothing }, Random.generate GotLewisSeed Random.independentSeed )



-- SUBSCRIPTIONS


solved : State Grid -> Bool
solved state =
    SimulatedAnnealing.bestEnergy state == 0


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.saState of
            Just state ->
                if model.running && not (solved state) && not (SimulatedAnnealing.isDone config state) then
                    Browser.Events.onAnimationFrame (\_ -> Tick)

                else
                    Sub.none

            Nothing ->
                Sub.none
        , case model.lewis of
            Just run ->
                if model.lewisRunning && SimulatedAnnealing.bestEnergy run.state /= 0 && not (SimulatedAnnealing.isDone run.config run.state) then
                    Browser.Events.onAnimationFrame (\_ -> LewisTick)

                else
                    Sub.none

            Nothing ->
                Sub.none
        ]



-- VIEW


cellSize : Float
cellSize =
    42


boardSize : Float
boardSize =
    cellSize * 9


conflictingPositions : Grid -> Set Int
conflictingPositions grid =
    let
        rows =
            List.range 0 8 |> List.concatMap (\r -> duplicatePositions (List.range 0 8 |> List.map (\c -> ( r * 9 + c, cellAt r c grid ))))

        cols =
            List.range 0 8 |> List.concatMap (\c -> duplicatePositions (List.range 0 8 |> List.map (\r -> ( r * 9 + c, cellAt r c grid ))))
    in
    Set.fromList (rows ++ cols)


duplicatePositions : List ( Int, Int ) -> List Int
duplicatePositions cellsWithValues =
    let
        counts =
            cellsWithValues
                |> List.map Tuple.second
                |> List.foldl (\v d -> Dict.update v (\mc -> Just (Maybe.withDefault 0 mc + 1)) d) Dict.empty
    in
    cellsWithValues
        |> List.filter (\( _, v ) -> (Dict.get v counts |> Maybe.withDefault 0) > 1)
        |> List.map Tuple.first


view : Model -> Html Msg
view model =
    Html.div [ Attr.style "font-family" "sans-serif" ]
        [ Html.div [ Attr.style "display" "flex", Attr.style "gap" "2.5rem", Attr.style "flex-wrap" "wrap" ]
            [ viewAnnealingPanel model
            , viewLewisPanel model
            , viewBacktrackingPanel model
            ]
        ]


viewAnnealingPanel : Model -> Html Msg
viewAnnealingPanel model =
    Html.div [ Attr.style "max-width" (String.fromFloat boardSize ++ "px") ]
        (Html.h3 [] [ Html.text "Simulated annealing (live)" ]
            :: (case model.saState of
                    Nothing ->
                        [ Html.text "Filling in a starting grid..." ]

                    Just state ->
                        let
                            grid =
                                SimulatedAnnealing.current state

                            conflicts =
                                conflictingPositions grid

                            isSolved =
                                solved state

                            done =
                                isSolved || SimulatedAnnealing.isDone config state
                        in
                        [ Svg.svg
                            [ SvgAttr.width (String.fromFloat boardSize)
                            , SvgAttr.height (String.fromFloat boardSize)
                            ]
                            (viewCells grid conflicts ++ viewGridLines)
                        , if isSolved then
                            Html.p [ Attr.style "color" "#1a7f37", Attr.style "font-weight" "bold" ] [ Html.text "Solved!" ]

                          else
                            Html.text ""
                        , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "1.5rem", Attr.style "flex-wrap" "wrap" ]
                            [ readout "Iteration" (String.fromInt (SimulatedAnnealing.iteration state))
                            , readout "Temperature" (String.fromFloat (roundTo 3 (SimulatedAnnealing.temperature config state)))
                            , readout "Current conflicts" (String.fromInt (round (SimulatedAnnealing.currentEnergy state)))
                            , readout "Best conflicts" (String.fromInt (round (SimulatedAnnealing.bestEnergy state)))
                            ]
                        , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "0.5rem" ]
                            [ Html.button [ Events.onClick ToggleRunning, Attr.disabled done ]
                                [ Html.text
                                    (if done then
                                        "Done"

                                     else if model.running then
                                        "Pause"

                                     else
                                        "Resume"
                                    )
                                ]
                            , Html.button [ Events.onClick Restart ] [ Html.text "Restart (new fill)" ]
                            ]
                        , Html.p [ Attr.style "color" "#666", Attr.style "font-size" "0.85rem" ]
                            [ Html.text "Black digits are givens. Red cells are part of a row or column conflict — boxes are never wrong, by construction." ]
                        ]
               )
        )


viewBacktrackingPanel : Model -> Html Msg
viewBacktrackingPanel model =
    Html.div [ Attr.style "max-width" (String.fromFloat boardSize ++ "px") ]
        (Html.h3 [] [ Html.text "Backtracking (deterministic)" ]
            :: (case model.deterministicSolution of
                    Nothing ->
                        [ Svg.svg
                            [ SvgAttr.width (String.fromFloat boardSize)
                            , SvgAttr.height (String.fromFloat boardSize)
                            ]
                            (viewCells givenGrid Set.empty ++ viewGridLines)
                        , Html.div [ Attr.style "margin-top" "0.5rem" ]
                            [ Html.button [ Events.onClick RunBacktracking ] [ Html.text "Solve instantly" ] ]
                        ]

                    Just Nothing ->
                        [ Svg.svg
                            [ SvgAttr.width (String.fromFloat boardSize)
                            , SvgAttr.height (String.fromFloat boardSize)
                            ]
                            (viewCells givenGrid Set.empty ++ viewGridLines)
                        , Html.p [ Attr.style "color" "#c62828" ] [ Html.text "No solution found — the givens are inconsistent." ]
                        ]

                    Just (Just grid) ->
                        [ Svg.svg
                            [ SvgAttr.width (String.fromFloat boardSize)
                            , SvgAttr.height (String.fromFloat boardSize)
                            ]
                            (viewCells grid Set.empty ++ viewGridLines)
                        , Html.p [ Attr.style "color" "#1a7f37", Attr.style "font-weight" "bold" ] [ Html.text "Solved!" ]
                        , Html.p [ Attr.style "color" "#666", Attr.style "font-size" "0.85rem" ]
                            [ Html.text "Complete on the first call — no temperature, no wandering, no chance of getting stuck. This is the right tool for Sudoku in practice; annealing is here to show the library, not because it's the best fit for this problem." ]
                        ]
               )
        )


viewLewisPanel : Model -> Html Msg
viewLewisPanel model =
    Html.div [ Attr.style "max-width" (String.fromFloat boardSize ++ "px") ]
        (Html.h3 [] [ Html.text "Simulated annealing (Lewis 2007)" ]
            :: (case model.lewis of
                    Nothing ->
                        [ Html.text "Calibrating starting temperature..." ]

                    Just run ->
                        let
                            grid =
                                SimulatedAnnealing.current run.state

                            conflicts =
                                conflictingPositions grid

                            isSolved =
                                SimulatedAnnealing.bestEnergy run.state == 0

                            done =
                                isSolved || SimulatedAnnealing.isDone run.config run.state

                            chainIndex =
                                lewisChainIndex run.state

                            stagnantChains =
                                chainIndex - run.bestChainIndex

                            t0 =
                                run.config.temperature 0
                        in
                        [ Svg.svg
                            [ SvgAttr.width (String.fromFloat boardSize)
                            , SvgAttr.height (String.fromFloat boardSize)
                            ]
                            (viewCells grid conflicts ++ viewGridLines)
                        , if isSolved then
                            Html.p [ Attr.style "color" "#1a7f37", Attr.style "font-weight" "bold" ] [ Html.text "Solved!" ]

                          else
                            Html.text ""
                        , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "1.1rem", Attr.style "flex-wrap" "wrap" ]
                            [ readout "Chain" (String.fromInt chainIndex)
                            , readout "Stagnant chains" (String.fromInt stagnantChains ++ " / " ++ String.fromInt reheatPatience)
                            , readout "t0 (calibrated)" (String.fromFloat (roundTo 3 t0))
                            , readout "Temperature" (String.fromFloat (roundTo 3 (SimulatedAnnealing.temperature run.config run.state)))
                            , readout "Current conflicts" (String.fromInt (round (SimulatedAnnealing.currentEnergy run.state)))
                            , readout "Best conflicts" (String.fromInt (round (SimulatedAnnealing.bestEnergy run.state)))
                            , readout "Reheats" (String.fromInt run.reheatCount)
                            ]
                        , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "0.5rem" ]
                            [ Html.button [ Events.onClick LewisToggleRunning, Attr.disabled done ]
                                [ Html.text
                                    (if done then
                                        "Done"

                                     else if model.lewisRunning then
                                        "Pause"

                                     else
                                        "Resume"
                                    )
                                ]
                            , Html.button [ Events.onClick LewisRestart ] [ Html.text "Restart (recalibrate)" ]
                            ]
                        , Html.p [ Attr.style "color" "#666", Attr.style "font-size" "0.85rem" ]
                            [ Html.text ("Chain length ml = " ++ String.fromInt markovChainLength ++ " moves. Temperature only drops between chains, not every move; a reheat resets to the original t0 with a fresh random fill, but keeps that same t0 — only \"Restart\" below recalibrates it.")
                            ]
                        ]
               )
        )


roundTo : Int -> Float -> Float
roundTo decimals value =
    let
        factor =
            toFloat (10 ^ decimals)
    in
    toFloat (round (value * factor)) / factor


readout : String -> String -> Html msg
readout label value =
    Html.div []
        [ Html.div [ Attr.style "font-size" "0.75rem", Attr.style "color" "#666" ] [ Html.text label ]
        , Html.div [ Attr.style "font-variant-numeric" "tabular-nums" ] [ Html.text value ]
        ]


viewCells : Grid -> Set Int -> List (Svg msg)
viewCells grid conflicts =
    List.range 0 8
        |> List.concatMap
            (\r ->
                List.range 0 8
                    |> List.concatMap (\c -> viewCell r c grid conflicts)
            )


viewCell : Int -> Int -> Grid -> Set Int -> List (Svg msg)
viewCell r c grid conflicts =
    let
        pos =
            r * 9 + c

        value =
            cellAt r c grid

        given =
            Array.get pos isGiven |> Maybe.withDefault False

        inConflict =
            Set.member pos conflicts

        x =
            toFloat c * cellSize

        y =
            toFloat r * cellSize
    in
    [ Svg.rect
        [ SvgAttr.x (String.fromFloat x)
        , SvgAttr.y (String.fromFloat y)
        , SvgAttr.width (String.fromFloat cellSize)
        , SvgAttr.height (String.fromFloat cellSize)
        , SvgAttr.fill
            (if inConflict then
                "#fbdcdc"

             else if given then
                "#f0f0f0"

             else
                "#fff"
            )
        , SvgAttr.stroke "#ddd"
        ]
        []
    , Svg.text_
        [ SvgAttr.x (String.fromFloat (x + cellSize / 2))
        , SvgAttr.y (String.fromFloat (y + cellSize / 2 + 6))
        , SvgAttr.textAnchor "middle"
        , SvgAttr.fontSize "22"
        , SvgAttr.fontWeight
            (if given then
                "bold"

             else
                "normal"
            )
        , SvgAttr.fill
            (if inConflict then
                "#c62828"

             else if given then
                "#111"

             else
                "#1f6feb"
            )
        ]
        [ Svg.text (String.fromInt value) ]
    ]


viewGridLines : List (Svg msg)
viewGridLines =
    List.range 0 9
        |> List.concatMap
            (\i ->
                let
                    thick =
                        modBy 3 i == 0

                    width =
                        if thick then
                            "3"

                        else
                            "1"

                    pos =
                        toFloat i * cellSize
                in
                [ Svg.line
                    [ SvgAttr.x1 (String.fromFloat pos)
                    , SvgAttr.y1 "0"
                    , SvgAttr.x2 (String.fromFloat pos)
                    , SvgAttr.y2 (String.fromFloat boardSize)
                    , SvgAttr.stroke "#333"
                    , SvgAttr.strokeWidth width
                    ]
                    []
                , Svg.line
                    [ SvgAttr.x1 "0"
                    , SvgAttr.y1 (String.fromFloat pos)
                    , SvgAttr.x2 (String.fromFloat boardSize)
                    , SvgAttr.y2 (String.fromFloat pos)
                    , SvgAttr.stroke "#333"
                    , SvgAttr.strokeWidth width
                    ]
                    []
                ]
            )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
