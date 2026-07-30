module Tsp exposing (main)

{-| Live viewer for the traveling salesman problem, annealed with
`SimulatedAnnealing`. Cities are placed randomly, then a tour is annealed
in front of you: each animation frame runs a batch of `step`s and repaints,
so you watch the tour untangle as the temperature drops.
-}

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import List.Extra
import Random
import SimulatedAnnealing exposing (Config, State)
import SimulatedAnnealing.Schedule as Schedule
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr



-- PROBLEM DEFINITION


type alias City =
    { x : Float, y : Float }


{-| A tour is a permutation of city indices, visited in order and closing
back to the start.
-}
type alias Tour =
    List Int


numCities : Int
numCities =
    40


canvasWidth : Float
canvasWidth =
    600


canvasHeight : Float
canvasHeight =
    400


cityGenerator : Random.Generator City
cityGenerator =
    Random.map2 City
        (Random.float 20 (canvasWidth - 20))
        (Random.float 20 (canvasHeight - 20))


distance : City -> City -> Float
distance a b =
    sqrt ((a.x - b.x) ^ 2 + (a.y - b.y) ^ 2)


tourLength : Array City -> Tour -> Float
tourLength cities tour =
    case tour of
        [] ->
            0

        first :: _ ->
            let
                coords =
                    List.filterMap (\i -> Array.get i cities) (tour ++ [ first ])
            in
            coords
                |> List.map2 Tuple.pair (List.drop 1 coords)
                |> List.map (\( a, b ) -> distance a b)
                |> List.sum


{-| A 2-opt move: reverse a random contiguous segment of the tour. This is
the standard TSP neighbor move — far better behaved than swapping two random
cities, because it never crosses the new segment over itself.
-}
twoOptNeighbor : Tour -> Random.Generator Tour
twoOptNeighbor tour =
    let
        n =
            List.length tour
    in
    Random.pair (Random.int 0 (n - 1)) (Random.int 0 (n - 1))
        |> Random.map
            (\( i, j ) ->
                let
                    lo =
                        min i j

                    hi =
                        max i j
                in
                if lo == hi then
                    tour

                else
                    reverseSegment lo hi tour
            )


reverseSegment : Int -> Int -> List a -> List a
reverseSegment lo hi list =
    let
        ( before, rest ) =
            List.Extra.splitAt lo list

        ( middle, after ) =
            List.Extra.splitAt (hi - lo + 1) rest
    in
    before ++ List.reverse middle ++ after


tourConfig : Array City -> Config Tour
tourConfig cities =
    { energy = tourLength cities
    , neighbor = twoOptNeighbor
    , temperature = Schedule.exponential { initial = 120, alpha = 0.999 }
    , stoppingTemperature = 0.05
    }



-- MODEL


type alias Model =
    { cities : Array City
    , saState : Maybe (State Tour)
    , running : Bool
    , stepsPerFrame : Int
    }


type Msg
    = GotCities (List City)
    | Tick
    | ToggleRunning
    | NewCities
    | Restart


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cities = Array.empty
      , saState = Nothing
      , running = False
      , stepsPerFrame = 20
      }
    , Random.generate GotCities (Random.list numCities cityGenerator)
    )


startAnnealing : Array City -> State Tour
startAnnealing cities =
    let
        initialTour =
            List.range 0 (Array.length cities - 1)
    in
    SimulatedAnnealing.init (tourConfig cities) (Random.initialSeed 12345) initialTour



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCities cities ->
            let
                citiesArray =
                    Array.fromList cities
            in
            ( { model
                | cities = citiesArray
                , saState = Just (startAnnealing citiesArray)
                , running = True
              }
            , Cmd.none
            )

        Tick ->
            case model.saState of
                Nothing ->
                    ( model, Cmd.none )

                Just state ->
                    ( { model | saState = Just (SimulatedAnnealing.stepN (tourConfig model.cities) model.stepsPerFrame state) }
                    , Cmd.none
                    )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Restart ->
            ( { model | saState = Just (startAnnealing model.cities), running = True }, Cmd.none )

        NewCities ->
            ( model, Random.generate GotCities (Random.list numCities cityGenerator) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.saState of
        Just state ->
            if model.running && not (SimulatedAnnealing.isDone (tourConfig model.cities) state) then
                Browser.Events.onAnimationFrame (\_ -> Tick)

            else
                Sub.none

        Nothing ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model.saState of
        Nothing ->
            Html.text "Placing cities..."

        Just state ->
            let
                config =
                    tourConfig model.cities

                done =
                    SimulatedAnnealing.isDone config state
            in
            Html.div [ Attr.style "font-family" "sans-serif", Attr.style "max-width" "640px" ]
                [ Svg.svg
                    [ SvgAttr.width (String.fromFloat canvasWidth)
                    , SvgAttr.height (String.fromFloat canvasHeight)
                    , SvgAttr.style "border: 1px solid #ccc; background: #fafafa"
                    ]
                    (viewTour model.cities (SimulatedAnnealing.current state)
                        ++ viewCities model.cities
                    )
                , Html.div [ Attr.style "margin-top" "0.75rem", Attr.style "display" "flex", Attr.style "gap" "1.5rem" ]
                    [ readout "Iteration" (String.fromInt (SimulatedAnnealing.iteration state))
                    , readout "Temperature" (formatFloat (SimulatedAnnealing.temperature config state))
                    , readout "Current length" (formatFloat (SimulatedAnnealing.currentEnergy state))
                    , readout "Best length" (formatFloat (SimulatedAnnealing.bestEnergy state))
                    ]
                , Html.div [ Attr.style "margin-top" "0.75rem", Attr.style "display" "flex", Attr.style "gap" "0.5rem" ]
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
                    , Html.button [ Events.onClick Restart ] [ Html.text "Restart (same cities)" ]
                    , Html.button [ Events.onClick NewCities ] [ Html.text "New cities" ]
                    ]
                ]


readout : String -> String -> Html msg
readout label value =
    Html.div []
        [ Html.div [ Attr.style "font-size" "0.75rem", Attr.style "color" "#666" ] [ Html.text label ]
        , Html.div [ Attr.style "font-variant-numeric" "tabular-nums" ] [ Html.text value ]
        ]


formatFloat : Float -> String
formatFloat value =
    String.fromInt (round value)


viewCities : Array City -> List (Svg msg)
viewCities cities =
    cities
        |> Array.toList
        |> List.map
            (\city ->
                Svg.circle
                    [ SvgAttr.cx (String.fromFloat city.x)
                    , SvgAttr.cy (String.fromFloat city.y)
                    , SvgAttr.r "4"
                    , SvgAttr.fill "#1f6feb"
                    ]
                    []
            )


viewTour : Array City -> Tour -> List (Svg msg)
viewTour cities tour =
    let
        points =
            (tour ++ List.take 1 tour)
                |> List.filterMap (\i -> Array.get i cities)
                |> List.map (\city -> String.fromFloat city.x ++ "," ++ String.fromFloat city.y)
                |> String.join " "
    in
    [ Svg.polyline
        [ SvgAttr.points points
        , SvgAttr.fill "none"
        , SvgAttr.stroke "#e85d2b"
        , SvgAttr.strokeWidth "1.5"
        ]
        []
    ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
