module TimeWindows exposing (main)

{-| A second live example, deliberately unlike the TSP one:

  - The problem has no coordinates, only a static travel-time matrix, so the
    view is a schedule timeline instead of a map.
  - The energy function isn't a simple sum of edge weights — it has to walk
    the tour in order, carrying an accumulated clock forward, because
    whether you wait at a city depends on when you arrive. `energy` in this
    library's `Config` is just `solution -> Float`, so nothing about the
    core had to change to support that; only this example's own `simulate`
    function got more interesting.
  - Randomness is used once, up front (via a `Cmd`, see `GotSeed`), to pick
    the search's starting seed; everything after that is pure stepping. The
    TSP example instead used a `Cmd` to randomize the *problem* (city
    positions) and a fixed seed for the search — this one flips that.

Ten cities, one of them ("Depot") is the 6:00 AM start/end point of a round
trip. Five cities are open 24 hours; the other five are only open 8:00-17:00
daily, and arriving outside that window means waiting for the next opening
even if that's most of a day away.
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
    { name : String
    , windowed : Bool
    }


{-| Index 0 is the depot. Five cities (B, D, F, H, I) are open 8:00-17:00
only; the rest, including the depot, are open all day.
-}
cities : Array City
cities =
    Array.fromList
        [ City "Depot" False
        , City "A" False
        , City "B" True
        , City "C" False
        , City "D" True
        , City "E" False
        , City "F" True
        , City "G" False
        , City "H" True
        , City "I" True
        ]


{-| Symmetric travel times in minutes between every pair of the 10 cities.
Hand-generated, not derived from coordinates — the median of the 45 unique
city-pair distances is exactly 45 minutes.
-}
costMatrix : Array (Array Int)
costMatrix =
    Array.fromList
        [ Array.fromList [ 0, 30, 35, 70, 30, 25, 30, 45, 65, 45 ]
        , Array.fromList [ 30, 0, 25, 50, 45, 80, 85, 60, 40, 45 ]
        , Array.fromList [ 35, 25, 0, 40, 55, 35, 55, 70, 60, 40 ]
        , Array.fromList [ 70, 50, 40, 0, 45, 25, 55, 25, 35, 40 ]
        , Array.fromList [ 30, 45, 55, 45, 0, 50, 30, 70, 35, 50 ]
        , Array.fromList [ 25, 80, 35, 25, 50, 0, 55, 30, 25, 40 ]
        , Array.fromList [ 30, 85, 55, 55, 30, 55, 0, 50, 45, 55 ]
        , Array.fromList [ 45, 60, 70, 25, 70, 30, 50, 0, 55, 45 ]
        , Array.fromList [ 65, 40, 60, 35, 35, 25, 45, 55, 0, 60 ]
        , Array.fromList [ 45, 45, 40, 40, 50, 40, 55, 45, 60, 0 ]
        ]


travelTime : Int -> Int -> Int
travelTime from to =
    costMatrix
        |> Array.get from
        |> Maybe.andThen (Array.get to)
        |> Maybe.withDefault 0


depotIndex : Int
depotIndex =
    0


startTime : Int
startTime =
    6 * 60


windowOpen : Int
windowOpen =
    8 * 60


windowClose : Int
windowClose =
    17 * 60


dayLength : Int
dayLength =
    24 * 60


{-| Minutes to wait if you arrive (in absolute minutes since the start of
day 0) at a windowed city at this clock time. Arriving exactly at closing
time counts as closed — that's the case that produces the worst wait,
exactly 15 hours until the next opening.
-}
waitUntilOpen : Int -> Int
waitUntilOpen arrival =
    let
        clock =
            modBy dayLength arrival
    in
    if clock < windowOpen then
        windowOpen - clock

    else if clock < windowClose then
        0

    else
        (dayLength - clock) + windowOpen



-- SOLUTION: a tour is a permutation of the 9 non-depot city indices,
-- visited in order, starting and ending at the depot.


type alias Tour =
    List Int


type alias Visit =
    { cityIndex : Int
    , arrival : Int
    , wait : Int
    , departure : Int
    }


{-| Walk the tour in order starting from the depot at `startTime`, carrying
the clock forward leg by leg. This is why the energy function can't just sum
edge weights: whether (and how long) you wait at a city depends on the
accumulated time when you get there, not on the edge alone.
-}
simulate : Tour -> List Visit
simulate tour =
    let
        visitCity ( prevCity, clock, visits ) cityIndex =
            let
                arrival =
                    clock + travelTime prevCity cityIndex

                wait =
                    if cityIsWindowed cityIndex then
                        waitUntilOpen arrival

                    else
                        0

                departure =
                    arrival + wait
            in
            ( cityIndex, departure, { cityIndex = cityIndex, arrival = arrival, wait = wait, departure = departure } :: visits )

        depotStart =
            { cityIndex = depotIndex, arrival = startTime, wait = 0, departure = startTime }

        ( lastCity, lastDeparture, reversedVisits ) =
            List.foldl (\c acc -> visitCity acc c) ( depotIndex, startTime, [] ) tour

        returnArrival =
            lastDeparture + travelTime lastCity depotIndex

        depotReturn =
            { cityIndex = depotIndex, arrival = returnArrival, wait = 0, departure = returnArrival }
    in
    depotStart :: List.reverse reversedVisits ++ [ depotReturn ]


cityIsWindowed : Int -> Bool
cityIsWindowed cityIndex =
    Array.get cityIndex cities |> Maybe.map .windowed |> Maybe.withDefault False


totalMinutes : Tour -> Int
totalMinutes tour =
    simulate tour
        |> List.Extra.last
        |> Maybe.map (\v -> v.departure - startTime)
        |> Maybe.withDefault 0


{-| Reverse a random contiguous segment of the visiting order — the same
2-opt move as the TSP example, just applied to a list of 9 stops instead of
40.
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
                    let
                        ( before, rest ) =
                            List.Extra.splitAt lo tour

                        ( middle, after ) =
                            List.Extra.splitAt (hi - lo + 1) rest
                    in
                    before ++ List.reverse middle ++ after
            )


config : Config Tour
config =
    { energy = toFloat << totalMinutes
    , neighbor = twoOptNeighbor
    , temperature = Schedule.exponential { initial = 400, alpha = 0.999 }
    , stoppingTemperature = 0.05
    }


initialTour : Tour
initialTour =
    List.range 1 9



-- MODEL


type alias Model =
    { saState : Maybe (State Tour)
    , running : Bool
    , started : Bool
    , stepsPerFrame : Int
    }


type Msg
    = GotSeed Random.Seed
    | Tick
    | Start
    | ToggleRunning
    | Restart


init : () -> ( Model, Cmd Msg )
init _ =
    ( { saState = Nothing, running = False, started = False, stepsPerFrame = 30 }
    , Random.generate GotSeed Random.independentSeed
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSeed seed ->
            ( { model | saState = Just (SimulatedAnnealing.init config seed initialTour), running = False }
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

        Start ->
            ( { model | running = True, started = True }, Cmd.none )

        ToggleRunning ->
            ( { model | running = not model.running }, Cmd.none )

        Restart ->
            ( { model | saState = Nothing, running = False, started = False }, Random.generate GotSeed Random.independentSeed )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.saState of
        Just state ->
            if model.running && not (SimulatedAnnealing.isDone config state) then
                Browser.Events.onAnimationFrame (\_ -> Tick)

            else
                Sub.none

        Nothing ->
            Sub.none



-- VIEW


timelineWidth : Float
timelineWidth =
    760


rowHeight : Float
rowHeight =
    34


view : Model -> Html Msg
view model =
    case model.saState of
        Nothing ->
            Html.text "Choosing a starting seed..."

        Just state ->
            let
                tour =
                    SimulatedAnnealing.current state

                visits =
                    simulate tour

                done =
                    SimulatedAnnealing.isDone config state

                viewMax =
                    max 600 (((totalMinutes tour // 60) + 1) * 60)
            in
            Html.div [ Attr.style "font-family" "sans-serif", Attr.style "max-width" "800px" ]
                [ Svg.svg
                    [ SvgAttr.width (String.fromFloat timelineWidth)
                    , SvgAttr.height (String.fromFloat (rowHeight * toFloat (List.length visits) + 10))
                    ]
                    (List.indexedMap (viewRow viewMax) visits |> List.concat)
                , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "1.5rem" ]
                    [ readout "Iteration" (String.fromInt (SimulatedAnnealing.iteration state))
                    , readout "Temperature" (String.fromInt (round (SimulatedAnnealing.temperature config state)))
                    , readout "Current route time" (formatDuration (round (SimulatedAnnealing.currentEnergy state)))
                    , readout "Best route time" (formatDuration (round (SimulatedAnnealing.bestEnergy state)))
                    ]
                , Html.div [ Attr.style "margin-top" "0.5rem", Attr.style "display" "flex", Attr.style "gap" "0.5rem" ]
                    [ if model.started then
                        Html.button [ Events.onClick ToggleRunning, Attr.disabled done ]
                            [ Html.text
                                (if done then
                                    "Done"

                                 else if model.running then
                                    "Pause"

                                 else
                                    "Resume"
                                )
                            ]

                      else
                        Html.button [ Events.onClick Start ] [ Html.text "Start" ]
                    , Html.button [ Events.onClick Restart ] [ Html.text "Restart (new seed)" ]
                    ]
                , Html.p [ Attr.style "color" "#666", Attr.style "font-size" "0.85rem" ]
                    [ Html.text "Pink bands are hours a city is closed. Orange bars are time spent waiting for a city to open." ]
                , Html.details [ Attr.style "margin-top" "0.5rem" ]
                    [ Html.summary [ Attr.style "cursor" "pointer", Attr.style "color" "#666" ]
                        [ Html.text "Travel time matrix (minutes)" ]
                    , viewCostMatrix
                    ]
                ]


readout : String -> String -> Html msg
readout label value =
    Html.div []
        [ Html.div [ Attr.style "font-size" "0.75rem", Attr.style "color" "#666" ] [ Html.text label ]
        , Html.div [ Attr.style "font-variant-numeric" "tabular-nums" ] [ Html.text value ]
        ]


xScale : Int -> Float -> Float
xScale viewMax elapsedMinutes =
    elapsedMinutes * (timelineWidth / toFloat viewMax)


viewRow : Int -> Int -> Visit -> List (Svg msg)
viewRow viewMax rowIndex visit =
    let
        y =
            toFloat rowIndex * rowHeight

        city =
            Array.get visit.cityIndex cities |> Maybe.withDefault (City "?" False)

        arrivalElapsed =
            toFloat (visit.arrival - startTime)

        departureElapsed =
            toFloat (visit.departure - startTime)

        label =
            city.name
                ++ " — arr "
                ++ formatClock visit.arrival
                ++ (if visit.wait > 0 then
                        " (waited " ++ formatDuration visit.wait ++ ")"

                    else
                        ""
                   )
    in
    [ Svg.rect
        [ SvgAttr.x "0"
        , SvgAttr.y (String.fromFloat y)
        , SvgAttr.width (String.fromFloat timelineWidth)
        , SvgAttr.height (String.fromFloat (rowHeight - 4))
        , SvgAttr.fill "#fff"
        , SvgAttr.stroke "#eee"
        ]
        []
    ]
        ++ (if city.windowed then
                closedBands viewMax
                    |> List.map
                        (\( s, e ) ->
                            Svg.rect
                                [ SvgAttr.x (String.fromFloat (xScale viewMax (toFloat s)))
                                , SvgAttr.y (String.fromFloat y)
                                , SvgAttr.width (String.fromFloat (xScale viewMax (toFloat (e - s))))
                                , SvgAttr.height (String.fromFloat (rowHeight - 4))
                                , SvgAttr.fill "#fbdcdc"
                                ]
                                []
                        )

            else
                []
           )
        ++ [ Svg.rect
                [ SvgAttr.x (String.fromFloat (xScale viewMax arrivalElapsed))
                , SvgAttr.y (String.fromFloat (y + 6))
                , SvgAttr.width (String.fromFloat (max 2 (xScale viewMax (departureElapsed - arrivalElapsed))))
                , SvgAttr.height (String.fromFloat (rowHeight - 16))
                , SvgAttr.fill
                    (if visit.wait > 0 then
                        "#e8871e"

                     else
                        "#1f6feb"
                    )
                ]
                []
           , Svg.text_
                [ SvgAttr.x "4"
                , SvgAttr.y (String.fromFloat (y + rowHeight - 8))
                , SvgAttr.fontSize "11"
                , SvgAttr.fill "#333"
                ]
                [ Svg.text label ]
           ]


{-| The recurring "closed" intervals (in minutes elapsed since the 6:00 AM
start) for a windowed city, up to `viewMax`. Every window opens for 9 hours
and is then closed for 15, on a 24-hour cycle offset by the 6:00 AM start
time.
-}
closedBands : Int -> List ( Int, Int )
closedBands viewMax =
    let
        firstOpenStart =
            modBy dayLength (windowOpen - startTime)

        closedLength =
            dayLength - (windowClose - windowOpen)

        maxK =
            (viewMax // dayLength) + 2
    in
    List.range 0 maxK
        |> List.map (\k -> ( firstOpenStart + k * dayLength - closedLength, firstOpenStart + k * dayLength ))
        |> List.filterMap
            (\( s, e ) ->
                if e < 0 || s > viewMax then
                    Nothing

                else
                    Just ( max 0 s, min viewMax e )
            )


matrixCell : List (Html.Attribute msg)
matrixCell =
    [ Attr.style "border" "1px solid #ddd"
    , Attr.style "padding" "2px 6px"
    , Attr.style "text-align" "center"
    ]


viewCostMatrix : Html msg
viewCostMatrix =
    let
        cityList =
            Array.toList cities

        headerRow =
            Html.tr []
                (Html.th matrixCell [] :: List.map (\c -> Html.th matrixCell [ Html.text c.name ]) cityList)

        bodyRow i city =
            Html.tr []
                (Html.th matrixCell [ Html.text city.name ]
                    :: List.indexedMap
                        (\j _ ->
                            Html.td matrixCell
                                [ Html.text
                                    (if i == j then
                                        "—"

                                     else
                                        String.fromInt (travelTime i j)
                                    )
                                ]
                        )
                        cityList
                )
    in
    Html.table
        [ Attr.style "border-collapse" "collapse"
        , Attr.style "margin-top" "0.5rem"
        , Attr.style "font-size" "0.8rem"
        , Attr.style "font-variant-numeric" "tabular-nums"
        ]
        (headerRow :: List.indexedMap bodyRow cityList)


formatClock : Int -> String
formatClock absoluteMinutes =
    let
        day =
            absoluteMinutes // dayLength

        clock =
            modBy dayLength absoluteMinutes

        hh =
            clock // 60

        mm =
            modBy 60 clock

        pad n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    "day " ++ String.fromInt day ++ " " ++ pad hh ++ ":" ++ pad mm


formatDuration : Int -> String
formatDuration minutes =
    String.fromInt (minutes // 60) ++ "h " ++ String.fromInt (modBy 60 minutes) ++ "m"


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
