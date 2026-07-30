This library is entirely written by Claude Sonnet 5. I'm happy with it and find it useful, but I'm leaning against publishing it to Elm packages.

To use in your project, clone it to a `vendor/` directory and add `vendor/elm-simulated-annealing/src` to `source-directories` in your elm.json file.


# elm-simulated-annealing

A pure, steppable [simulated annealing](https://en.wikipedia.org/wiki/Simulated_annealing) core for The Elm Architecture.

This library never runs a loop itself. It exposes one pure function, `step`,
that advances a search by exactly one iteration. Your application decides how
to drive it: once per animation frame for a live view, in a batch for a
headless run, or one click at a time for a debugger-style walkthrough.
Because every intermediate `State` is an ordinary immutable value, it's
always safe to render, store, or compare.

## Quick start

You provide four things in a `Config`: an energy function to minimize, a way
to generate a random neighboring solution, a cooling schedule, and a
stopping temperature.

```elm
import Random
import SimulatedAnnealing exposing (Config, State)
import SimulatedAnnealing.Schedule as Schedule


type alias Solution =
    List Int


config : Config Solution
config =
    { energy = List.sum -- lower is better; this is a toy example
    , neighbor = shuffleOneElement
    , temperature = Schedule.exponential { initial = 100, alpha = 0.995 }
    , stoppingTemperature = 0.01
    }


shuffleOneElement : Solution -> Random.Generator Solution
shuffleOneElement solution =
    Debug.todo "produce a random nearby solution"


initialState : State Solution
initialState =
    SimulatedAnnealing.init config (Random.initialSeed 0) [ 5, 3, 8, 1 ]
```

From there, drive the search however fits your app:

```elm
-- One step at a time, e.g. on every animation frame:
SimulatedAnnealing.step config state

-- A batch of steps in one pure pass, e.g. before the next repaint:
SimulatedAnnealing.stepN config 20 state

-- Check whether the cooling schedule has bottomed out:
SimulatedAnnealing.isDone config state
```

Read the solution back out with the accessors:

```elm
SimulatedAnnealing.current state       -- solution the search is sitting at
SimulatedAnnealing.currentEnergy state
SimulatedAnnealing.best state          -- best solution seen across the whole run
SimulatedAnnealing.bestEnergy state
SimulatedAnnealing.temperature config state
SimulatedAnnealing.iteration state
```

## Cooling schedules

`SimulatedAnnealing.Schedule` has three ready-made schedules — `exponential`,
`linear`, and `logarithmic`. Each is a function of type `Int -> Float`, so you
can write your own by matching that signature if none of these fit. See the
module docs in
[`src/SimulatedAnnealing/Schedule.elm`](src/SimulatedAnnealing/Schedule.elm)
for the tradeoffs between them.

## Examples

The [`examples/`](examples) directory has three live, running demos, each
chosen to exercise a different part of the API:

- [`Tsp.elm`](examples/src/Tsp.elm) — traveling salesman with a 2-opt
  neighbor move. Watch a tangled tour untangle as the temperature drops.
- [`TimeWindows.elm`](examples/src/TimeWindows.elm) — a routing problem
  where the energy function has to walk the tour in order, carrying an
  accumulated clock, because arrival time affects cost.
- [`Sudoku.elm`](examples/src/Sudoku.elm) — pure constraint satisfaction,
  with no notion of distance or time; energy counts the number of
  conflicting cells.

To run one, build it to the corresponding page in `examples/`, e.g.:

```sh
cd examples
elm make src/Tsp.elm --output tsp.js
open tsp.html
```

## Tests

```sh
elm-test
```

or:

```sh
npx elm-test
```

See [`tests/SimulatedAnnealingTest.elm`](tests/SimulatedAnnealingTest.elm).
