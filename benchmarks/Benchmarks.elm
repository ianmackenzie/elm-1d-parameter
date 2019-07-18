module Benchmarks exposing (main)

import Array
import Benchmark
import Benchmark.Runner
import Float.Extra as Float
import List.Extra
import Parameter1d
import Parameter1d.Array


main : Benchmark.Runner.BenchmarkProgram
main =
    Benchmark.Runner.program <|
        let
            interpolationFunction =
                Float.interpolateFrom 100 200

            numSteps =
                1000
        in
        Benchmark.describe "steps"
            [ Benchmark.describe "Parameter1d.steps"
                [ Benchmark.benchmark "naive"
                    (\() ->
                        List.range 0 numSteps
                            |> List.map
                                (\i ->
                                    let
                                        parameterValue =
                                            toFloat i / toFloat numSteps
                                    in
                                    interpolationFunction parameterValue
                                )
                    )
                , Benchmark.benchmark "List.Extra"
                    (\() ->
                        List.Extra.initialize (numSteps + 1)
                            (\i ->
                                let
                                    parameterValue =
                                        toFloat i / toFloat numSteps
                                in
                                interpolationFunction parameterValue
                            )
                    )
                , Benchmark.benchmark "Parameter1d"
                    (\() -> Parameter1d.steps numSteps interpolationFunction)
                ]
            , Benchmark.describe "Parameter1d.Array.steps"
                [ Benchmark.benchmark "direct"
                    (\() ->
                        Array.initialize (numSteps + 1)
                            (\i ->
                                let
                                    parameterValue =
                                        toFloat i / toFloat numSteps
                                in
                                interpolationFunction parameterValue
                            )
                    )
                , Benchmark.benchmark "Parameter1d"
                    (\() ->
                        Parameter1d.Array.steps numSteps interpolationFunction
                    )
                ]
            ]
