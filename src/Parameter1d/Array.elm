module Parameter1d.Array exposing (steps, leading, trailing, inBetween, midpoints)

{-| These functions behave exactly like the corresponding ones in the base
`Parameter1d` module, but return `Array`s instead of `List`s. For example,

    Parameter1d.Array.steps 4 (Float.interplateFrom 10 20)

is equivalent to

    Parameter1d.steps 4 (Float.interpolateFrom 10 20)
        |> Array.fromList

but is more efficient (internally, `Parameter1d.Array.steps` calls
`Array.initialize` directly instead of constructing a temporary list).

@docs steps, leading, trailing, inBetween, midpoints

-}

import Array exposing (Array)


{-| -}
steps : Int -> (Float -> a) -> Array a
steps n function =
    if n > 0 then
        Array.initialize (n + 1)
            (\i ->
                let
                    parameterValue =
                        toFloat i / toFloat n
                in
                function parameterValue
            )

    else
        Array.empty


{-| -}
leading : Int -> (Float -> a) -> Array a
leading n function =
    if n > 0 then
        Array.initialize n
            (\i ->
                let
                    parameterValue =
                        toFloat i / toFloat n
                in
                function parameterValue
            )

    else
        Array.empty


{-| -}
trailing : Int -> (Float -> a) -> Array a
trailing n function =
    if n > 0 then
        Array.initialize n
            (\i ->
                let
                    parameterValue =
                        toFloat (i + 1) / toFloat n
                in
                function parameterValue
            )

    else
        Array.empty


{-| -}
inBetween : Int -> (Float -> a) -> Array a
inBetween n function =
    if n > 1 then
        Array.initialize (n - 1)
            (\i ->
                let
                    parameterValue =
                        toFloat (i + 1) / toFloat n
                in
                function parameterValue
            )

    else
        Array.empty


{-| -}
midpoints : Int -> (Float -> a) -> Array a
midpoints n function =
    if n > 0 then
        Array.initialize n
            (\i ->
                let
                    parameterValue =
                        toFloat (2 * i + 1) / (2 * toFloat n)
                in
                function parameterValue
            )

    else
        Array.empty
