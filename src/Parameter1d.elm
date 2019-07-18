--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This Source Code Form is subject to the terms of the Mozilla Public        --
-- License, v. 2.0. If a copy of the MPL was not distributed with this file,  --
-- you can obtain one at http://mozilla.org/MPL/2.0/.                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


module Parameter1d exposing (steps, leading, trailing, inBetween, midpoints)

{-| All of the functions in this module take as their first argument the number
of steps to take from 0 to 1, and a function to evaluate, but evaluate that
function at a different number of parameter values to obtain different numbers
of results:

    Function call | Results | Parameter values passed
    --------------|---------|-------------------------------
    steps 4       | 5       | [ 0, 0.25, 0.5, 0.75, 1 ]
    leading 4     | 4       | [ 0, 0.25, 0.5, 0.75 ]
    trailing 4    | 4       | [ 0.25, 0.5, 0.75, 1 ]
    inBetween 4   | 3       | [ 0.25, 0.5, 0.75 ]
    midpoints 4   | 4       | [ 0.125, 0.375, 0.625, 0.875 ]

See the documentation of each function for details.

@docs steps, leading, trailing, inBetween, midpoints

-}


{-| For a given `n` (which must be greater than zero), call the given function
`n + 1` times with values evenly spaced between 0 and 1 inclusive, and return
the results as a list:

    import Float.Extra as Float

    Parameter1d.steps 4 (Float.interpolateFrom 10 20)
    --> [ 10, 12.5, 15, 17.5, 20 ]

You can read this as "take 4 steps to interpolate from 10 to 20". More generally,

    Parameter1d.steps 4 someFunction

will return

    [ someFunction 0.0
    , someFunction 0.25
    , someFunction 0.5
    , someFunction 0.75
    , someFunction 1.0
    ]

Note that the results do not themselves have to be `Float`s:

    import Point2d

    p1 =
        Point2d.fromCoordinates ( 0, 0 )

    p2 =
        Point2d.fromCoordinates ( 100, 200 )

    Parameter1d.steps 4 (Point2d.interpolateFrom p1 p2)
    --> [ Point2d.fromCoordinates ( 0, 0 )
    --> , Point2d.fromCoordinates ( 25, 50 )
    --> , Point2d.fromCoordinates ( 50, 100 )
    --> , Point2d.fromCoordinates ( 75, 150 )
    --> , Point2d.fromCoordinates ( 100, 200 )
    --> ]

Passing a negative or zero value as the first argument will result in an empty
list being returned.

-}
steps : Int -> (Float -> a) -> List a
steps n function =
    if n < 1 then
        []

    else
        range 0 n (toFloat n) function []


{-| Like `steps`, but with the last value omitted:

    import Float.Extra as Float

    Parameter1d.leading 4 (Float.interpolateFrom 10 20)
    --> [ 10, 12.5, 15, 17.5 ]

-}
leading : Int -> (Float -> a) -> List a
leading n function =
    if n < 1 then
        []

    else
        range 0 (n - 1) (toFloat n) function []


{-| Also like `steps`, but with the first value omitted:

    import Float.Extra as Float

    Parameter1d.trailing 4 (Float.interpolateFrom 10 20)
    --> [ 12.5, 15, 17.5, 20 ]

-}
trailing : Int -> (Float -> a) -> List a
trailing n function =
    if n < 1 then
        []

    else
        range 1 n (toFloat n) function []


{-| Like `steps` but with both the first _and_ last values omitted (the
"in-between" values only). Note that the given `n` still refers to the number of
steps to take; the number of returned values will be one less than the number of
steps.

    import Float.Extra as Float

    Parameter1d.inBetween 4 (Float.interpolateFrom 10 20)
    --> [ 12.5, 15, 17.5 ]

    Parameter1d.inBetween 3 (Float.interpolateFrom 10 20)
    --> [ 13.3333, 16.6667 ]

    Parameter1d.inBetween 2 (Float.interpolateFrom 10 20)
    --> [ 15 ]

Passing a value less than 2 will result in an empty list being returned.

-}
inBetween : Int -> (Float -> a) -> List a
inBetween n function =
    if n < 2 then
        []

    else
        range 1 (n - 1) (toFloat n) function []


range : Int -> Int -> Float -> (Float -> a) -> List a -> List a
range startIndex index divisor function accumulated =
    let
        newValue =
            function (toFloat index / divisor)

        newAccumulated =
            newValue :: accumulated
    in
    if index == startIndex then
        newAccumulated

    else
        range startIndex (index - 1) divisor function newAccumulated


{-| For a given `n` > 0, construct `n` equal-width intervals between 0 and 1 and
call the given function at the midpoint of each interval. This is useful for
some mathematical operations like [numerical
integration](https://en.wikipedia.org/wiki/Riemann_sum#Midpoint_rule).

    import Float.Extra as Float

    Parameter1d.midpoints 4 (Float.interpolateFrom 10 20)
    --> [ 11.25, 13.75, 16.25, 18.75 ]

-}
midpoints : Int -> (Float -> a) -> List a
midpoints n function =
    if n < 1 then
        []

    else
        midpointsHelp (2 * n - 1) (2 * toFloat n) function []


midpointsHelp : Int -> Float -> (Float -> a) -> List a -> List a
midpointsHelp index divisor function accumulated =
    let
        newValue =
            function (toFloat index / divisor)

        newAccumulated =
            newValue :: accumulated
    in
    if index == 1 then
        newAccumulated

    else
        midpointsHelp (index - 2) divisor function newAccumulated
