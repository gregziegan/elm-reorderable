module Util exposing (..)

import Animation
import Html.Events exposing (Options)
import Array.Hamt as Array exposing (Array)
import Time exposing (second)
import Types exposing (Tab)


toPx : number -> String
toPx num =
    (toString num) ++ "px"


removeAtIndex : Int -> Array a -> Array a
removeAtIndex index array =
    Array.append
        (Array.slice 0 index array)
        (Array.slice (index + 1) (Array.length array + 1) array)


getNumPinned : Array Tab -> Int
getNumPinned tabs =
    tabs
        |> Array.filter .isPinned
        |> Array.length


defaultPrevented : Options
defaultPrevented =
    Html.Events.Options False True



-- CONSTANTS


fastDrift : Animation.Interpolation
fastDrift =
    Animation.easing
        { duration = 0.1 * second
        , ease = (\x -> x ^ 2)
        }
