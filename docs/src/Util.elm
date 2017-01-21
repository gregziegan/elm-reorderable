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


{-| Rocket syntax for pretty tuples; lets you do

    "display" => "none"

instead of

    ("display", "none")

Also works great for update functions, especially those that return
`( model, List (Cmd msg) )` like so:

    { model | foo = bar } => []


    { model |
        foo = bar
    }
        => []


    model
        |> doSomething
        |> doSomethingElse
        => []

To get an update function that returns `( model, List (Cmd msg) )`, see
`Util.batchUpdate`
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)



-- infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
-- meaning you can use it at the end of a pipeline and have it work out.


infixl 0 =>


{-| use this with `program` to make its `init` return `( model, List (Cmd msg) )`
like so:

    main : Program Value Model Msg
    main =
        Html.programWithFlags
            { init = init >> Util.batchInit
            , update = Update.update >> Util.batchUpdate
            , view = View.view
            , subscriptions = subscriptions
            }
-}
batchInit : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
batchInit =
    batchCommands


{-| use this with `program` to make its `update` return `( model, List (Cmd msg) )`
like so:

    main : Program Value Model Msg
    main =
        Html.programWithFlags
            { init = init >> Util.batchInit
            , update = Update.update >> Util.batchUpdate
            , view = View.view
            , subscriptions = subscriptions
            }
-}
batchUpdate : (model -> ( model, List (Cmd msg) )) -> model -> ( model, Cmd msg )
batchUpdate fn =
    fn >> batchCommands


batchCommands : ( model, List (Cmd msg) ) -> ( model, Cmd msg )
batchCommands ( model, commands ) =
    model => Cmd.batch commands
