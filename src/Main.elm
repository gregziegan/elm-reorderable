module Main exposing (..)

import Html
import Example exposing (init, update, view, subscriptions)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
