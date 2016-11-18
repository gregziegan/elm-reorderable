module Main exposing (..)

import Html
import DraggableTabs exposing (init, update, view, subscriptions)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
