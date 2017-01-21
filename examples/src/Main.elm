module Main exposing (..)

import Html
import Messages exposing (Msg)
import Model exposing (Model)
import Subscriptions exposing (subscriptions)
import Update
import Util
import View


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model.initialModel, [] ) |> Util.batchInit
        , update = Update.update >> Util.batchUpdate
        , view = View.view
        , subscriptions = subscriptions
        }
