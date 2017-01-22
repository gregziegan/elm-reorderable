module Main exposing (..)

import Html
import Messages exposing (Msg)
import Model exposing (Model)
import Ports
import Rocket
import Subscriptions exposing (subscriptions)
import Update
import View


main : Program Never Model Msg
main =
    Html.program
        { init = ( Model.initialModel, [ Ports.getFlexTabWidth 0 ] ) |> Rocket.batchInit
        , update = Update.update >> Rocket.batchUpdate
        , view = View.view
        , subscriptions = subscriptions
        }
