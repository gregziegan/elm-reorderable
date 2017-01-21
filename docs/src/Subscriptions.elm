port module Subscriptions exposing (..)

import Animation
import Keyboard.Extra
import Messages exposing (..)
import Model exposing (Model)
import Types exposing (dragConfig)
import Reorderable.Update
import Window


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
    , Sub.map DragMsg (Reorderable.Update.subscriptions dragConfig model.dragState)
    , Animation.subscription AnimateMessenger [ model.pinPlaceholderStyle, model.placeholderAnimationStyle, model.pinDestinationStyle, model.pinStartBackdropStyle ]
    , newTabWidth NewTabWidth
    , Window.resizes WindowResize
    ]
        |> Sub.batch


port newTabWidth : (Float -> msg) -> Sub msg
