module Model exposing (..)

import Animation
import Animation.Messenger
import Array.Hamt as Array exposing (Array)
import Keyboard.Extra
import Messages exposing (Msg)
import Reorderable.State exposing (State, ViewableReorderable(..))
import Types exposing (..)


type alias Model =
    { selected : Tab
    , pinPlaceholder : Maybe PinPlaceholder
    , pinPlaceholderStyle : Animation.Messenger.State Msg
    , placeholderAnimationStyle : Animation.Messenger.State Msg
    , tabMenu : Maybe TabMenu
    , keyboardModel : Keyboard.Extra.Model
    , pinDestinationStyle : Animation.Messenger.State Msg
    , pinStartBackdropStyle : Animation.Messenger.State Msg
    , dragState : State Tab
    , flexTabWidth : Float
    , pinnedTabWidth : Float
    , showingAnyMenu : Bool
    , overflowArea : Maybe OverflowArea
    , overflowMenuStyle : Animation.Messenger.State Msg
    }


initDragState : Array Tab -> State Tab
initDragState tabs =
    { placeholder = Nothing
    , items = tabs
    , reorderedItems = tabs
    , animating = False
    }


someTabs : Array Tab
someTabs =
    Array.fromList
        [ { id = 1, title = "First Tab", icon = "ElmLogo", isPinned = False }
        , { id = 2, title = "Second Tab", icon = "ElmLogo", isPinned = False }
        ]


initialModel : Model
initialModel =
    let
        ( keyboardModel, _ ) =
            Keyboard.Extra.init
    in
        { selected = { id = 1, title = "First Tab", icon = "ElmLogo", isPinned = False }
        , pinPlaceholder = Nothing
        , pinPlaceholderStyle = Animation.style []
        , placeholderAnimationStyle = Animation.style []
        , tabMenu = Nothing
        , keyboardModel = keyboardModel
        , pinDestinationStyle = Animation.style []
        , pinStartBackdropStyle = Animation.style []
        , dragState = initDragState someTabs
        , flexTabWidth = 0
        , pinnedTabWidth = 60
        , showingAnyMenu = False
        , overflowArea = Nothing
        , overflowMenuStyle = Animation.style []
        }
