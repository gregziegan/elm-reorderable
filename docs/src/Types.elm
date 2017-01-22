module Types exposing (..)

import DOM
import Mouse
import Reorderable.State exposing (Point)


type alias SlidingPlaceholder =
    { start : Point
    , end : Point
    , sourceTabIndex : Int
    , destTabIndex : Int
    , tab : Tab
    }


type alias PinningPlaceholder =
    { start : Point
    , end : Point
    , startWidth : Float
    , oldTabIndex : Int
    , newTabIndex : Int
    , tab : Tab
    }


type alias UnPinningPlaceholder =
    { start : Point
    , end : Point
    , endWidth : Float
    , oldTabIndex : Int
    , newTabIndex : Int
    , tab : Tab
    }


type PinPlaceholder
    = Pinning PinningPlaceholder
    | UnPinning UnPinningPlaceholder


type ReorderItem
    = ReorderableTab
    | DropPreview
    | PinSourceBackdrop
    | UnPinSourceBackdrop
    | PinDestBackdrop


type alias TabMenu =
    { tabIndex : Int
    , position : Mouse.Position
    , tabRect : DOM.Rectangle
    , tab : Tab
    }


type TabMenuItem
    = PinTab Int
    | UnpinTab Int
    | CloseTab Int
    | CloseOtherTabs Int
    | CloseTabsToTheRight Int


type alias Tab =
    { id : Int
    , title : String
    , icon : String
    , isPinned : Bool
    }


type Logo
    = Elm
    | Elixir
    | Haskell


type alias TabClickInfo =
    { position : Mouse.Position
    , rect : DOM.Rectangle
    }


toLogo : String -> Logo
toLogo str =
    case str of
        "Elm" ->
            Elm

        "Elixir" ->
            Elixir

        "Haskell" ->
            Haskell

        _ ->
            Elm


tabMenuItemToString : TabMenuItem -> String
tabMenuItemToString menuItem =
    case menuItem of
        PinTab _ ->
            "Pin Tab"

        UnpinTab _ ->
            "Unpin Tab"

        CloseTab _ ->
            "Close Tab"

        CloseOtherTabs _ ->
            "Close Other Tabs"

        CloseTabsToTheRight _ ->
            "Close Tabs to the Right"


tabMenuItems : Bool -> Int -> List TabMenuItem
tabMenuItems isPinned tabIndex =
    [ if isPinned then
        UnpinTab tabIndex
      else
        PinTab tabIndex
    , CloseTab tabIndex
    , CloseOtherTabs tabIndex
    , CloseTabsToTheRight tabIndex
    ]


dragConfig : Reorderable.State.Config
dragConfig =
    { animate = True }
