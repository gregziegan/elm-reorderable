module Messages exposing (..)

import Animation
import DOM
import Keyboard.Extra
import Reorderable.Update
import Types exposing (..)
import Window


type Msg
    = AnimateMessenger Animation.Msg
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | DragMsg Reorderable.Update.DragMsg
    | FinishSlidingTab SlidingPlaceholder
    | SetActive Tab
    | ToggleTabMenu Int TabClickInfo
    | PinTabAtIndex Int Tab DOM.Rectangle
    | FinishPinningTab PinningPlaceholder
    | UnpinTabAtIndex Int Tab DOM.Rectangle
    | FinishUnpinningTab UnPinningPlaceholder
    | CloseTabAtIndex Int
    | CloseTabsOtherThanIndex Int
    | CloseTabsToTheRightOfIndex Int
    | CloseAllMenus
    | NewTabWidth Float
    | WindowResize Window.Size
