module Update exposing (..)

import Animation exposing (px)
import Animation.Messenger
import Array.Hamt as Array exposing (Array)
import DOM
import Keyboard.Extra
import Messages exposing (Msg(..))
import Model exposing (..)
import Types exposing (..)
import Ports
import Reorderable.State exposing (Bounds, Placeholder, Point)
import Reorderable.Update exposing (dropAndShift)
import Util exposing (..)


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg model =
    case msg of
        AnimateMessenger animMsg ->
            let
                ( placeholderAnimationStyle, cmdsPlaceholder ) =
                    Animation.Messenger.update animMsg model.placeholderAnimationStyle

                ( pinPlaceholderStyle, cmdsMoving ) =
                    Animation.Messenger.update animMsg model.pinPlaceholderStyle

                ( pinDestinationStyle, cmdsTargetPreview ) =
                    Animation.Messenger.update animMsg model.pinDestinationStyle

                ( pinStartBackdropStyle, cmdsStartingPreview ) =
                    Animation.Messenger.update animMsg model.pinStartBackdropStyle

                cmds =
                    [ cmdsMoving, cmdsPlaceholder, cmdsTargetPreview, cmdsStartingPreview ]
            in
                { model
                    | pinPlaceholderStyle = pinPlaceholderStyle
                    , placeholderAnimationStyle = placeholderAnimationStyle
                    , pinDestinationStyle = pinDestinationStyle
                    , pinStartBackdropStyle = pinStartBackdropStyle
                }
                    => cmds

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel

                escapeIsPressed =
                    Keyboard.Extra.isPressed Keyboard.Extra.Escape keyboardModel
            in
                { model | keyboardModel = keyboardModel }
                    |> closeAllMenus
                    |> resetDragState
                    => [ Cmd.map KeyboardExtraMsg keyboardCmd ]

        DragMsg dragMsg ->
            updateDrag dragMsg model

        FinishSlidingTab { sourceTabIndex, destTabIndex, tab } ->
            let
                newTabs =
                    dropAndShift sourceTabIndex destTabIndex model.dragState.items
            in
                { model
                    | placeholderAnimationStyle = Animation.style []
                    , dragState = initDragState newTabs
                }
                    => []

        SetActive tab ->
            model
                |> setActiveTab tab
                => []

        ToggleTabMenu tabIndex tabClickInfo ->
            model
                |> toggleTabMenu tabIndex tabClickInfo
                |> setShowingAnyMenuTrue
                => []

        PinTabAtIndex tabIndex tab rect ->
            model
                |> startPinTabAnimation (initPinningInfo tabIndex tab rect model)
                |> closeAllMenus
                => []

        FinishPinningTab { oldTabIndex } ->
            model
                |> pinTab oldTabIndex
                => [ Ports.getFlexTabWidth <| clamp oldTabIndex (Array.length model.dragState.items - 1) (oldTabIndex + 1) ]

        UnpinTabAtIndex tabIndex tab rect ->
            model
                |> startUnPinTabAnimation (initUnPinningInfo tabIndex tab rect model)
                |> closeAllMenus
                => []

        FinishUnpinningTab { oldTabIndex, newTabIndex } ->
            model
                |> unpinTab oldTabIndex newTabIndex
                => [ Ports.getFlexTabWidth <| clamp oldTabIndex (Array.length model.dragState.items - 1) (oldTabIndex + 1) ]

        CloseTabAtIndex tabIndex ->
            model
                |> closeTab tabIndex
                |> closeAllMenus
                => if Array.length model.dragState.items > 1 then
                    [ Ports.getFlexTabWidth <| clamp 0 (Array.length model.dragState.items - 2) tabIndex ]
                   else
                    []

        CloseTabsOtherThanIndex tabIndex ->
            model
                |> closeTabsOtherThanIndex tabIndex
                |> closeAllMenus
                => [ Ports.getFlexTabWidth 0 ]

        CloseTabsToTheRightOfIndex tabIndex ->
            model
                |> closeTabsToTheRightOfIndex tabIndex
                |> closeAllMenus
                => [ Ports.getFlexTabWidth tabIndex ]

        CloseAllMenus ->
            model
                |> closeAllMenus
                |> setShowingAnyMenuFalse
                => []

        AddTab ->
            model
                |> addTab (tabFromId model.nextTabId)
                => [ Ports.getFlexTabWidth 0 ]

        NewTabWidth newTabWidth ->
            { model | flexTabWidth = newTabWidth }
                => []

        WindowResize size ->
            model
                => [ Ports.getFlexTabWidth (Array.length model.dragState.items - 1) ]


updateDrag : Reorderable.Update.DragMsg -> Model -> ( Model, List (Cmd Msg) )
updateDrag dragMsg model =
    case Reorderable.Update.update dragMsg model.dragState of
        Ok ( newDragState, cmd ) ->
            case dragMsg of
                Reorderable.Update.DragHold { placeholder, reorderableBounds } ->
                    { model | dragState = newDragState }
                        |> slideTabIntoPreview placeholder.bounds reorderableBounds
                        => []

                _ ->
                    { model | dragState = newDragState }
                        => [ Cmd.map DragMsg cmd ]

        Err _ ->
            model => []


{-|
Calculates the drop destination for the currently held placeholder.
If the current placeholder is a pinned tab, it will slide the placeholder to the nearest pinned tab position.
Pinned tabs are always first in tab order.
Vice-versa applies to unpinned tabs.
-}
slideTabIntoPreview : Bounds -> List Bounds -> Model -> Model
slideTabIntoPreview placeholderBounds reorderableBounds model =
    model.dragState.placeholder
        |> Maybe.map (slideTabIntoPreviewHelp model placeholderBounds reorderableBounds)
        |> Maybe.withDefault model


slideTabIntoPreviewHelp : Model -> Bounds -> List Bounds -> Placeholder Tab -> Model
slideTabIntoPreviewHelp model placeholderBounds reorderableBounds { draggable, sourceIndex } =
    let
        maybeDestIndex =
            Reorderable.Update.getMostOverlappingBounds placeholderBounds reorderableBounds

        destTabIndex =
            getDestForPinnableTabs draggable model.dragState.items reorderableBounds (Maybe.map Tuple.first maybeDestIndex)

        destPreviewBounds =
            reorderableBounds
                |> List.drop destTabIndex
                |> List.head
                |> Maybe.withDefault (Bounds 0 0 0 0)

        start =
            Point placeholderBounds.left placeholderBounds.top

        end =
            Point destPreviewBounds.left destPreviewBounds.top
    in
        startSlidingTabAnimation
            { start = start
            , end = end
            , sourceTabIndex = sourceIndex
            , destTabIndex = destTabIndex
            , tab = draggable
            }
            model


{-| Begins animating the dropped placeholder towards its destination.
When finished, a `FinishSlidingTab` msg is produced so we can change the tabs array.
-}
startSlidingTabAnimation : SlidingPlaceholder -> Model -> Model
startSlidingTabAnimation ({ start, end, destTabIndex } as slidingTab) model =
    let
        placeholderAnimationStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px start.x) ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px end.x) ]
                , Animation.Messenger.send (FinishSlidingTab slidingTab)
                ]
                model.placeholderAnimationStyle
    in
        { model
            | placeholderAnimationStyle = placeholderAnimationStyle
        }


setActiveTab : Tab -> Model -> Model
setActiveTab tab model =
    { model | selected = tab }


{-|
Toggles the tab context menu, used for pinning, closing, etc.
-}
toggleTabMenu : Int -> TabClickInfo -> Model -> Model
toggleTabMenu tabIndex tabClickInfo model =
    Array.get tabIndex model.dragState.items
        |> Maybe.map (toggleTabMenuHelp model tabIndex tabClickInfo)
        |> Maybe.withDefault model


toggleTabMenuHelp : Model -> Int -> TabClickInfo -> Tab -> Model
toggleTabMenuHelp model tabIndex { position, rect } tab =
    { model
        | tabMenu =
            if model.tabMenu == Nothing then
                Just <|
                    { tabIndex = tabIndex
                    , position = position
                    , tabRect = rect
                    , tab = tab
                    }
            else
                Nothing
    }


setShowingAnyMenuTrue : Model -> Model
setShowingAnyMenuTrue model =
    { model | showingAnyMenu = True }


setShowingAnyMenuFalse : Model -> Model
setShowingAnyMenuFalse model =
    { model | showingAnyMenu = False }


initPinningInfo : Int -> Tab -> DOM.Rectangle -> Model -> PinningPlaceholder
initPinningInfo tabIndex tab { top, left, width } model =
    let
        tabPosition =
            Point left top

        newTabIndex =
            getNumPinned model.dragState.items

        numUnPinned =
            tabIndex - newTabIndex

        pinDisplacement =
            toFloat numUnPinned * width

        newTabPosition =
            Point (left - pinDisplacement) top
    in
        { start = tabPosition
        , end = newTabPosition
        , startWidth = width
        , oldTabIndex = tabIndex
        , newTabIndex = newTabIndex
        , tab = tab
        }


initUnPinningInfo : Int -> Tab -> DOM.Rectangle -> Model -> UnPinningPlaceholder
initUnPinningInfo tabIndex tab { top, left, width } model =
    let
        tabPosition =
            Point left top

        numPinned =
            getNumPinned model.dragState.items

        numPinnedTabsToTravel =
            numPinned - tabIndex - 1

        newTabPosition =
            Point (left + (toFloat numPinnedTabsToTravel * model.pinnedTabWidth)) top
    in
        { start = tabPosition
        , end = newTabPosition
        , endWidth = model.flexTabWidth
        , oldTabIndex = tabIndex
        , newTabIndex = numPinned
        , tab = tab
        }


closeAllMenus : Model -> Model
closeAllMenus model =
    { model | tabMenu = Nothing, showingAnyMenu = False }


resetDragState : Model -> Model
resetDragState model =
    { model | dragState = initDragState model.dragState.items }


getDestForPinnableTabs : Tab -> Array Tab -> List Bounds -> Maybe Int -> Int
getDestForPinnableTabs tab tabs reorderableBounds maybeDestIndex =
    let
        numPinned =
            getNumPinned tabs

        determinePlaceholderDestination =
            determineDestIndex tabs numPinned reorderableBounds tab.isPinned
    in
        maybeDestIndex
            |> Maybe.andThen determinePlaceholderDestination
            |> Maybe.withDefault 0


determineDestHelp : Int -> List Bounds -> Bool -> Int -> Tab -> Int
determineDestHelp numPinned reorderableBounds placeholderIsPinned index tab =
    if placeholderIsPinned && not tab.isPinned then
        numPinned - 1
    else if not placeholderIsPinned && tab.isPinned then
        numPinned
    else
        index


determineDestIndex : Array Tab -> Int -> List Bounds -> Bool -> Int -> Maybe Int
determineDestIndex tabs numPinned reorderableBounds placeholderIsPinned index =
    let
        helpDetermineIndex =
            determineDestHelp numPinned reorderableBounds placeholderIsPinned index
    in
        Array.get index tabs
            |> Maybe.map helpDetermineIndex


pinTab : Int -> Model -> Model
pinTab oldTabIndex model =
    model.dragState.items
        |> Array.get oldTabIndex
        |> Maybe.map (pinTabHelp oldTabIndex model.dragState.items model)
        |> Maybe.withDefault model


pinTabHelp : Int -> Array Tab -> Model -> Tab -> Model
pinTabHelp oldTabIndex tabs model tabToPin =
    let
        pinnedTab =
            { tabToPin | isPinned = True }

        unpinnedIndex =
            getNumPinned tabs

        newTabs =
            tabs
                |> Array.slice 0 unpinnedIndex
                |> Array.push pinnedTab
                |> flip Array.append (Array.slice unpinnedIndex oldTabIndex tabs)
                |> flip Array.append (Array.slice (oldTabIndex + 1) (Array.length tabs) tabs)

        newDragState =
            initDragState newTabs
    in
        { model
            | dragState = newDragState
            , pinPlaceholder = Nothing
            , pinPlaceholderStyle = Animation.style []
            , pinStartBackdropStyle = Animation.style []
            , pinDestinationStyle = Animation.style []
        }


startPinTabAnimation : PinningPlaceholder -> Model -> Model
startPinTabAnimation ({ start, end, startWidth, oldTabIndex, newTabIndex } as pinningTab) model =
    let
        newPinPlaceholderStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px start.x)
                    , Animation.width (px startWidth)
                    ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px end.x)
                    , Animation.width (px model.pinnedTabWidth)
                    ]
                , Animation.Messenger.send (FinishPinningTab pinningTab)
                ]
                model.pinPlaceholderStyle

        newTargetPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px startWidth ]
                , Animation.toWith
                    fastDrift
                    [ Animation.width <| px 0 ]
                ]
                model.pinDestinationStyle

        newStartingPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px 0 ]
                , Animation.toWith
                    fastDrift
                    [ Animation.width <| px <| model.pinnedTabWidth ]
                ]
                model.pinStartBackdropStyle
    in
        { model
            | pinPlaceholderStyle = newPinPlaceholderStyle
            , flexTabWidth = startWidth
            , pinPlaceholder = Just <| Pinning pinningTab
            , pinDestinationStyle = newStartingPlaceholderStyle
            , pinStartBackdropStyle = newTargetPlaceholderStyle
        }


unpinTab : Int -> Int -> Model -> Model
unpinTab oldTabIndex newTabIndex model =
    model.dragState.items
        |> Array.get oldTabIndex
        |> Maybe.map (unPinTabHelp oldTabIndex newTabIndex model.dragState.items model)
        |> Maybe.withDefault model


unPinTabHelp : Int -> Int -> Array Tab -> Model -> Tab -> Model
unPinTabHelp oldTabIndex newTabIndex tabs model tabToUnPin =
    let
        unpinnedTab =
            { tabToUnPin | isPinned = False }

        unpinnedIndex =
            getNumPinned model.dragState.items

        newTabs =
            tabs
                |> Array.slice 0 oldTabIndex
                |> flip Array.append (Array.slice (oldTabIndex + 1) newTabIndex tabs)
                |> Array.push unpinnedTab
                |> flip Array.append (Array.slice newTabIndex (Array.length tabs) tabs)

        newDragState =
            initDragState newTabs
    in
        { model
            | dragState = newDragState
            , pinPlaceholder = Nothing
            , pinPlaceholderStyle = Animation.style []
            , pinStartBackdropStyle = Animation.style []
            , pinDestinationStyle = Animation.style []
        }


startUnPinTabAnimation : UnPinningPlaceholder -> Model -> Model
startUnPinTabAnimation ({ start, end, endWidth, oldTabIndex, newTabIndex } as pinningTab) model =
    let
        numPinned =
            getNumPinned model.dragState.items + 1

        numTabs =
            Array.length model.dragState.items

        newPinPlaceholderStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px start.x)
                    , Animation.width (px model.pinnedTabWidth)
                    ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px end.x)
                    , Animation.width (px endWidth)
                    ]
                , Animation.Messenger.send (FinishUnpinningTab pinningTab)
                ]
                model.pinPlaceholderStyle

        newTargetPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px model.pinnedTabWidth ]
                , Animation.toWith
                    fastDrift
                    [ Animation.width <| px 0 ]
                ]
                model.pinDestinationStyle

        newStartingPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px 0 ]
                , Animation.toWith
                    fastDrift
                    [ Animation.width <| px <| model.flexTabWidth ]
                ]
                model.pinStartBackdropStyle
    in
        { model
            | pinPlaceholderStyle = newPinPlaceholderStyle
            , pinPlaceholder = Just <| UnPinning pinningTab
            , pinDestinationStyle = newStartingPlaceholderStyle
            , pinStartBackdropStyle = newTargetPlaceholderStyle
        }


closeTab : Int -> Model -> Model
closeTab indexToClose model =
    { model
        | dragState =
            model.dragState.items
                |> removeAtIndex indexToClose
                |> initDragState
    }


closeTabsOtherThanIndex : Int -> Model -> Model
closeTabsOtherThanIndex tabIndex model =
    model.dragState.items
        |> Array.get tabIndex
        |> Maybe.map (closeOtherTabsThanIndexHelp model)
        |> Maybe.withDefault model


closeOtherTabsThanIndexHelp : Model -> Tab -> Model
closeOtherTabsThanIndexHelp model selectedTab =
    { model | dragState = initDragState <| Array.fromList [ selectedTab ] }


closeTabsToTheRightOfIndex : Int -> Model -> Model
closeTabsToTheRightOfIndex tabIndex model =
    { model
        | dragState =
            model.dragState.items
                |> Array.slice 0 (tabIndex + 1)
                |> initDragState
    }


addTab : Tab -> Model -> Model
addTab tab model =
    { model
        | dragState =
            model.dragState.items
                |> Array.push tab
                |> initDragState
        , nextTabId = model.nextTabId + 1
    }
