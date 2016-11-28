module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, draggable, style, contextmenu)
import Html.Events exposing (Options, onClick, onMouseDown, onMouseUp, onMouseEnter, on, onWithOptions)
import Json.Decode as Json
import Mouse
import Animation exposing (px)
import Animation.Messenger
import Time exposing (second)
import Keyboard.Extra


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        maybeMovingTabSubscriptions =
            model.movingTab
                |> Maybe.map movingTabSubscriptions
                |> Maybe.withDefault []

        movingTabSubscriptions movingTab =
            case movingTab of
                Sliding _ ->
                    [ Animation.subscription AnimateMessenger [ model.movingTabStyle, model.targetPlaceholderStyle ] ]

                Dragging draggingTab ->
                    [ Mouse.moves (DraggingTabContinues draggingTab)
                    , Mouse.ups (DraggingTabEnding draggingTab)
                    ]

                Pinning pinningTab ->
                    [ Animation.subscription AnimateMessenger [ model.movingTabStyle, model.targetPlaceholderStyle, model.startingPlaceholderStyle ] ]
    in
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions ]
            |> List.append maybeMovingTabSubscriptions
            |> Sub.batch



-- MODEL


type alias SlidingTab =
    { start : Mouse.Position
    , tabIndex : Int
    , isPinned : Bool
    }


type alias PinningTab =
    { start : Mouse.Position
    , tabIndex : Int
    , startedPinned : Bool
    }


type alias DraggingTab =
    { start : Mouse.Position
    , current : Mouse.Position
    , tabIndex : Int
    , isPinned : Bool
    }


type MovingTab
    = Sliding SlidingTab
    | Pinning PinningTab
    | Dragging DraggingTab


type alias TabMenu =
    { tabIndex : Int
    , position : Mouse.Position
    , isPinned : Bool
    }


type alias Model =
    { tabs : List String
    , pinnedTabs : List String
    , selected : String
    , movingTab : Maybe MovingTab
    , movingTabStyle : Animation.Messenger.State Msg
    , tabMenu : Maybe TabMenu
    , keyboardModel : Keyboard.Extra.Model
    , targetPlaceholderStyle : Animation.Messenger.State Msg
    , startingPlaceholderStyle : Animation.Messenger.State Msg
    }


type TabMenuItem
    = Reload Int
    | PinTab Int
    | UnpinTab Int
    | CloseTab Int


tabMenuItemToString : TabMenuItem -> String
tabMenuItemToString menuItem =
    case menuItem of
        Reload _ ->
            "Reload"

        PinTab _ ->
            "Pin Tab"

        UnpinTab _ ->
            "Unpin Tab"

        CloseTab _ ->
            "Close Tab"


tabMenuItems : Bool -> Int -> List TabMenuItem
tabMenuItems isPinned tabIndex =
    [ Reload tabIndex
    , if isPinned then
        UnpinTab tabIndex
      else
        PinTab tabIndex
    , CloseTab tabIndex
    ]


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { tabs = [ "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6" ]
          , pinnedTabs = []
          , selected = "Tab 1"
          , movingTab = Nothing
          , movingTabStyle = Animation.style []
          , tabMenu = Nothing
          , keyboardModel = keyboardModel
          , targetPlaceholderStyle = Animation.style []
          , startingPlaceholderStyle = Animation.style []
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


initTabMenu : Int -> Mouse.Position -> Bool -> TabMenu
initTabMenu tabIndex xy isPinned =
    { tabIndex = tabIndex
    , position = xy
    , isPinned = isPinned
    }


initDraggingTab : Mouse.Position -> Int -> Bool -> DraggingTab
initDraggingTab start tabIndex isPinned =
    { start = start
    , current = start
    , tabIndex = tabIndex
    , isPinned = isPinned
    }


initSlidingTab : Mouse.Position -> Int -> Bool -> SlidingTab
initSlidingTab start tabIndex isPinned =
    { start = start
    , tabIndex = tabIndex
    , isPinned = isPinned
    }


initPinningTab : Mouse.Position -> Int -> Bool -> PinningTab
initPinningTab start tabIndex startedPinned =
    { start = start
    , tabIndex = tabIndex
    , startedPinned = startedPinned
    }



-- UPDATE


type Msg
    = SetActive String
    | DraggingTabStart Int Bool Mouse.Position
    | DraggingTabContinues DraggingTab Mouse.Position
    | DraggingTabEnding DraggingTab Mouse.Position
    | DraggingTabEnd SlidingTab Mouse.Position
    | AnimateMessenger Animation.Msg
    | ToggleTabMenu Int Bool Mouse.Position
    | CloseTabMenu
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | PinTabAtIndex Int Mouse.Position
    | UnpinTabAtIndex Int Mouse.Position
    | FinishPinningTab PinningTab
    | FinishUnpinningTab PinningTab
    | CloseTabAtIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive tabId ->
            ( { model | selected = tabId }, Cmd.none )

        DraggingTabStart tabIndex isPinned xy ->
            ( model
                |> startDraggingTab tabIndex isPinned xy
            , Cmd.none
            )

        DraggingTabContinues draggingTab xy ->
            let
                newDraggingTab =
                    draggingTab
                        |> setCurrent xy
                        |> boundDraggingTabMouse model xy
                        |> Dragging
            in
                ( { model | movingTab = Just newDraggingTab }
                , Cmd.none
                )

        DraggingTabEnding { start, current, tabIndex, isPinned } xy ->
            let
                slidingTab =
                    initSlidingTab current tabIndex isPinned
            in
                if start.x == current.x then
                    ( { model | movingTab = Nothing }, Cmd.none )
                else
                    ( { model | movingTab = Just <| Sliding slidingTab }
                        |> slidingTabAnimation (allTabs model) slidingTab
                    , Cmd.none
                    )

        DraggingTabEnd slidingTab xy ->
            ( model
                |> dropTab slidingTab xy
                |> resetDraggingTabAnimation
            , Cmd.none
            )

        AnimateMessenger animMsg ->
            let
                ( movingTabStyle, cmdsMoving ) =
                    Animation.Messenger.update animMsg model.movingTabStyle

                ( targetPlaceholderStyle, cmdsTargetPlaceholder ) =
                    Animation.Messenger.update animMsg model.targetPlaceholderStyle

                ( startingPlaceholderStyle, cmdsStartingPlaceholder ) =
                    Animation.Messenger.update animMsg model.startingPlaceholderStyle
            in
                ( { model
                    | movingTabStyle = movingTabStyle
                    , targetPlaceholderStyle = targetPlaceholderStyle
                    , startingPlaceholderStyle = startingPlaceholderStyle
                  }
                , Cmd.batch [ cmdsMoving, cmdsTargetPlaceholder, cmdsStartingPlaceholder ]
                )

        ToggleTabMenu tabIndex isPinned xy ->
            ( { model
                | tabMenu =
                    if model.tabMenu == Nothing then
                        Just <| initTabMenu tabIndex xy isPinned
                    else
                        Nothing
              }
            , Cmd.none
            )

        CloseTabMenu ->
            ( { model | tabMenu = Nothing }, Cmd.none )

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel

                escapeIsPressed =
                    Keyboard.Extra.isPressed Keyboard.Extra.Escape keyboardModel
            in
                ( { model
                    | keyboardModel = keyboardModel
                    , tabMenu =
                        if model.tabMenu /= Nothing && escapeIsPressed then
                            Nothing
                        else
                            model.tabMenu
                    , movingTab =
                        if model.movingTab /= Nothing && escapeIsPressed then
                            Nothing
                        else
                            model.movingTab
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

        PinTabAtIndex tabIndex xy ->
            let
                pinningTab =
                    initPinningTab xy tabIndex False

                tabs =
                    allTabs model
            in
                ( { model | movingTab = Just <| Pinning pinningTab }
                    |> resetDraggingTabAnimation
                    |> pinTabAnimation tabs pinningTab
                    |> pinPlaceholderAnimation False
                , Cmd.none
                )

        UnpinTabAtIndex tabIndex xy ->
            let
                pinningTab =
                    initPinningTab xy tabIndex True

                tabs =
                    allTabs model
            in
                ( { model | movingTab = Just <| Pinning pinningTab }
                    |> resetDraggingTabAnimation
                    |> unpinTabAnimation tabs pinningTab
                    |> pinPlaceholderAnimation True
                , Cmd.none
                )

        FinishPinningTab { tabIndex } ->
            ( model
                |> pinTab tabIndex
            , Cmd.none
            )

        FinishUnpinningTab { tabIndex } ->
            ( model
                |> unpinTab tabIndex
            , Cmd.none
            )

        CloseTabAtIndex tabIndex ->
            let
                removeTab tabs =
                    List.take tabIndex tabs ++ List.drop (tabIndex + 1) tabs

                newModel =
                    if tabIndex < List.length model.pinnedTabs then
                        { model | pinnedTabs = removeTab model.pinnedTabs }
                    else
                        { model | tabs = removeTab model.tabs }
            in
                ( newModel, Cmd.none )


boundDraggingTabMouse : Model -> Mouse.Position -> DraggingTab -> DraggingTab
boundDraggingTabMouse { pinnedTabs, tabs } xy draggingTab =
    let
        rightMostX =
            rightMostMouseX draggingTab.isPinned pinnedTabs tabs

        boundedXy =
            if xy.x < (calcTabWidth draggingTab.isPinned) // 2 then
                { xy | x = 0 }
            else if xy.x >= rightMostX then
                { xy | x = rightMostX }
            else
                xy
    in
        { draggingTab | current = boundedXy }


setCurrent : Mouse.Position -> DraggingTab -> DraggingTab
setCurrent xy draggingTab =
    { draggingTab | current = xy }


startDraggingTab : Int -> Bool -> Mouse.Position -> Model -> Model
startDraggingTab tabIndex isPinned xy model =
    { model | movingTab = Just <| Dragging <| initDraggingTab xy tabIndex isPinned }


pinTab : Int -> Model -> Model
pinTab tabIndex model =
    let
        unpinnedIndex =
            tabIndex - List.length model.pinnedTabs

        newTabs =
            List.take unpinnedIndex model.tabs ++ List.drop (unpinnedIndex + 1) model.tabs

        newPinnedTabs =
            model.tabs
                |> List.drop unpinnedIndex
                |> List.head
                |> Maybe.map (\tab -> model.pinnedTabs ++ [ tab ])
                |> Maybe.withDefault model.tabs
    in
        { model
            | tabs = newTabs
            , pinnedTabs = newPinnedTabs
            , movingTab = Nothing
            , movingTabStyle = Animation.style []
            , startingPlaceholderStyle = Animation.style []
            , targetPlaceholderStyle = Animation.style []
        }


unpinTab : Int -> Model -> Model
unpinTab tabIndex model =
    let
        newTabs =
            List.drop tabIndex model.pinnedTabs
                |> List.head
                |> Maybe.map (\tab -> tab :: model.tabs)
                |> Maybe.withDefault model.tabs

        newPinnedTabs =
            List.take tabIndex model.pinnedTabs ++ List.drop (tabIndex + 1) model.pinnedTabs
    in
        { model
            | tabs = newTabs
            , pinnedTabs = newPinnedTabs
            , movingTab = Nothing
            , movingTabStyle = Animation.style []
            , startingPlaceholderStyle = Animation.style []
            , targetPlaceholderStyle = Animation.style []
        }


pinTabAnimation : List String -> PinningTab -> Model -> Model
pinTabAnimation tabs ({ tabIndex, start } as pinningTab) model =
    let
        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertIndex =
            numPinned

        newTabOffset =
            insertIndex * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

        startTabOffset =
            start.x - (offsetFromPinned % tabWidth)

        newMovingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left <| px <| toFloat startTabOffset
                    , Animation.width <| px tabWidth
                    ]
                , Animation.toWith
                    (Animation.easing
                        { duration = 0.1 * second
                        , ease = (\x -> x ^ 1.5)
                        }
                    )
                    [ Animation.left <| px <| toFloat <| newTabOffset
                    , Animation.width <| px pinnedTabWidth
                    ]
                , Animation.Messenger.send (FinishPinningTab pinningTab)
                ]
                model.movingTabStyle
    in
        { model | movingTabStyle = newMovingTabStyle }


unpinTabAnimation : List String -> PinningTab -> Model -> Model
unpinTabAnimation tab ({ start, tabIndex } as pinningTab) model =
    let
        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        newTabOffset =
            (numPinned - 1) * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

        startTabOffset =
            start.x - (start.x % pinnedTabWidth)

        newMovingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left <| px <| toFloat startTabOffset
                    , Animation.width <| px pinnedTabWidth
                    ]
                , Animation.toWith
                    (Animation.easing
                        { duration = 0.1 * second
                        , ease = (\x -> x ^ 1.5)
                        }
                    )
                    [ Animation.left <| px <| toFloat <| newTabOffset
                    , Animation.width <| px tabWidth
                    ]
                , Animation.Messenger.send (FinishUnpinningTab pinningTab)
                ]
                model.movingTabStyle
    in
        { model | movingTabStyle = newMovingTabStyle }


slidingTabAnimation : List String -> SlidingTab -> Model -> Model
slidingTabAnimation tabs ({ start, isPinned, tabIndex } as slidingTab) model =
    let
        dragTabWidth =
            calcTabWidth isPinned

        currentLeft =
            max 0 (start.x - (dragTabWidth // 2))

        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos start.x numPinned numTabs

        insertIndex =
            newTabIndex isPinned insertPos numPinned

        newTabOffset =
            ((clamp 0 numPinned insertIndex) * pinnedTabWidth) + ((clamp 0 numTabs (insertIndex - numPinned)) * tabWidth)

        newDraggingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left <| px <| toFloat <| currentLeft ]
                , Animation.toWith
                    (Animation.easing
                        { duration = 0.1 * second
                        , ease = (\x -> x ^ 1.5)
                        }
                    )
                    [ Animation.left <| px <| toFloat <| newTabOffset ]
                , Animation.Messenger.send (DraggingTabEnd slidingTab { start | x = newTabOffset })
                ]
                model.movingTabStyle
    in
        { model
            | movingTabStyle = newDraggingTabStyle
            , movingTab = Just <| Sliding <| slidingTab
        }


pinPlaceholderAnimation : Bool -> Model -> Model
pinPlaceholderAnimation isPinned model =
    let
        newTargetPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px <| toFloat <| calcTabWidth isPinned ]
                , Animation.toWith
                    (Animation.easing
                        { duration = 0.1 * second
                        , ease = (\x -> x ^ 1.5)
                        }
                    )
                    [ Animation.width <| px 0 ]
                ]
                model.targetPlaceholderStyle

        newStartingPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px 0 ]
                , Animation.toWith
                    (Animation.easing
                        { duration = 0.1 * second
                        , ease = (\x -> x ^ 1.5)
                        }
                    )
                    [ Animation.width <| px <| toFloat <| calcTabWidth (not isPinned) ]
                ]
                model.startingPlaceholderStyle
    in
        { model
            | targetPlaceholderStyle = newStartingPlaceholderStyle
            , startingPlaceholderStyle = newTargetPlaceholderStyle
        }


resetDraggingTabAnimation : Model -> Model
resetDraggingTabAnimation model =
    { model | movingTabStyle = Animation.style [] }



-- Shift right
--            |
-- i = [1, 2, 3, 4, 5, 6]
--                  |
-- o = [1, 2, 4, 5, 3, 6]
-- [1, 2] ++ [4, 5] ++ [3] ++ [6]
-- Shift left
--                  |
-- i = [1, 2, 3, 4, 5, 6]
--            |
-- o = [1, 2, 5, 3, 4, 6]
-- [1, 2] ++ [5] ++ [3, 4] ++ [6]


shiftTabs : Int -> Int -> List String -> List String
shiftTabs newIndex selectedIndex tabs =
    let
        tabsUntilFirst first =
            List.take first tabs

        tabsBetweenShiftRight first second =
            tabs
                |> List.drop (first + 1)
                |> List.take (second - first)

        tabsBetweenShiftLeft first second =
            tabs
                |> List.drop first
                |> List.take (second - first)

        restOfTabs second =
            List.drop (second + 1) tabs

        shiftLeft first second selectedTab =
            List.concat
                [ tabsUntilFirst first
                , [ selectedTab ]
                , tabsBetweenShiftLeft first second
                , restOfTabs second
                ]

        shiftRight first second selectedTab =
            List.concat
                [ tabsUntilFirst first
                , tabsBetweenShiftRight first second
                , [ selectedTab ]
                , restOfTabs second
                ]

        shift selectedIndex newIndex tab =
            if selectedIndex < newIndex then
                shiftRight selectedIndex newIndex tab
            else
                shiftLeft newIndex selectedIndex tab
    in
        List.drop selectedIndex tabs
            |> List.head
            |> Maybe.map (shift selectedIndex newIndex)
            |> Maybe.withDefault tabs


dropTab : SlidingTab -> Mouse.Position -> Model -> Model
dropTab { start, tabIndex, isPinned } end model =
    let
        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos start.x numPinned numTabs

        newIndex =
            newTabIndex isPinned insertPos numPinned

        newTabs =
            shiftTabs newIndex tabIndex (allTabs model)
    in
        { model
            | movingTab = Nothing
            , pinnedTabs = List.take numPinned newTabs
            , tabs = List.drop numPinned newTabs
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        maybeTabByIndex index =
            model
                |> allTabs
                |> List.drop index
                |> List.head

        maybeViewMovingTab movingTab =
            case movingTab of
                Dragging ({ start, current, tabIndex } as tab) ->
                    if current.x == start.x then
                        Nothing
                    else
                        maybeTabByIndex tabIndex
                            |> Maybe.map (viewDraggingTab model.movingTabStyle tab)

                Sliding tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewSlidingTab model.movingTabStyle tab)

                Pinning tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewPinningTab model.movingTabStyle tab)
    in
        div []
            [ viewTabs model
            , model.movingTab
                |> Maybe.andThen maybeViewMovingTab
                |> Maybe.withDefault (text "")
            , model.tabMenu
                |> Maybe.map viewTabMenu
                |> Maybe.withDefault (text "")
            ]


viewStartingPlaceholder : Bool -> Model -> Html Msg
viewStartingPlaceholder startedPinned model =
    div
        ([ class "tab-placeholder"
         , style [ ( "width", toPx <| calcTabWidth startedPinned ) ]
         ]
            ++ Animation.render model.startingPlaceholderStyle
        )
        []


viewTargetPlaceholder : Model -> Html Msg
viewTargetPlaceholder model =
    div
        ([ class "tab-placeholder"
         , style [ ( "width", toPx 0 ) ]
         ]
            ++ Animation.render model.targetPlaceholderStyle
        )
        []


viewPlaceholder : Bool -> Model -> Html Msg
viewPlaceholder isPinned model =
    div
        [ class "tab-placeholder"
        , style [ ( "width", toPx <| calcTabWidth isPinned ) ]
        ]
        []


viewTabs : Model -> Html Msg
viewTabs model =
    let
        numPinned =
            List.length model.pinnedTabs

        insertPos current =
            calcInsertPos current.x numPinned (List.length model.tabs)

        draggableTabs =
            List.indexedMap (viewTab model) (allTabs model)

        reorderedTabsPreview insertIndex tabIndex =
            allTabs model
                |> shiftTabs insertIndex tabIndex
                |> List.indexedMap (viewTab model)

        viewDraggableTabsWithTabPlaceholder isPinned insertIndex tabIndex =
            List.concat
                [ List.take insertIndex (reorderedTabsPreview insertIndex tabIndex)
                , [ viewPlaceholder isPinned model ]
                , List.drop (insertIndex + 1) (reorderedTabsPreview insertIndex tabIndex)
                ]

        viewDraggableTabsWithPinningPlaceholders tabIndex =
            List.concat
                [ List.take numPinned draggableTabs
                , [ viewTargetPlaceholder model ]
                , List.drop numPinned <| List.take tabIndex draggableTabs
                , [ viewStartingPlaceholder False model ]
                , List.drop (tabIndex + 1) draggableTabs
                ]

        viewDraggableTabsWithUnpinningPlaceholders tabIndex =
            List.concat
                [ List.take tabIndex draggableTabs
                , [ viewStartingPlaceholder True model ]
                , List.drop (tabIndex + 1) <| List.take numPinned draggableTabs
                , [ viewTargetPlaceholder model ]
                , List.drop numPinned draggableTabs
                ]

        shouldShowPlaceholder movingTab =
            case movingTab of
                Pinning { startedPinned, tabIndex } ->
                    if startedPinned then
                        Just <| viewDraggableTabsWithUnpinningPlaceholders tabIndex
                    else
                        Just <| viewDraggableTabsWithPinningPlaceholders tabIndex

                Sliding { start, tabIndex, isPinned } ->
                    Just <| viewDraggableTabsWithTabPlaceholder isPinned (insertPos start) tabIndex

                Dragging { start, current, tabIndex, isPinned } ->
                    if start.x == current.x then
                        Nothing
                    else
                        Just <| viewDraggableTabsWithTabPlaceholder isPinned (insertPos current) tabIndex
    in
        div
            [ class "tab-list"
            , draggable "false"
            ]
            (model.movingTab
                |> Maybe.andThen shouldShowPlaceholder
                |> Maybe.withDefault draggableTabs
            )


defaultPrevented : Options
defaultPrevented =
    Html.Events.Options False True


viewTab : Model -> Int -> String -> Html Msg
viewTab model index tab =
    let
        isPinned =
            List.any ((==) tab) model.pinnedTabs
    in
        div
            [ classList
                [ ( "tab", True )
                , ( "tab--selected", model.selected == tab )
                , ( "tab--pinned", isPinned )
                ]
            , contextmenu "tab-menu"
            , onMouseUp (SetActive tab)
            , onWithOptions "contextmenu" defaultPrevented <| Json.map (ToggleTabMenu index isPinned) Mouse.position
            , on "mousedown" <| Json.map (DraggingTabStart index isPinned) Mouse.position
            ]
            [ text tab ]


viewTabMenu : TabMenu -> Html Msg
viewTabMenu { tabIndex, position, isPinned } =
    let
        px int =
            (toString int) ++ "px"
    in
        div
            [ class "tab-context-menu__backdrop"
            , onMouseDown CloseTabMenu
            , onWithOptions "contextmenu" defaultPrevented <| Json.map (\_ -> CloseTabMenu) (Json.succeed "contextClick")
            ]
            [ nav
                [ id "tab-menu"
                , onWithOptions "contextmenu" defaultPrevented <| Json.map (ToggleTabMenu tabIndex isPinned) Mouse.position
                , class "tab-context-menu"
                , style
                    [ ( "top", px position.y )
                    , ( "left", px position.x )
                    ]
                ]
                [ ul [ class "tab-context-menu__list" ]
                    (List.map (viewTabMenuItem position) (tabMenuItems isPinned tabIndex))
                ]
            ]


viewTabMenuItem : Mouse.Position -> TabMenuItem -> Html Msg
viewTabMenuItem pos menuItem =
    let
        menuItemBehaviors =
            case menuItem of
                PinTab tabIndex ->
                    [ onMouseDown (PinTabAtIndex tabIndex pos) ]

                UnpinTab tabIndex ->
                    [ onMouseDown (UnpinTabAtIndex tabIndex pos) ]

                CloseTab tabIndex ->
                    [ onMouseDown (CloseTabAtIndex tabIndex) ]

                _ ->
                    []
    in
        li
            ([ class "tab-context-menu__item" ]
                ++ menuItemBehaviors
            )
            [ text <| tabMenuItemToString menuItem ]


viewSlidingTab : Animation.Messenger.State Msg -> SlidingTab -> String -> Html Msg
viewSlidingTab movingTabStyle { start, isPinned } tab =
    div
        ([ classList
            [ ( "tab", True )
            , ( "moving-tab", True )
            , ( "tab--pinned", isPinned )
            ]
         , draggable "false"
         , style [ ( "left", toString start ++ "px" ) ]
         ]
            ++ Animation.render movingTabStyle
        )
        [ text tab ]


viewPinningTab : Animation.Messenger.State Msg -> PinningTab -> String -> Html Msg
viewPinningTab movingTabStyle { start, startedPinned } tab =
    let
        tabOffset =
            start.x - (start.x % calcTabWidth startedPinned)
    in
        div
            ([ classList
                [ ( "tab", True )
                , ( "moving-tab", True )
                ]
             , draggable "false"
             , style
                [ ( "top", toPx 0 )
                , ( "left", toPx tabOffset )
                , ( "width", (toPx << calcTabWidth) startedPinned )
                ]
             ]
                ++ Animation.render movingTabStyle
            )
            [ text tab ]


viewDraggingTab : Animation.Messenger.State Msg -> DraggingTab -> String -> Html Msg
viewDraggingTab movingTabStyle { start, current, isPinned } tab =
    let
        px int =
            (toString int) ++ "px"

        left =
            if current.x < (calcTabWidth isPinned // 2) then
                px 0
            else
                px (current.x - (calcTabWidth isPinned // 2))
    in
        div
            ([ classList
                [ ( "tab", True )
                , ( "moving-tab", True )
                , ( "tab--pinned", isPinned )
                ]
             , draggable "false"
             , onMouseUp (SetActive tab)
             , style
                [ ( "top", px 0 )
                , ( "left", left )
                ]
             ]
                ++ Animation.render movingTabStyle
            )
            [ text tab ]



-- HELPERS


allTabs : Model -> List String
allTabs model =
    model.pinnedTabs ++ model.tabs


newTabIndex : Bool -> Int -> Int -> Int
newTabIndex isPinned insertPos numPinned =
    if isPinned && insertPos > (numPinned - 1) then
        (numPinned - 1)
    else if not isPinned && insertPos < numPinned then
        numPinned
    else
        insertPos



-- 600 // 50 = 12, cap at 2, 2
-- 600 // 100 = 6, cap at 4, 4
--
-- 150 // 50 = 3, cap at 2, 2
-- 150 // 100 = 1, cap at 4, 1
--
-- 100 // 50 = 2, cap at 3, 2
-- 100 - (2 * 50) // 100 = 0, cap at 3, 0


calcInsertPos : Int -> Int -> Int -> Int
calcInsertPos xPos numPinned numTabs =
    let
        posFromPinned =
            pinnedTabWidth * numPinned

        pinnedTabIndex =
            clamp 0 numPinned ((xPos // pinnedTabWidth))

        tabIndex =
            clamp 0 numTabs (((xPos - posFromPinned) // tabWidth))
    in
        pinnedTabIndex + tabIndex


calcTabWidth : Bool -> number
calcTabWidth isPinned =
    if isPinned then
        pinnedTabWidth
    else
        tabWidth


rightMostMouseX : Bool -> List String -> List String -> Int
rightMostMouseX isPinned pinnedTabs tabs =
    if isPinned then
        allTabsWidth pinnedTabs tabs - (pinnedTabWidth // 2)
    else
        (allTabsWidth pinnedTabs tabs) - halfTab


toPx : Int -> String
toPx int =
    (toString int) ++ "px"



-- CONSTANTS


tabWidth : number
tabWidth =
    100


pinnedTabWidth : number
pinnedTabWidth =
    50


halfTab : Int
halfTab =
    tabWidth // 2


allTabsWidth : List String -> List String -> Int
allTabsWidth pinnedTabs tabs =
    (pinnedTabWidth * List.length pinnedTabs) + (tabWidth * List.length tabs)
