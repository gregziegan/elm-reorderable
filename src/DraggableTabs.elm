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
import Svgs exposing (viewElmLogo)


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


type alias Config =
    { containerWidth : Float
    , maxTabWidth : Float
    , minTabWidth : Float
    , pinnedTabWidth : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias SlidingTab =
    { start : Position
    , tabIndex : Int
    , isPinned : Bool
    }


type alias PinningTab =
    { start : Position
    , tabIndex : Int
    , startedPinned : Bool
    }


type alias DraggingTab =
    { start : Position
    , current : Position
    , tabIndex : Int
    , isPinned : Bool
    }


type MovingTab
    = Sliding SlidingTab
    | Pinning PinningTab
    | Dragging DraggingTab


type alias TabMenu =
    { tabIndex : Int
    , position : Position
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
        ( { tabs = [ "Elm", "Is", "A", "Nice", "Programming", "Language" ]
          , pinnedTabs = []
          , selected = "Elm"
          , movingTab = Nothing
          , movingTabStyle = Animation.style []
          , tabMenu = Nothing
          , keyboardModel = keyboardModel
          , targetPlaceholderStyle = Animation.style []
          , startingPlaceholderStyle = Animation.style []
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


initTabMenu : Int -> Position -> Bool -> TabMenu
initTabMenu tabIndex xy isPinned =
    { tabIndex = tabIndex
    , position = xy
    , isPinned = isPinned
    }


initDraggingTab : Position -> Int -> Bool -> DraggingTab
initDraggingTab start tabIndex isPinned =
    { start = start
    , current = start
    , tabIndex = tabIndex
    , isPinned = isPinned
    }


initSlidingTab : Position -> Int -> Bool -> SlidingTab
initSlidingTab start tabIndex isPinned =
    { start = start
    , tabIndex = tabIndex
    , isPinned = isPinned
    }


initPinningTab : Position -> Int -> Bool -> PinningTab
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
    | DraggingTabEnd SlidingTab Position
    | AnimateMessenger Animation.Msg
    | ToggleTabMenu Int Bool Mouse.Position
    | CloseTabMenu
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | PinTabAtIndex Int Position
    | UnpinTabAtIndex Int Position
    | FinishPinningTab PinningTab
    | FinishUnpinningTab PinningTab
    | CloseTabAtIndex Int


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        SetActive tabId ->
            ( { model | selected = tabId }, Cmd.none )

        DraggingTabStart tabIndex isPinned xy ->
            ( model
                |> startDraggingTab tabIndex isPinned (toPosition xy)
            , Cmd.none
            )

        DraggingTabContinues draggingTab xy ->
            let
                pos =
                    toPosition xy

                newDraggingTab =
                    draggingTab
                        |> setCurrent pos
                        |> boundDraggingTabMouse config model pos
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
                        |> slidingTabAnimation config (allTabs model) slidingTab
                    , Cmd.none
                    )

        DraggingTabEnd slidingTab xy ->
            ( model
                |> dropTab config slidingTab xy
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
                        Just <| initTabMenu tabIndex (toPosition xy) isPinned
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
                    |> pinTabAnimation config tabs pinningTab
                    |> pinPlaceholderAnimation config False
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
                    |> unpinTabAnimation config tabs pinningTab
                    |> pinPlaceholderAnimation config True
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


boundDraggingTabMouse : Config -> Model -> Position -> DraggingTab -> DraggingTab
boundDraggingTabMouse config { pinnedTabs, tabs } pos draggingTab =
    let
        numTabs =
            List.length tabs

        rightMostX =
            rightMostMouseX config draggingTab.isPinned pinnedTabs tabs

        varTabWidth =
            calcVarTabWidth config draggingTab.isPinned numTabs

        boundedXy =
            if pos.x < varTabWidth / 2 then
                { pos | x = 0 }
            else if pos.x >= rightMostX then
                { pos | x = rightMostX }
            else
                pos
    in
        { draggingTab | current = boundedXy }


setCurrent : Position -> DraggingTab -> DraggingTab
setCurrent xy draggingTab =
    { draggingTab | current = xy }


startDraggingTab : Int -> Bool -> Position -> Model -> Model
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


pinTabAnimation : Config -> List String -> PinningTab -> Model -> Model
pinTabAnimation ({ pinnedTabWidth } as config) tabs ({ tabIndex, start } as pinningTab) model =
    let
        numPinned =
            (toFloat << List.length) model.pinnedTabs

        numTabs =
            List.length model.tabs

        newTabOffset =
            numPinned * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

        varTabWidth =
            calcVarTabWidth config False numTabs

        startTabOffset =
            start.x - toFloat (floor offsetFromPinned % floor varTabWidth)

        newMovingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px startTabOffset)
                    , Animation.width (px varTabWidth)
                    ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px newTabOffset)
                    , Animation.width (px pinnedTabWidth)
                    ]
                , Animation.Messenger.send (FinishPinningTab pinningTab)
                ]
                model.movingTabStyle
    in
        { model | movingTabStyle = newMovingTabStyle }


unpinTabAnimation : Config -> List String -> PinningTab -> Model -> Model
unpinTabAnimation ({ pinnedTabWidth } as config) tab ({ start, tabIndex } as pinningTab) model =
    let
        numPinned =
            (toFloat << List.length) model.pinnedTabs

        numTabs =
            List.length model.tabs

        newTabOffset =
            (numPinned - 1) * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

        varTabWidth =
            calcVarTabWidth config False numTabs

        startTabOffset =
            start.x - toFloat ((floor start.x) % (floor pinnedTabWidth))

        newMovingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px startTabOffset)
                    , Animation.width (px pinnedTabWidth)
                    ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px newTabOffset)
                    , Animation.width (px varTabWidth)
                    ]
                , Animation.Messenger.send (FinishUnpinningTab pinningTab)
                ]
                model.movingTabStyle
    in
        { model | movingTabStyle = newMovingTabStyle }


slidingTabAnimation : Config -> List String -> SlidingTab -> Model -> Model
slidingTabAnimation config tabs ({ start, isPinned, tabIndex } as slidingTab) model =
    let
        dragTabWidth =
            calcVarTabWidth config isPinned (List.length model.tabs)

        currentLeft =
            max 0 (start.x - (dragTabWidth / 2))

        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos config start.x numPinned numTabs

        insertIndex =
            newTabIndex isPinned insertPos numPinned

        varTabWidth =
            calcVarTabWidth config False (List.length model.tabs)

        newTabOffset =
            (toFloat (clamp 0 numPinned insertIndex) * config.pinnedTabWidth) + (toFloat (clamp 0 numTabs (insertIndex - numPinned)) * varTabWidth)

        newDraggingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px currentLeft) ]
                , Animation.toWith
                    fastDrift
                    [ Animation.left (px newTabOffset) ]
                , Animation.Messenger.send (DraggingTabEnd slidingTab { start | x = newTabOffset })
                ]
                model.movingTabStyle
    in
        { model
            | movingTabStyle = newDraggingTabStyle
            , movingTab = Just <| Sliding <| slidingTab
        }


pinPlaceholderAnimation : Config -> Bool -> Model -> Model
pinPlaceholderAnimation config isPinned model =
    let
        numTabs =
            List.length model.tabs

        newTargetPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px <| calcVarTabWidth config isPinned numTabs ]
                , Animation.to
                    [ Animation.width <| px 0 ]
                ]
                model.targetPlaceholderStyle

        newStartingPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px 0 ]
                , Animation.to
                    [ Animation.width <| px <| calcVarTabWidth config (not isPinned) numTabs ]
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


{-|
  Shift right
             |
  i = [1, 2, 3, 4, 5, 6]
                   |
  o = [1, 2, 4, 5, 3, 6]
  [1, 2] ++ [4, 5] ++ [3] ++ [6]
  Shift left
                   |
  i = [1, 2, 3, 4, 5, 6]
             |
  o = [1, 2, 5, 3, 4, 6]
  [1, 2] ++ [5] ++ [3, 4] ++ [6]
-}
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


dropTab : Config -> SlidingTab -> Position -> Model -> Model
dropTab config { start, tabIndex, isPinned } end model =
    let
        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos config start.x numPinned numTabs

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


view : Config -> Model -> Html Msg
view config model =
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
                            |> Maybe.map (viewDraggingTab config model tab)

                Sliding tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewSlidingTab config model tab)

                Pinning tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewPinningTab config model tab tab.tabIndex)
    in
        div []
            [ viewTabs config model
            , model.movingTab
                |> Maybe.andThen maybeViewMovingTab
                |> Maybe.withDefault (text "")
            , model.tabMenu
                |> Maybe.map viewTabMenu
                |> Maybe.withDefault (text "")
            ]


viewStartingPlaceholder : Config -> Bool -> Model -> Html Msg
viewStartingPlaceholder config startedPinned model =
    div
        ([ class "tab-placeholder"
         , style [ ( "width", toPx <| calcVarTabWidth config startedPinned (List.length model.tabs) ) ]
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


viewPlaceholder : Config -> Bool -> Model -> Html Msg
viewPlaceholder config isPinned model =
    div
        [ class "tab-placeholder"
        , style [ ( "width", toPx <| calcVarTabWidth config isPinned (List.length model.tabs) ) ]
        ]
        []


viewTabs : Config -> Model -> Html Msg
viewTabs config model =
    let
        numPinned =
            List.length model.pinnedTabs

        insertPos current =
            calcInsertPos config current.x numPinned (List.length model.tabs)

        draggableTabs =
            List.indexedMap (viewTab config model) (allTabs model)

        reorderedTabsPreview insertIndex tabIndex =
            allTabs model
                |> shiftTabs insertIndex tabIndex
                |> List.indexedMap (viewTab config model)

        viewDraggableTabsWithTabPlaceholder isPinned insertIndex tabIndex =
            List.concat
                [ List.take insertIndex (reorderedTabsPreview insertIndex tabIndex)
                , [ viewPlaceholder config isPinned model ]
                , List.drop (insertIndex + 1) (reorderedTabsPreview insertIndex tabIndex)
                ]

        viewDraggableTabsWithPinningPlaceholders tabIndex =
            List.concat
                [ List.take numPinned draggableTabs
                , [ viewTargetPlaceholder model ]
                , List.drop numPinned <| List.take tabIndex draggableTabs
                , [ viewStartingPlaceholder config False model ]
                , List.drop (tabIndex + 1) draggableTabs
                ]

        viewDraggableTabsWithUnpinningPlaceholders tabIndex =
            List.concat
                [ List.take tabIndex draggableTabs
                , [ viewStartingPlaceholder config True model ]
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


viewTab : Config -> Model -> Int -> String -> Html Msg
viewTab config model index tab =
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
            , style [ ( "width", (toPx << calcVarTabWidth config isPinned) (List.length model.tabs) ) ]
            , contextmenu "tab-menu"
            , onMouseUp (SetActive tab)
            , onWithOptions "contextmenu" defaultPrevented <| Json.map (ToggleTabMenu index isPinned) Mouse.position
            , on "mousedown" <| Json.map (DraggingTabStart index isPinned) Mouse.position
            ]
            (viewTabContent isPinned tab)


viewTabContent : Bool -> String -> List (Html Msg)
viewTabContent isPinned tab =
    if isPinned then
        [ div [ class "tab-logo" ] [ viewElmLogo ] ]
    else
        [ div [ class "tab-logo" ] [ viewElmLogo ]
        , div [ class "tab-info" ]
            [ h4 [ class "tab-title" ] [ text tab ]
            , p [ class "tab-subtitle" ] [ text "drag me!" ]
            ]
        ]


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


viewTabMenuItem : Position -> TabMenuItem -> Html Msg
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


viewSlidingTab : Config -> Model -> SlidingTab -> String -> Html Msg
viewSlidingTab config model { start, isPinned } tab =
    let
        slidingTabWidth =
            calcVarTabWidth config isPinned (List.length model.tabs)
    in
        div
            ([ classList
                [ ( "tab", True )
                , ( "moving-tab", True )
                , ( "tab--pinned", isPinned )
                ]
             , draggable "false"
             , style
                [ ( "left", toPx (start.x - slidingTabWidth / 2) )
                , ( "width", toPx slidingTabWidth )
                ]
             ]
                ++ Animation.render model.movingTabStyle
            )
            (viewTabContent isPinned tab)


viewPinningTab : Config -> Model -> PinningTab -> Int -> String -> Html Msg
viewPinningTab config model { start, startedPinned } index tab =
    let
        movingTabWidth =
            calcVarTabWidth config startedPinned (List.length model.tabs)

        tabOffset =
            tabPosition config startedPinned index (List.length model.pinnedTabs) (List.length model.tabs)
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
                , ( "width", toPx movingTabWidth )
                ]
             ]
                ++ Animation.render model.movingTabStyle
            )
            (viewTabContent startedPinned tab)


viewDraggingTab : Config -> Model -> DraggingTab -> String -> Html Msg
viewDraggingTab config model { start, current, isPinned } tab =
    let
        draggingTabWidth =
            calcVarTabWidth config isPinned (List.length model.tabs)

        left =
            if current.x < (draggingTabWidth / 2) then
                0
            else
                current.x - (draggingTabWidth / 2)
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
                [ ( "top", toPx 0 )
                , ( "left", toPx left )
                , ( "width", toPx draggingTabWidth )
                ]
             ]
                ++ Animation.render model.movingTabStyle
            )
            (viewTabContent isPinned tab)



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


{-|
  Calculates which tab index the current mouse position is over
-}
calcInsertPos : Config -> Float -> Int -> Int -> Int
calcInsertPos config xPos numPinned numTabs =
    let
        { pinnedTabWidth } =
            config

        posFromPinned =
            pinnedTabWidth * (toFloat numPinned)

        pinnedTabIndex =
            floor <| clamp 0.0 (toFloat numPinned) (xPos / pinnedTabWidth)

        tabIndex =
            floor <| clamp 0.0 (toFloat numTabs) ((xPos - posFromPinned) / calcVarTabWidth config False numTabs)
    in
        pinnedTabIndex + tabIndex


calcVarTabWidth : Config -> Bool -> Int -> Float
calcVarTabWidth config isPinned numTabs =
    let
        { minTabWidth, maxTabWidth, containerWidth, pinnedTabWidth } =
            config

        totalTabWidth =
            (toFloat numTabs * maxTabWidth)

        varTabWidth =
            if totalTabWidth > containerWidth then
                clamp minTabWidth maxTabWidth (containerWidth / toFloat numTabs)
            else
                maxTabWidth
    in
        if isPinned then
            pinnedTabWidth
        else
            varTabWidth


rightMostMouseX : Config -> Bool -> List String -> List String -> Float
rightMostMouseX config isPinned pinnedTabs tabs =
    if isPinned then
        allTabsWidth config pinnedTabs tabs - (config.pinnedTabWidth / 2)
    else
        (allTabsWidth config pinnedTabs tabs) - (calcVarTabWidth config False (List.length tabs) / 2)


toPx : number -> String
toPx num =
    (toString num) ++ "px"


{-|
Calculate the x position for a tab with an index
-}
tabPosition : Config -> Bool -> Int -> Int -> Int -> Float
tabPosition config isPinned index numPinned numTabs =
    if isPinned then
        (toFloat index) * config.pinnedTabWidth
    else
        (toFloat numPinned * config.pinnedTabWidth)
            + (toFloat (index - numPinned) * (calcVarTabWidth config False numTabs))


toPosition : Mouse.Position -> Position
toPosition { x, y } =
    { x = toFloat x
    , y = toFloat y
    }


allTabsWidth : Config -> List String -> List String -> Float
allTabsWidth ({ pinnedTabWidth } as config) pinnedTabs tabs =
    let
        numTabs =
            (toFloat << List.length) tabs

        numPinned =
            (toFloat << List.length) pinnedTabs

        varTabWidth =
            calcVarTabWidth config False (List.length tabs)
    in
        (pinnedTabWidth * numPinned) + (varTabWidth * numTabs)



-- CONSTANTS


fastDrift : Animation.Interpolation
fastDrift =
    Animation.easing
        { duration = 0.1 * second
        , ease = (\x -> x ^ 1.5)
        }
