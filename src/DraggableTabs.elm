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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                        |> boundDraggingTabMouse model pos
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


boundDraggingTabMouse : Model -> Position -> DraggingTab -> DraggingTab
boundDraggingTabMouse { pinnedTabs, tabs } pos draggingTab =
    let
        numTabs =
            List.length tabs

        rightMostX =
            rightMostMouseX draggingTab.isPinned pinnedTabs tabs

        boundedXy =
            if pos.x < (calcVarTabWidth draggingTab.isPinned numTabs) / 2 then
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


pinTabAnimation : List String -> PinningTab -> Model -> Model
pinTabAnimation tabs ({ tabIndex, start } as pinningTab) model =
    let
        numPinned =
            (toFloat << List.length) model.pinnedTabs

        numTabs =
            (toFloat << List.length) model.tabs

        newTabOffset =
            numPinned * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

        startTabOffset =
            start.x - toFloat (floor offsetFromPinned % floor tabWidth)

        newMovingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left (px startTabOffset)
                    , Animation.width (px tabWidth)
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


unpinTabAnimation : List String -> PinningTab -> Model -> Model
unpinTabAnimation tab ({ start, tabIndex } as pinningTab) model =
    let
        numPinned =
            (toFloat << List.length) model.pinnedTabs

        numTabs =
            (toFloat << List.length) model.tabs

        newTabOffset =
            (numPinned - 1) * pinnedTabWidth

        offsetFromPinned =
            start.x - newTabOffset

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
                    , Animation.width (px tabWidth)
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
            calcVarTabWidth isPinned (List.length model.tabs)

        currentLeft =
            max 0 (start.x - (dragTabWidth / 2))

        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos start.x numPinned numTabs

        insertIndex =
            newTabIndex isPinned insertPos numPinned

        varTabWidth =
            calcVarTabWidth False (List.length model.tabs)

        newTabOffset =
            (toFloat (clamp 0 numPinned insertIndex) * pinnedTabWidth) + (toFloat (clamp 0 numTabs (insertIndex - numPinned)) * varTabWidth)

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


pinPlaceholderAnimation : Bool -> Model -> Model
pinPlaceholderAnimation isPinned model =
    let
        numTabs =
            List.length model.tabs

        newTargetPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px <| calcVarTabWidth isPinned numTabs ]
                , Animation.to
                    [ Animation.width <| px 0 ]
                ]
                model.targetPlaceholderStyle

        newStartingPlaceholderStyle =
            Animation.interrupt
                [ Animation.set [ Animation.width <| px 0 ]
                , Animation.to
                    [ Animation.width <| px <| calcVarTabWidth (not isPinned) numTabs ]
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


dropTab : SlidingTab -> Position -> Model -> Model
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
                            |> Maybe.map (viewDraggingTab model tab)

                Sliding tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewSlidingTab model tab)

                Pinning tab ->
                    maybeTabByIndex tab.tabIndex
                        |> Maybe.map (viewPinningTab model tab tab.tabIndex)
    in
        div []
            [ viewTabs model
            , model.movingTab
                |> Maybe.andThen maybeViewMovingTab
                |> Maybe.withDefault (text "")
            , model.tabMenu
                |> Maybe.map viewTabMenu
                |> Maybe.withDefault (text "")
              -- , viewVarTabs model
            ]


viewVarTabs model =
    div [ class "var-tabs" ]
        (List.map viewVarTab model.tabs)


viewVarTab tab =
    div [ class "var-tab" ]
        [ div [ class "tab-logo" ] [ viewElmLogo ]
        , div [ class "tab-info" ]
            [ h4 [ class "tab-title" ] [ text tab ]
            , p [ class "tab-subtitle" ] [ text "Elm" ]
            ]
        ]


viewStartingPlaceholder : Bool -> Model -> Html Msg
viewStartingPlaceholder startedPinned model =
    div
        ([ class "tab-placeholder"
         , style [ ( "width", toPx <| calcVarTabWidth startedPinned (List.length model.tabs) ) ]
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
        , style [ ( "width", toPx <| calcVarTabWidth isPinned (List.length model.tabs) ) ]
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
            [ class "var-tabs"
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
                [ ( "var-tab", True )
                , ( "tab--selected", model.selected == tab )
                , ( "tab--pinned", isPinned )
                ]
            , style [ ( "width", (toPx << calcVarTabWidth isPinned) (List.length model.tabs) ) ]
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


viewSlidingTab : Model -> SlidingTab -> String -> Html Msg
viewSlidingTab model { start, isPinned } tab =
    let
        slidingTabWidth =
            calcVarTabWidth isPinned (List.length model.tabs)
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
            [ text tab ]


viewPinningTab : Model -> PinningTab -> Int -> String -> Html Msg
viewPinningTab model { start, startedPinned } index tab =
    let
        movingTabWidth =
            calcVarTabWidth startedPinned (List.length model.tabs)

        tabOffset =
            tabPosition startedPinned index (List.length model.pinnedTabs) (List.length model.tabs)
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
            [ text tab ]


viewDraggingTab : Model -> DraggingTab -> String -> Html Msg
viewDraggingTab model { start, current, isPinned } tab =
    let
        draggingTabWidth =
            calcVarTabWidth isPinned (List.length model.tabs)

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


{-|
  Calculates which tab index the current mouse position is over
-}
calcInsertPos : Float -> Int -> Int -> Int
calcInsertPos xPos numPinned numTabs =
    let
        posFromPinned =
            pinnedTabWidth * (toFloat numPinned)

        pinnedTabIndex =
            floor <| clamp 0.0 (toFloat numPinned) (xPos / pinnedTabWidth)

        tabIndex =
            floor <| clamp 0.0 (toFloat numTabs) ((xPos - posFromPinned) / calcVarTabWidth False numTabs)
    in
        pinnedTabIndex + tabIndex


calcTabWidth : Bool -> Float
calcTabWidth isPinned =
    if isPinned then
        pinnedTabWidth
    else
        tabWidth


calcVarTabWidth : Bool -> Int -> Float
calcVarTabWidth isPinned numTabs =
    let
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


rightMostMouseX : Bool -> List String -> List String -> Float
rightMostMouseX isPinned pinnedTabs tabs =
    if isPinned then
        allTabsWidth pinnedTabs tabs - (pinnedTabWidth / 2)
    else
        (allTabsWidth pinnedTabs tabs) - (calcVarTabWidth False (List.length tabs) / 2)


toPx : number -> String
toPx num =
    (toString num) ++ "px"


{-|
Calculate the x position for a tab with an index
-}
tabPosition : Bool -> Int -> Int -> Int -> Float
tabPosition isPinned index numPinned numTabs =
    if isPinned then
        (toFloat index) * pinnedTabWidth
    else
        (toFloat numPinned * pinnedTabWidth)
            + (toFloat (index - numPinned) * (calcVarTabWidth False numTabs))


toPosition : Mouse.Position -> Position
toPosition { x, y } =
    { x = toFloat x
    , y = toFloat y
    }



-- CONSTANTS


containerWidth =
    910


tabBasis =
    0.2


maxTabWidth =
    202


minTabWidth =
    102


tabWidth : Float
tabWidth =
    102


pinnedTabWidth : Float
pinnedTabWidth =
    52


allTabsWidth : List String -> List String -> Float
allTabsWidth pinnedTabs tabs =
    let
        numTabs =
            (toFloat << List.length) tabs

        numPinned =
            (toFloat << List.length) pinnedTabs

        varTabWidth =
            calcVarTabWidth False (List.length tabs)
    in
        (pinnedTabWidth * numPinned) + (varTabWidth * numTabs)


fastDrift : Animation.Interpolation
fastDrift =
    Animation.easing
        { duration = 0.1 * second
        , ease = (\x -> x ^ 1.5)
        }
