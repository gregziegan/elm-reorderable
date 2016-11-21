module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (id, class, classList, draggable, style, contextmenu)
import Html.Events exposing (onClick, onMouseDown, onMouseEnter, on, onWithOptions)
import Json.Decode as Json
import Mouse
import Animation exposing (px)
import Animation.Messenger
import Time exposing (second)
import Keyboard.Extra


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        List.append [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions ] <|
            case model.tabDrag of
                Nothing ->
                    []

                Just tabDrag ->
                    (if tabDrag.isSliding then
                        [ Animation.subscription AnimateMessenger [ model.draggingTabStyle ] ]
                     else
                        [ Mouse.moves (TabDragging tabDrag)
                        , Mouse.ups (TabDragEnding tabDrag)
                        , Animation.subscription Animate [ model.tabPlaceholderStyle ]
                        ]
                    )



-- MODEL


type alias TabDrag =
    { start : Mouse.Position
    , current : Mouse.Position
    , tabIndex : Int
    , isPinned : Bool
    , isSliding : Bool
    }


type alias TabMenu =
    { tabIndex : Int
    , position : Mouse.Position
    }


type alias Model =
    { tabs : List String
    , pinnedTabs : List String
    , selected : String
    , tabDrag : Maybe TabDrag
    , tabPlaceholderStyle : Animation.State
    , draggingTabStyle : Animation.Messenger.State Msg
    , tabMenu : Maybe TabMenu
    , keyboardModel : Keyboard.Extra.Model
    }


type TabMenuItem
    = Reload Int
    | Duplicate Int
    | PinTab Int
    | CloseTab Int


tabMenuItemToString menuItem =
    case menuItem of
        Reload _ ->
            "Reload"

        Duplicate _ ->
            "Duplicate"

        PinTab _ ->
            "Pin Tab"

        CloseTab _ ->
            "Close Tab"


tabMenuItems tabIndex =
    [ Reload tabIndex, Duplicate tabIndex, PinTab tabIndex, CloseTab tabIndex ]


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { tabs = [ "Tab 4", "Tab 5", "Tab 6" ]
          , pinnedTabs = [ "Tab 1", "Tab 2", "Tab 3" ]
          , selected = "Tab 1"
          , tabDrag = Nothing
          , tabPlaceholderStyle = initTabPlaceholderStyle
          , draggingTabStyle = initDraggingTabStyle
          , tabMenu = Nothing
          , keyboardModel = keyboardModel
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


initTabPlaceholderStyle =
    Animation.style []


initDraggingTabStyle =
    Animation.style []


initTabMenu tabIndex xy =
    { tabIndex = tabIndex
    , position = xy
    }



-- UPDATE


type Msg
    = SetActive String
    | TabDragStart Int Bool Mouse.Position
    | TabDragging TabDrag Mouse.Position
    | TabDragEnding TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position
    | Animate Animation.Msg
    | AnimateMessenger Animation.Msg
    | ToggleTabMenu Int Mouse.Position
    | CloseTabMenu
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | PinTabAtIndex Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive tabId ->
            ( { model | selected = tabId }, Cmd.none )

        TabDragStart tabIndex isPinned xy ->
            ( model
                |> startTabDrag tabIndex isPinned xy
            , Cmd.none
            )

        TabDragging tabDrag xy ->
            ( { model | tabDrag = Just <| normalizeTabDragMouse model xy <| setCurrent xy tabDrag }
            , Cmd.none
            )

        TabDragEnding tabDrag xy ->
            let
                newTabDrag =
                    Just <| startTabSlide <| normalizeTabDragMouse model xy <| setCurrent xy tabDrag
            in
                ( { model | tabDrag = newTabDrag }
                    |> resetTabPlaceholderAnimation
                    |> slideDraggingTabAnimation (allTabs model) tabDrag
                , Cmd.none
                )

        TabDragEnd tabDrag xy ->
            ( model
                |> dropTab tabDrag xy
                |> resetTabPlaceholderAnimation
                |> resetDraggingTabAnimation
            , Cmd.none
            )

        Animate animMsg ->
            let
                tabPlaceholderStyle =
                    Animation.update animMsg model.tabPlaceholderStyle
            in
                ( { model
                    | tabPlaceholderStyle = tabPlaceholderStyle
                  }
                , Cmd.none
                )

        AnimateMessenger animMsg ->
            let
                ( draggingTabStyle, cmds ) =
                    Animation.Messenger.update animMsg model.draggingTabStyle
            in
                ( { model
                    | draggingTabStyle = draggingTabStyle
                  }
                , cmds
                )

        ToggleTabMenu tabIndex xy ->
            ( { model
                | tabMenu =
                    case model.tabMenu of
                        Just tabMenu ->
                            Nothing

                        Nothing ->
                            Just <| initTabMenu tabIndex xy
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
                    , tabDrag =
                        if model.tabDrag /= Nothing && escapeIsPressed then
                            Nothing
                        else
                            model.tabDrag
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

        PinTabAtIndex tabIndex ->
            let
                unpinnedIndex =
                    tabIndex - (List.length model.pinnedTabs)
            in
                ( { model
                    | tabs = List.take unpinnedIndex model.tabs ++ List.drop (unpinnedIndex + 1) model.tabs
                    , pinnedTabs =
                        getTabByIndex (allTabs model) tabIndex
                            |> Maybe.map (\tab -> model.pinnedTabs ++ [ tab ])
                            |> Maybe.withDefault model.tabs
                  }
                , Cmd.none
                )


normalizeTabDragMouse { pinnedTabs, tabs } xy tabDrag =
    let
        rightMostX =
            rightMostMouse tabDrag.isPinned pinnedTabs tabs

        boundedXy =
            if xy.x < (calcTabWidth tabDrag.isPinned) then
                { xy | x = 0 }
            else if xy.x >= rightMostX then
                { xy | x = rightMostX }
            else
                xy
    in
        { tabDrag | current = boundedXy }


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }


startTabDrag tabIndex isPinned xy model =
    { model | tabDrag = Just <| TabDrag xy xy tabIndex isPinned False }


startTabSlide tabDrag =
    { tabDrag | isSliding = True }


resetTabPlaceholderAnimation model =
    { model | tabPlaceholderStyle = initTabPlaceholderStyle }


slideDraggingTabAnimation tabs ({ current, isPinned } as tabDrag) model =
    let
        dragTabWidth =
            calcTabWidth isPinned

        currentLeft =
            current.x - (dragTabWidth // 2)

        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos current.x numPinned numTabs

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
                , Animation.Messenger.send (TabDragEnd tabDrag { current | x = newTabOffset })
                ]
                model.draggingTabStyle
    in
        if current.x == 0 || current.x > rightMostMouse isPinned model.pinnedTabs tabs then
            model
        else
            { model | draggingTabStyle = newDraggingTabStyle }


resetDraggingTabAnimation model =
    { model | draggingTabStyle = initDraggingTabStyle }



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


dropTab : TabDrag -> Mouse.Position -> Model -> Model
dropTab { start, current, tabIndex, isPinned } end model =
    let
        numPinned =
            List.length model.pinnedTabs

        numTabs =
            List.length model.tabs

        insertPos =
            calcInsertPos current.x numPinned numTabs

        newIndex =
            newTabIndex isPinned insertPos numPinned

        newTabs =
            shiftTabs newIndex tabIndex (allTabs model)
    in
        { model
            | tabDrag = Nothing
            , pinnedTabs = List.take numPinned newTabs
            , tabs = List.drop numPinned newTabs
        }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTabs model
        , model.tabDrag
            |> Maybe.andThen
                (\({ tabIndex, start, current } as tabDrag) ->
                    if start.x == current.x then
                        Nothing
                    else
                        getTabByIndex (allTabs model) tabIndex
                            |> Maybe.map (viewDraggingTab model.draggingTabStyle tabDrag)
                )
            |> Maybe.withDefault (text "")
        , model.tabMenu
            |> Maybe.map viewTabMenu
            |> Maybe.withDefault (text "")
        ]


viewTabPlaceholder : Model -> Html Msg
viewTabPlaceholder model =
    let
        px int =
            (toString int) ++ "px"

        isPinned =
            model.tabDrag
                |> Maybe.map .isPinned
                |> Maybe.withDefault False
    in
        div
            (Animation.render model.tabPlaceholderStyle
                ++ [ class "tab-placeholder"
                   , style [ ( "width", px <| calcTabWidth isPinned ) ]
                   ]
            )
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

        viewDraggableTabsWithTabPlaceholder insertIndex tabIndex =
            List.concat
                [ List.take insertIndex (reorderedTabsPreview insertIndex tabIndex)
                , [ viewTabPlaceholder model ]
                , List.drop (insertIndex + 1) (reorderedTabsPreview insertIndex tabIndex)
                ]
    in
        div
            [ class "tab-list"
            , draggable "false"
            ]
            (model.tabDrag
                |> Maybe.andThen
                    (\{ start, current, tabIndex } ->
                        if start.x == current.x then
                            Nothing
                        else
                            Just <| viewDraggableTabsWithTabPlaceholder (insertPos current) tabIndex
                    )
                |> Maybe.withDefault draggableTabs
            )


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
            , Html.Events.onMouseUp (SetActive tab)
            , onWithOptions "contextmenu" defaultPrevented <| Json.map (ToggleTabMenu index) Mouse.position
            , on "mousedown" <| Json.map (TabDragStart index isPinned) Mouse.position
            ]
            [ text tab ]


viewTabMenu : TabMenu -> Html Msg
viewTabMenu { tabIndex, position } =
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
                , onWithOptions "contextmenu" defaultPrevented <| Json.map (ToggleTabMenu tabIndex) Mouse.position
                , class "tab-context-menu"
                , style
                    [ ( "top", px position.y )
                    , ( "left", px position.x )
                    ]
                ]
                [ ul [ class "tab-context-menu__list" ]
                    (List.map viewTabMenuItem (tabMenuItems tabIndex))
                ]
            ]


viewTabMenuItem : TabMenuItem -> Html Msg
viewTabMenuItem menuItem =
    let
        menuItemBehaviors =
            case menuItem of
                PinTab tabIndex ->
                    [ onMouseDown (PinTabAtIndex tabIndex) ]

                _ ->
                    []
    in
        li
            ([ class "tab-context-menu__item" ]
                ++ menuItemBehaviors
            )
            [ text <| tabMenuItemToString menuItem ]


viewDraggingTab : Animation.Messenger.State Msg -> TabDrag -> String -> Html Msg
viewDraggingTab draggingTabStyle { current, isSliding, isPinned } tab =
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
                , ( "dragging-tab", True )
                , ( "tab--pinned", isPinned )
                ]
             , draggable "false"
             , Html.Events.onMouseUp (SetActive tab)
             , style
                [ ( "top", px 0 )
                , ( "left", left )
                ]
             ]
                ++ Animation.render draggingTabStyle
            )
            [ text tab ]



-- HELPERS


getTabByIndex tabs index =
    List.drop index tabs
        |> List.head


allTabs model =
    model.pinnedTabs ++ model.tabs


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



-- CONSTANTS


tabWidth =
    100


pinnedTabWidth =
    50


halfTab =
    tabWidth // 2


allTabsWidth pinnedTabs tabs =
    (pinnedTabWidth * List.length pinnedTabs) + (tabWidth * List.length tabs)


calcTabWidth isPinned =
    if isPinned then
        pinnedTabWidth
    else
        tabWidth


rightMostMouse isPinned pinnedTabs tabs =
    if isPinned then
        allTabsWidth pinnedTabs tabs - (pinnedTabWidth // 2)
    else
        (allTabsWidth pinnedTabs tabs) - halfTab
