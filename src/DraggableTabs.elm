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


type alias Model =
    { tabs : List String
    , pinnedTabs : List String
    , selected : String
    , tabDrag : Maybe TabDrag
    , tabPlaceholderStyle : Animation.State
    , draggingTabStyle : Animation.Messenger.State Msg
    , showTabMenu : Bool
    , keyboardModel : Keyboard.Extra.Model
    , activeTabMenuItem : TabMenuItem
    , tabMenuPos : Mouse.Position
    }


type TabMenuItem
    = Reload
    | Duplicate
    | PinTab
    | CloseTab


tabMenuItemToString menuItem =
    case menuItem of
        Reload ->
            "Reload"

        Duplicate ->
            "Duplicate"

        PinTab ->
            "Pin Tab"

        CloseTab ->
            "Close Tab"


tabMenuItems =
    [ Reload, Duplicate, PinTab, CloseTab ]


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { tabs = [ "Tab 3", "Tab 4", "Tab 5", "Tab 6" ]
          , pinnedTabs = [ "Tab 1", "Tab 2" ]
          , selected = "Tab 1"
          , tabDrag = Nothing
          , tabPlaceholderStyle = initTabPlaceholderStyle
          , draggingTabStyle = initDraggingTabStyle
          , showTabMenu = False
          , keyboardModel = keyboardModel
          , activeTabMenuItem = Reload
          , tabMenuPos = { x = 0, y = 0 }
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


initTabPlaceholderStyle =
    Animation.style []


initDraggingTabStyle =
    Animation.style []



-- UPDATE


type Msg
    = SetActive String
    | TabDragStart Int Bool Mouse.Position
    | TabDragging TabDrag Mouse.Position
    | TabDragEnding TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position
    | Animate Animation.Msg
    | AnimateMessenger Animation.Msg
    | ToggleTabMenu Mouse.Position
    | CloseTabMenu
    | SetActiveTabMenuItem TabMenuItem
    | KeyboardExtraMsg Keyboard.Extra.Msg


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

        ToggleTabMenu xy ->
            ( { model | showTabMenu = not model.showTabMenu, tabMenuPos = xy }, Cmd.none )

        CloseTabMenu ->
            ( { model | showTabMenu = False }, Cmd.none )

        SetActiveTabMenuItem menuItem ->
            ( { model | activeTabMenuItem = menuItem }, Cmd.none )

        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel

                escapeIsPressed =
                    Keyboard.Extra.isPressed Keyboard.Extra.Escape keyboardModel
            in
                ( { model
                    | keyboardModel = keyboardModel
                    , showTabMenu =
                        if model.showTabMenu && escapeIsPressed then
                            False
                        else
                            model.showTabMenu
                    , tabDrag =
                        if model.tabDrag /= Nothing && escapeIsPressed then
                            Nothing
                        else
                            model.tabDrag
                  }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )


normalizeTabDragMouse { pinnedTabs, tabs } xy tabDrag =
    let
        rightMostX =
            rightMostMouse pinnedTabs tabs

        boundedXy =
            if xy.x < halfTab then
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
        currentLeft =
            current.x - (tabWidth // 2)

        numPinned =
            List.length model.pinnedTabs

        insertPos =
            current.x // tabWidth

        newTabOffset =
            (newTabIndex isPinned insertPos numPinned) * tabWidth

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
        if current.x == 0 || current.x > rightMostMouse model.pinnedTabs tabs then
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
dropTab { start, tabIndex, isPinned } end model =
    let
        numPinned =
            List.length model.pinnedTabs

        insertPos =
            end.x // tabWidth

        newIndex =
            newTabIndex isPinned insertPos numPinned

        newTabs =
            Debug.log "shifted" <| shiftTabs newIndex tabIndex (allTabs model)
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
        , viewTabMenu model
        ]


viewTabPlaceholder : Model -> Html Msg
viewTabPlaceholder model =
    div
        (Animation.render model.tabPlaceholderStyle
            ++ [ class "tab-placeholder"
               ]
        )
        []


viewTabs : Model -> Html Msg
viewTabs model =
    let
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
                            Just <| viewDraggableTabsWithTabPlaceholder (current.x // tabWidth) tabIndex
                    )
                |> Maybe.withDefault draggableTabs
            )


defaultPrevented =
    Html.Events.Options False True


viewTab : Model -> Int -> String -> Html Msg
viewTab model index tab =
    let
        isPinned =
            index < List.length model.pinnedTabs
    in
        div
            [ classList
                [ ( "tab", True )
                , ( "tab--selected", model.selected == tab )
                , ( "tab--pinned", Debug.log "ispinne" isPinned )
                ]
            , contextmenu "tab-menu"
            , Html.Events.onMouseUp (SetActive tab)
            , onWithOptions "contextmenu" defaultPrevented <| Json.map ToggleTabMenu Mouse.position
            , on "mousedown" <| Json.map (TabDragStart index isPinned) Mouse.position
            ]
            [ text tab ]


viewTabMenu : Model -> Html Msg
viewTabMenu { activeTabMenuItem, tabMenuPos, showTabMenu } =
    let
        px int =
            (toString int) ++ "px"
    in
        div
            [ classList
                [ ( "tab-context-menu__backdrop", True )
                , ( "tab-context-menu__backdrop--active", showTabMenu )
                ]
            , onMouseDown CloseTabMenu
            , onWithOptions "contextmenu" defaultPrevented <| Json.map (\_ -> CloseTabMenu) (Json.succeed "contextClick")
            ]
            [ nav
                [ id "tab-menu"
                , onWithOptions "contextmenu" defaultPrevented <| Json.map ToggleTabMenu Mouse.position
                , classList
                    [ ( "tab-context-menu", True )
                    , ( "tab-context-menu--active", showTabMenu )
                    ]
                , style
                    [ ( "top", px tabMenuPos.y )
                    , ( "left", px tabMenuPos.x )
                    ]
                ]
                [ ul [ class "tab-context-menu__list" ]
                    (List.map (viewTabMenuItem activeTabMenuItem) tabMenuItems)
                ]
            ]


viewTabMenuItem : TabMenuItem -> TabMenuItem -> Html Msg
viewTabMenuItem activeMenuItem menuItem =
    li
        [ classList
            [ ( "tab-context-menu__item", True )
            , ( "tab-context-menu__item--active", activeMenuItem == menuItem )
            ]
        , onMouseEnter (SetActiveTabMenuItem menuItem)
        ]
        [ text <| tabMenuItemToString menuItem ]


viewDraggingTab : Animation.Messenger.State Msg -> TabDrag -> String -> Html Msg
viewDraggingTab draggingTabStyle { current, isSliding } tab =
    let
        px int =
            (toString int) ++ "px"

        left =
            if current.x < (tabWidth // 2) then
                px 0
            else
                px (current.x - (tabWidth // 2))
    in
        div
            ([ class "tab dragging-tab"
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
    else
        insertPos



-- CONSTANTS


tabWidth =
    100


pinnedTabWidth =
    50


halfTab =
    tabWidth // 2


allTabsWidth pinnedTabs tabs =
    (pinnedTabWidth * List.length pinnedTabs) + (tabWidth * List.length tabs)


rightMostMouse pinnedTabs tabs =
    if List.length tabs == 0 then
        (allTabsWidth pinnedTabs tabs) - halfTab
    else
        allTabsWidth pinnedTabs tabs - (pinnedTabWidth // 2)
