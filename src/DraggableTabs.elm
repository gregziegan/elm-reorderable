module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, draggable, style)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import Mouse
import Animation exposing (px)
import Color exposing (rgba, rgb)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tabDrag of
        Nothing ->
            Sub.none

        Just tabDrag ->
            Sub.batch
                [ Mouse.moves (TabDragging tabDrag)
                , Mouse.ups (TabDragEnd tabDrag)
                , Animation.subscription Animate [ model.tabPlaceholderStyle, model.draggingTabStyle ]
                ]



-- MODEL


type alias TabDrag =
    { start : Mouse.Position
    , current : Mouse.Position
    , tabIndex : Int
    }


type alias Model =
    { tabs : List String
    , selected : String
    , tabDrag : Maybe TabDrag
    , tabPlaceholderStyle : Animation.State
    , draggingTabStyle : Animation.State
    }


init : ( Model, Cmd Msg )
init =
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6" ]
      , selected = "Tab 1"
      , tabDrag = Nothing
      , tabPlaceholderStyle = initTabPlaceholderStyle
      , draggingTabStyle = initDraggingTabStyle
      }
    , Cmd.none
    )


initTabPlaceholderStyle =
    Animation.style [ Animation.backgroundColor Color.gray ]


initDraggingTabStyle =
    Animation.style
        [ Animation.shadow
            { offsetX = 0
            , offsetY = 0
            , blur = 0
            , size = 0
            , color = rgba 0 0 0 0.1
            }
        ]



-- UPDATE


type Msg
    = SetActive String
    | TabDragStart Int Mouse.Position
    | TabDragging TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position
    | Animate Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case Debug.log "msg" msg of
        SetActive tabId ->
            { model | selected = tabId }

        TabDragStart tabIndex xy ->
            model
                |> startTabDrag tabIndex xy
                |> growTabPlaceholder
                |> emphasizeDraggingTab

        TabDragging tabDrag xy ->
            { model | tabDrag = Just <| setCurrent xy tabDrag }

        TabDragEnd tabDrag xy ->
            model
                |> dropTab tabDrag xy
                |> resetTabPlaceholderAnimation
                |> resetDraggingTabAnimation

        Animate animMsg ->
            let
                tabPlaceholderStyle =
                    Animation.update animMsg model.tabPlaceholderStyle

                draggingTabStyle =
                    Animation.update animMsg model.draggingTabStyle
            in
                { model
                    | tabPlaceholderStyle = tabPlaceholderStyle
                    , draggingTabStyle = draggingTabStyle
                }


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }


startTabDrag tabIndex xy model =
    { model | tabDrag = Just <| TabDrag xy xy tabIndex }


growTabPlaceholder : Model -> Model
growTabPlaceholder model =
    let
        newTabPlaceholderStyle =
            Animation.interrupt
                [ Animation.to
                    [ Animation.backgroundColor Color.purple
                    ]
                ]
                model.tabPlaceholderStyle
    in
        { model | tabPlaceholderStyle = newTabPlaceholderStyle }


emphasizeDraggingTab model =
    let
        newDraggingTabStyle =
            Animation.interrupt
                [ Animation.to
                    [ Animation.shadow
                        { offsetX = 10
                        , offsetY = 10
                        , blur = 15
                        , size = 0
                        , color = rgba 0 0 0 0.1
                        }
                    ]
                ]
                model.draggingTabStyle
    in
        { model | draggingTabStyle = newDraggingTabStyle }


resetTabPlaceholderAnimation model =
    { model | tabPlaceholderStyle = initTabPlaceholderStyle }


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
dropTab { start, tabIndex } end ({ tabs } as model) =
    let
        newIndex =
            end.x // tabWidth

        newTabs =
            shiftTabs newIndex tabIndex tabs
    in
        { model
            | tabDrag = Nothing
            , tabs = newTabs
        }



-- VIEW


tabWidth =
    100


tabHeight =
    50


view : Model -> Html Msg
view model =
    div []
        [ viewTabs model
        , model.tabDrag
            |> Maybe.andThen
                (\{ tabIndex, current } ->
                    getTabByIndex model.tabs tabIndex
                        |> Maybe.map (viewDraggingTab model.draggingTabStyle current)
                )
            |> Maybe.withDefault (text "")
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
            List.indexedMap (viewTab model) model.tabs

        reorderedTabsPreview insertIndex tabIndex =
            model.tabs
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
                |> Maybe.map (\{ current, tabIndex } -> viewDraggableTabsWithTabPlaceholder (current.x // tabWidth) tabIndex)
                |> Maybe.withDefault draggableTabs
            )


viewTab : Model -> Int -> String -> Html Msg
viewTab model index tab =
    div
        [ classList
            [ ( "tab", True )
            , ( "tab-selected", model.selected == tab )
            ]
        , onClick (SetActive tab)
        , on "mousedown" <| Json.map (TabDragStart index) Mouse.position
        ]
        [ text tab ]


viewDraggingTab : Animation.State -> Mouse.Position -> String -> Html Msg
viewDraggingTab draggingTabStyle current tab =
    let
        px int =
            (toString int) ++ "px"
    in
        div
            (Animation.render draggingTabStyle
                ++ [ class "tab dragging-tab"
                   , draggable "false"
                   , style
                        [ ( "top", px 0 )
                        , ( "left", px (current.x - (tabWidth // 2)) )
                        ]
                   ]
            )
            [ text tab ]



-- HELPERS


getTabByIndex tabs index =
    List.drop index tabs
        |> List.head
