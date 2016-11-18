module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, draggable, style)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import Mouse
import Animation exposing (px)
import Animation.Messenger
import Color exposing (rgba, rgb)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tabDrag of
        Nothing ->
            Sub.none

        Just tabDrag ->
            Sub.batch
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
    , isSliding : Bool
    }


type alias Model =
    { tabs : List String
    , selected : String
    , tabDrag : Maybe TabDrag
    , tabPlaceholderStyle : Animation.State
    , draggingTabStyle : Animation.Messenger.State Msg
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
    | TabDragEnding TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position
    | Animate Animation.Msg
    | AnimateMessenger Animation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive tabId ->
            ( { model | selected = tabId }, Cmd.none )

        TabDragStart tabIndex xy ->
            ( model
                |> startTabDrag tabIndex xy
                |> growTabPlaceholder
                |> emphasizeDraggingTab
            , Cmd.none
            )

        TabDragging tabDrag xy ->
            ( { model | tabDrag = Just <| setCurrent xy tabDrag }
            , Cmd.none
            )

        TabDragEnding tabDrag xy ->
            let
                newTabDrag =
                    Just <| startTabSlide <| setCurrent xy tabDrag
            in
                ( { model | tabDrag = newTabDrag }
                    |> resetTabPlaceholderAnimation
                    |> slideDraggingTabAnimation tabDrag
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



-- |> (a -> a -> b)


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }


startTabDrag tabIndex xy model =
    { model | tabDrag = Just <| TabDrag xy xy tabIndex False }


startTabSlide tabDrag =
    { tabDrag | isSliding = True }


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


slideDraggingTabAnimation ({ current } as tabDrag) model =
    let
        currentLeft =
            current.x - (tabWidth // 2)

        newTabOffset =
            (current.x // tabWidth) * tabWidth

        newDraggingTabStyle =
            Animation.interrupt
                [ Animation.set
                    [ Animation.left <| px <| toFloat <| currentLeft ]
                , Animation.to
                    [ Animation.left <| px <| toFloat <| newTabOffset ]
                , Animation.Messenger.send (TabDragEnd tabDrag { current | x = newTabOffset })
                ]
                model.draggingTabStyle
    in
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
                (\({ tabIndex, current } as tabDrag) ->
                    getTabByIndex model.tabs tabIndex
                        |> Maybe.map (viewDraggingTab model.draggingTabStyle tabDrag)
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


viewDraggingTab : Animation.Messenger.State Msg -> TabDrag -> String -> Html Msg
viewDraggingTab draggingTabStyle { current, isSliding } tab =
    let
        px int =
            (toString int) ++ "px"
    in
        div
            ([ class "tab dragging-tab"
             , draggable "false"
             , style
                [ ( "top", px 0 )
                , ( "left", px (current.x - (tabWidth // 2)) )
                ]
             ]
                ++ Animation.render draggingTabStyle
            )
            [ text tab ]



-- HELPERS


getTabByIndex tabs index =
    List.drop index tabs
        |> List.head
