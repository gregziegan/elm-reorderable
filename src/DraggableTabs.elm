module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, draggable, style)
import Html.Events exposing (onClick, on, onMouseDown)
import Json.Decode as Json
import Mouse
import Animation exposing (px)
import Color exposing (rgba, rgb)
import Time exposing (Time, millisecond, second)


subscriptions : Model -> Sub Msg
subscriptions model =
    List.concat
        [ [ Time.every (100 * millisecond) Tick
          , Animation.subscription Animate [ model.insertAreaStyle, model.draggingTabStyle ]
          ]
        , tabDragSubscriptions model
        ]
        |> Sub.batch


tabDragSubscriptions model =
    let
        getSubscriptions tabDrag =
            [ Mouse.moves (TabDragging tabDrag)
            , Mouse.ups (TabDragEnd tabDrag)
            ]
    in
        (Maybe.map getSubscriptions model.tabDrag
            |> Maybe.withDefault []
        )



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
    , insertAreaStyle : Animation.State
    , draggingTabStyle : Animation.State
    , currentTime : Time
    , lastTabClick : Time
    }


init : ( Model, Cmd Msg )
init =
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6" ]
      , selected = "Tab 1"
      , tabDrag = Nothing
      , insertAreaStyle = initInsertAreaStyle
      , draggingTabStyle = initDraggingTabStyle
      , currentTime = 0
      , lastTabClick = 0
      }
    , Cmd.none
    )


initInsertAreaStyle =
    Animation.style [ Animation.width (px 0.0) ]


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
    | TabClick
    | TabDragStart Int Mouse.Position
    | TabDragging TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position
    | Animate Animation.Msg
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case msg of
        SetActive tabId ->
            { model | selected = tabId }

        TabClick ->
            let
                tabClick =
                    Debug.log "tabClicked!" "tabClicked"
            in
                { model | lastTabClick = model.currentTime }

        TabDragStart tabIndex xy ->
            model
                |> startTabDrag tabIndex xy
                |> growInsertArea
                |> emphasizeDraggingTab

        TabDragging tabDrag xy ->
            { model | tabDrag = Just <| setCurrent xy tabDrag }

        TabDragEnd tabDrag xy ->
            model
                |> dropTab tabDrag xy
                |> resetInsertAreaAnimation
                |> resetDraggingTabAnimation

        Animate animMsg ->
            let
                insertAreaStyle =
                    Animation.update animMsg model.insertAreaStyle

                draggingTabStyle =
                    Animation.update animMsg model.draggingTabStyle
            in
                { model
                    | insertAreaStyle = insertAreaStyle
                    , draggingTabStyle = draggingTabStyle
                }

        Tick time ->
            { model | currentTime = time }


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }


startTabDrag tabIndex xy model =
    { model | tabDrag = Just <| TabDrag xy xy tabIndex }


growInsertArea model =
    let
        newInsertAreaStyle =
            Animation.interrupt
                [ Animation.to
                    [ Animation.width (px (tabWidth))
                    ]
                ]
                model.insertAreaStyle
    in
        { model | insertAreaStyle = newInsertAreaStyle }


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


resetInsertAreaAnimation model =
    { model | insertAreaStyle = initInsertAreaStyle }


resetDraggingTabAnimation model =
    { model | draggingTabStyle = initDraggingTabStyle }


trackTabClickTime model =
    { model | lastTabClick = model.currentTime }



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
        dx =
            end.x - start.x

        offsetFromPrevTab =
            start.x % tabWidth

        distToNextTab =
            tabWidth - offsetFromPrevTab

        movingLeft =
            dx > 0

        movingRight =
            dx < 0

        crossedToPrevTab =
            dx > offsetFromPrevTab

        crossedToNextTab =
            abs dx > distToNextTab

        newIndex =
            end.x // tabWidth

        newTabs =
            if
                (movingLeft && crossedToPrevTab)
                    || (movingRight && crossedToNextTab)
            then
                shiftTabs newIndex tabIndex tabs
            else
                tabs
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
            `Maybe.andThen` (\{ tabIndex, current } ->
                                getTabByIndex model.tabs tabIndex
                                    |> Maybe.map (viewDraggingTab model.draggingTabStyle current)
                            )
            |> Maybe.withDefault (text "")
        , p [] [ text <| toString model.currentTime ]
        , p [] [ text <| toString model.lastTabClick ]
        ]


viewInsertArea model =
    div
        (Animation.render model.insertAreaStyle
            ++ [ class "insert-area"
               ]
        )
        []


viewTabs : Model -> Html Msg
viewTabs model =
    let
        draggableTabs =
            List.indexedMap (viewTab model) model.tabs

        viewDraggableTabsWithInsertArea insertIndex =
            List.concat
                [ List.take insertIndex draggableTabs
                , [ viewInsertArea model ]
                , List.drop insertIndex draggableTabs
                ]
    in
        div
            [ class "tab-list"
            , draggable "false"
            ]
            (model.tabDrag
                |> Maybe.map (\{ current } -> viewDraggableTabsWithInsertArea (current.x // tabWidth))
                |> Maybe.withDefault draggableTabs
            )


viewTab : Model -> Int -> String -> Html Msg
viewTab model index tab =
    let
        tabHasBeenClickedAtLeastOnce =
            model.lastTabClick > 0

        eitherTabDragOrClick xy =
            let
                hasItBeenEnoughTime =
                    abs (model.lastTabClick - model.currentTime)
                        > (0.1 * Time.second)
                        |> Debug.log "hasItBeenEnoughTime"
            in
                if tabHasBeenClickedAtLeastOnce && hasItBeenEnoughTime then
                    Ok (TabDragStart index xy)
                else
                    Ok TabClick

        dec =
            Json.customDecoder Mouse.position eitherTabDragOrClick
    in
        div
            [ classList
                [ ( "tab", True )
                , ( "tab-selected", model.selected == tab )
                ]
            , onClick (SetActive tab)
            , on "mousedown" dec
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
                   , style
                        [ ( "top", px (current.y - (tabHeight // 2)) )
                        , ( "left", px (current.x - (tabWidth // 2)) )
                        ]
                   ]
            )
            [ text tab ]



-- HELPERS


getTabByIndex tabs index =
    List.drop index tabs
        |> List.head
