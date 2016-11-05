module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, draggable, style)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import Mouse


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.tabDrag of
        Nothing ->
            Sub.none

        Just tabDrag ->
            Sub.batch
                [ Mouse.moves (TabDragging tabDrag)
                , Mouse.ups (TabDragEnd tabDrag)
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
    }


init : ( Model, Cmd Msg )
init =
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6" ]
      , selected = "Tab 1"
      , tabDrag = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetActive String
    | TabDragStart Int Mouse.Position
    | TabDragging TabDrag Mouse.Position
    | TabDragEnd TabDrag Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( pureUpdate msg model, Cmd.none )


pureUpdate : Msg -> Model -> Model
pureUpdate msg model =
    case Debug.log "msg" msg of
        SetActive tabId ->
            { model | selected = tabId }

        TabDragStart tabIndex xy ->
            { model | tabDrag = Just <| TabDrag xy xy tabIndex }

        TabDragging tabDrag xy ->
            { model | tabDrag = Just <| setCurrent xy tabDrag }

        TabDragEnd tabDrag xy ->
            model
                |> dropTab tabDrag xy


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }



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

        tabWidth =
            100

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


view : Model -> Html Msg
view model =
    div []
        [ viewTabs model
        , model.tabDrag
            `Maybe.andThen` (\{ tabIndex } -> getTabByIndex model.tabs tabIndex)
            |> Maybe.map (viewDraggingTab model.tabDrag)
            |> Maybe.withDefault (text "")
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    div
        [ class "tab-list"
        , draggable "false"
        ]
        (List.indexedMap (viewTab model) model.tabs)


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


px int =
    (toString int) ++ "px"


viewDraggingTab : Maybe TabDrag -> String -> Html Msg
viewDraggingTab maybeTabDrag tab =
    case maybeTabDrag of
        Just { current } ->
            div
                [ class "tab dragging-tab"
                , style
                    [ ( "top", px current.y )
                    , ( "left", px current.x )
                    ]
                ]
                [ text tab ]

        Nothing ->
            text ""



-- HELPERS


getTabByIndex tabs index =
    List.drop index tabs
        |> List.head
