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
                [ Mouse.moves TabDragging
                , Mouse.ups TabDragEnd
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
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3" ]
      , selected = "Tab 1"
      , tabDrag = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetActive String
    | TabDragStart Int Mouse.Position
    | TabDragging Mouse.Position
    | TabDragEnd Mouse.Position


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

        TabDragging xy ->
            { model | tabDrag = Maybe.map (setCurrent xy) model.tabDrag }

        TabDragEnd xy ->
            { model | tabDrag = Nothing }


setCurrent : Mouse.Position -> TabDrag -> TabDrag
setCurrent xy tabDrag =
    { tabDrag | current = xy }



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
