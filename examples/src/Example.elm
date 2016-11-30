module Example exposing (..)

import DraggableTabs
import Html exposing (..)
import Html.Events exposing (onClick)
import Window


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Tabs (DraggableTabs.subscriptions model.tabs)
        , Window.resizes ResizeWindow
        ]


type alias Model =
    { tabs : DraggableTabs.Model
    , window : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    let
        ( tabs, tabsCmds ) =
            DraggableTabs.init
    in
        ( { tabs = tabs
          , window = { width = 1000, height = 300 }
          }
        , Cmd.map Tabs tabsCmds
        )


type Msg
    = Tabs DraggableTabs.Msg
    | ResizeWindow Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tabs tabsMsg ->
            let
                ( tabsState, tabsCmds ) =
                    DraggableTabs.update (tabsConfig model) tabsMsg model.tabs
            in
                ( { model | tabs = tabsState }, Cmd.map Tabs tabsCmds )

        ResizeWindow size ->
            ( resizeWindow size model, Cmd.none )


tabsConfig model =
    { containerWidth = toFloat model.window.width
    , maxTabWidth = 202
    , minTabWidth = 102
    , pinnedTabWidth = 52
    }


resizeWindow : Window.Size -> Model -> Model
resizeWindow size model =
    { model | window = size }


view : Model -> Html Msg
view model =
    div []
        [ Html.map Tabs (DraggableTabs.view (tabsConfig model) model.tabs)
        ]
