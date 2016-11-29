module Example exposing (..)

import DraggableTabs
import Html exposing (..)
import Html.Events exposing (onClick)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Tabs (DraggableTabs.subscriptions model.tabs)
        ]


type alias Model =
    { tabs : DraggableTabs.Model
    , window : { width : Float, height : Float }
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
    | ResizeWindow ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tabs tabsMsg ->
            let
                ( tabsState, tabsCmds ) =
                    DraggableTabs.update (tabsConfig model) tabsMsg model.tabs
            in
                ( { model | tabs = tabsState }, Cmd.map Tabs tabsCmds )

        ResizeWindow xy ->
            ( resizeWindow xy model, Cmd.none )


tabsConfig model =
    { containerWidth = model.window.width
    , maxTabWidth = 202
    , minTabWidth = 102
    , pinnedTabWidth = 52
    }


resizeWindow : ( Float, Float ) -> Model -> Model
resizeWindow ( x, y ) model =
    { model | window = { width = x, height = y } }


view : Model -> Html Msg
view model =
    div []
        [ Html.map Tabs (DraggableTabs.view (tabsConfig model) model.tabs)
        , button [ onClick <| ResizeWindow ( model.window.width * 0.95, model.window.height * 0.95 ) ]
            [ text "resize" ]
        ]
