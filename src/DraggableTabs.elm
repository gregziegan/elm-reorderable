module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { tabs : List String
    , selected : String
    }


init : ( Model, Cmd Msg )
init =
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3" ]
      , selected = "Tab 1"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = SetActive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetActive tabId ->
            ( { model | selected = tabId }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "tab-list" ]
        (List.map (viewTab model) model.tabs)


viewTab : Model -> String -> Html Msg
viewTab model tab =
    div
        [ classList
            [ ( "tab", True )
            , ( "tab-selected", model.selected == tab )
            ]
        , onClick (SetActive tab)
        ]
        [ text tab ]
