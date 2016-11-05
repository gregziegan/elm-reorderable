module DraggableTabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type alias Model =
    { tabs : List String }


init : ( Model, Cmd Msg )
init =
    ( { tabs = [ "Tab 1", "Tab 2", "Tab 3" ] }, Cmd.none )



-- UPDATE


type Msg
    = SetActive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "tab-list" ]
        (List.map viewTab model.tabs)


viewTab : String -> Html Msg
viewTab tab =
    div [ class "tab" ]
        [ text tab ]
