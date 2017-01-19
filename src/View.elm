module View exposing (..)

import Animation exposing (px)
import Array.Hamt as Array exposing (Array)
import DOM exposing (boundingClientRect, target)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, contextmenu, draggable, id, style)
import Html.Events exposing (Options, on, onClick, onMouseDown, onMouseEnter, onMouseUp, onWithOptions, onMouseEnter)
import Json.Decode as Decode exposing (Decoder, at, field, float, int, string)
import Messages exposing (..)
import Model exposing (Model)
import Mouse
import Reorderable.State exposing (State, ViewableReorderable(..))
import Svg exposing (Svg)
import Types exposing (OverflowArea(..), PinPlaceholder(..), PinningPlaceholder, ReorderItem(..), SlidingPlaceholder, TabClickInfo, TabMenu, TabMenuItem(..), UnPinningPlaceholder, Logo(..), Tab, tabMenuItemToString, tabMenuItems, toLogo)
import Svgs exposing (viewClose, viewDownCaret, viewElmLogo)
import Util exposing (defaultPrevented, toPx)


view : Model -> Html Msg
view model =
    let
        ( placeholder, maybeDestIndex ) =
            case model.dragState.placeholder of
                Just { point, draggable, sourceIndex, destIndex } ->
                    ( viewTabReorderItem (PlaceholderReorderable draggable point) model destIndex sourceIndex
                    , destIndex
                    )

                Nothing ->
                    ( text ""
                    , Nothing
                    )
    in
        div
            [ class "tabs-app" ]
            [ viewTooltips model
            , div
                [ class "tabs-container" ]
                [ viewTabsAndOverflow model maybeDestIndex
                , placeholder
                , model.pinPlaceholder
                    |> Maybe.map (viewPinPlaceholder model)
                    |> Maybe.withDefault (text "")
                ]
            , viewTooltipMask model.showingAnyMenu
            ]


viewTabs : Model -> Maybe Int -> Array Tab -> List (Html Msg)
viewTabs model maybeDestIndex tabs =
    tabs
        |> Array.indexedMap (viewNonPlaceholderTab model maybeDestIndex)
        |> Array.toList


viewTabsAndOverflow : Model -> Maybe Int -> Html Msg
viewTabsAndOverflow model maybeDestIndex =
    let
        tabs =
            model.dragState.reorderedItems

        withOverflow =
            viewTabsWithOverflow model maybeDestIndex tabs

        withoutOverflow =
            viewTabsWithoutOverflow model maybeDestIndex tabs
    in
        model.overflowArea
            |> Maybe.map withOverflow
            |> Maybe.withDefault withoutOverflow


viewTabsWithoutOverflow : Model -> Maybe Int -> Array Tab -> Html Msg
viewTabsWithoutOverflow model maybeDestIndex tabs =
    div [ class "tab-list" ] (viewTabs model maybeDestIndex tabs)


viewTabsWithOverflow : Model -> Maybe Int -> Array Tab -> OverflowArea -> Html Msg
viewTabsWithOverflow model maybeDestIndex tabs overflowArea =
    let
        numOverflow =
            2

        numNotOverflow =
            Array.length tabs - numOverflow
    in
        div
            [ class "tab-list"
            ]
            (tabs
                |> viewTabs model maybeDestIndex
                |> List.take numNotOverflow
                |> flip List.append
                    ([ tabs
                        |> Array.slice numNotOverflow (Array.length tabs)
                        |> viewOverflowArea model maybeDestIndex numNotOverflow overflowArea
                     ]
                    )
            )


viewOverflowArea : Model -> Maybe Int -> Int -> OverflowArea -> Array Tab -> Html Msg
viewOverflowArea model maybeDestIndex numNotOverflow overflowMenu tabs =
    div
        [ class "overflow-container" ]
        (case overflowMenu of
            MoreTab ->
                [ viewMoreTab overflowMenu ]

            Expanded ->
                [ viewMoreTab overflowMenu
                , tabs
                    |> Array.indexedMap (\index tab -> viewOverflowTab (NonPlaceholderReorderable tab) model maybeDestIndex (index + numNotOverflow) tab)
                    |> Array.toList
                    |> div [ class "overflow-menu" ]
                ]
        )


viewOverflowTab : ViewableReorderable Tab -> Model -> Maybe Int -> Int -> Tab -> Html Msg
viewOverflowTab reorderable model maybeDestIndex index tab =
    let
        ( tab, attrs, isSelected, reorderItem ) =
            case reorderable of
                NonPlaceholderReorderable draggable ->
                    let
                        reorderItem =
                            if maybeDestIndex == Just index then
                                DropPreview
                            else
                                ReorderableTab

                        styles =
                            List.concat
                                [ case reorderItem of
                                    DropPreview ->
                                        [ ( "visibility", "hidden" ) ]

                                    _ ->
                                        []
                                , if draggable.isPinned then
                                    [ ( "width", toPx model.pinnedTabWidth ) ]
                                  else
                                    [ ( "width", toPx model.flexTabWidth ) ]
                                ]
                    in
                        ( draggable
                        , [ class "draggable"
                          , classList
                                [ ( "overflow-tab", not draggable.isPinned )
                                , ( "tab--selected", model.selected.id == draggable.id )
                                , ( "tab--pinned", draggable.isPinned )
                                , ( "tab-id-" ++ toString draggable.id, True )
                                ]
                          , onClick CloseAllMenus
                          , style styles
                          , attribute "data-reorderable-index" (toString index)
                          , contextmenu "tab-menu"
                          , Decode.map2 TabClickInfo Mouse.position (field "currentTarget" boundingClientRect)
                                |> Decode.map (ToggleTabMenu index)
                                |> onWithOptions "contextmenu" defaultPrevented
                          , onMouseDown (SetActive draggable)
                          ]
                        , model.selected.id == draggable.id
                        , reorderItem
                        )

                PlaceholderReorderable draggable point ->
                    let
                        styles =
                            [ ( "position", "fixed" )
                            , ( "left", toPx point.x )
                            , ( "top", toPx point.y )
                            , ( "margin", "0" )
                            ]
                                ++ if draggable.isPinned then
                                    [ ( "width", toPx model.pinnedTabWidth )
                                    , ( "box-sizing", "border-box" )
                                    ]
                                   else
                                    [ ( "width", toPx model.flexTabWidth ) ]
                    in
                        ( draggable
                        , ([ class "draggable"
                           , style styles
                           , classList
                                [ ( "tab", not draggable.isPinned )
                                , ( "tab--selected", model.selected.id == draggable.id )
                                , ( "tab--pinned", draggable.isPinned )
                                ]
                           , id "reorderable-placeholder"
                           ]
                            ++ Animation.render model.placeholderAnimationStyle
                          )
                        , model.selected.id == draggable.id
                        , ReorderableTab
                        )

        viewTab =
            div attrs (viewPlaceholderDetails tab.isPinned index tab)
    in
        case reorderItem of
            ReorderableTab ->
                viewTab

            DropPreview ->
                div
                    [ class "drop-preview"
                    , style
                        [ ( "width"
                          , if tab.isPinned then
                                toPx model.pinnedTabWidth
                            else
                                toPx model.flexTabWidth
                          )
                        ]
                    ]
                    [ viewTab ]

            _ ->
                text ""


viewMoreTab : OverflowArea -> Html Msg
viewMoreTab overflowArea =
    button
        [ class "more-tab"
        , onClick (ToggleOverflowMenu overflowArea)
        ]
        [ div [ class "more-tab-caret" ] [ viewDownCaret ]
        , text "More"
        ]


viewNonPlaceholderTab : Model -> Maybe Int -> Int -> Tab -> Html Msg
viewNonPlaceholderTab model maybeDestIndex index tab =
    viewTabReorderItem (NonPlaceholderReorderable tab) model maybeDestIndex index


viewTooltips : Model -> Html Msg
viewTooltips model =
    model.tabMenu
        |> Maybe.map viewTabMenu
        |> Maybe.withDefault (text "")


viewTooltipMask : Bool -> Html Msg
viewTooltipMask showingAnyMenu =
    div
        [ classList [ ( "root-app", True ), ( "hidden", not showingAnyMenu ) ]
        , onClick CloseAllMenus
        ]
        []


{-|
Displays a `ReorderItem`.
This includes drop zones, pin backdrops, and viewable reorderables.
-}
viewTabReorderItem : ViewableReorderable Tab -> Model -> Maybe Int -> Int -> Html Msg
viewTabReorderItem viewableReorderable model maybeDestIndex index =
    let
        ( tab, attrs, isSelected, reorderItem ) =
            case viewableReorderable of
                NonPlaceholderReorderable draggable ->
                    let
                        reorderItem =
                            case model.pinPlaceholder of
                                Just pinPlaceholder ->
                                    case pinPlaceholder of
                                        Pinning { oldTabIndex, newTabIndex } ->
                                            if oldTabIndex == index then
                                                PinSourceBackdrop
                                            else if newTabIndex == index then
                                                PinDestBackdrop
                                            else
                                                ReorderableTab

                                        UnPinning { oldTabIndex, newTabIndex } ->
                                            if oldTabIndex == index then
                                                UnPinSourceBackdrop
                                            else if newTabIndex == index then
                                                PinDestBackdrop
                                            else
                                                ReorderableTab

                                Nothing ->
                                    if maybeDestIndex == Just index then
                                        DropPreview
                                    else
                                        ReorderableTab

                        styles =
                            List.concat
                                [ case reorderItem of
                                    DropPreview ->
                                        [ ( "visibility", "hidden" ) ]

                                    _ ->
                                        []
                                , if draggable.isPinned then
                                    [ ( "width", toPx model.pinnedTabWidth ) ]
                                  else
                                    []
                                ]
                    in
                        ( draggable
                        , [ class "draggable"
                          , classList
                                [ ( "tab", not draggable.isPinned )
                                , ( "tab--selected", model.selected.id == draggable.id )
                                , ( "tab--pinned", draggable.isPinned )
                                , ( "tab-id-" ++ toString draggable.id, True )
                                ]
                          , onClick CloseAllMenus
                          , style styles
                          , attribute "data-reorderable-index" (toString index)
                          , contextmenu "tab-menu"
                          , Decode.map2 TabClickInfo Mouse.position (field "currentTarget" boundingClientRect)
                                |> Decode.map (ToggleTabMenu index)
                                |> onWithOptions "contextmenu" defaultPrevented
                          , onMouseDown (SetActive draggable)
                          ]
                        , model.selected.id == draggable.id
                        , reorderItem
                        )

                PlaceholderReorderable draggable point ->
                    let
                        styles =
                            [ ( "position", "fixed" )
                            , ( "left", toPx point.x )
                            , ( "top", toPx point.y )
                            , ( "margin", "0" )
                            ]
                                ++ if draggable.isPinned then
                                    [ ( "width", toPx model.pinnedTabWidth )
                                    , ( "box-sizing", "border-box" )
                                    ]
                                   else
                                    [ ( "width", toPx model.flexTabWidth ) ]
                    in
                        ( draggable
                        , ([ class "draggable"
                           , style styles
                           , classList
                                [ ( "tab", not draggable.isPinned )
                                , ( "tab--selected", model.selected.id == draggable.id )
                                , ( "tab--pinned", draggable.isPinned )
                                ]
                           , id "reorderable-placeholder"
                           ]
                            ++ Animation.render model.placeholderAnimationStyle
                          )
                        , model.selected.id == draggable.id
                        , ReorderableTab
                        )

        viewTab =
            div attrs (viewPlaceholderDetails tab.isPinned index tab)
    in
        case reorderItem of
            ReorderableTab ->
                viewTab

            DropPreview ->
                div
                    [ class "drop-preview"
                    , style
                        [ ( "width"
                          , if tab.isPinned then
                                toPx model.pinnedTabWidth
                            else
                                toPx model.flexTabWidth
                          )
                        ]
                    ]
                    [ viewTab ]

            PinSourceBackdrop ->
                div
                    ([ class "pin-drop-preview"
                     , style
                        [ ( "width", toPx <| model.flexTabWidth )
                        ]
                     ]
                        ++ Animation.render model.pinStartBackdropStyle
                    )
                    []

            UnPinSourceBackdrop ->
                div
                    ([ class "pin-drop-preview"
                     , style
                        [ ( "width", toPx <| model.pinnedTabWidth )
                        ]
                     ]
                        ++ Animation.render model.pinStartBackdropStyle
                    )
                    []

            PinDestBackdrop ->
                div [ style [ ( "display", "flex" ) ] ]
                    [ div
                        ([ class "pin-drop-preview"
                         , style
                            [ ( "width", toPx 0 )
                            ]
                         ]
                            ++ Animation.render model.pinDestinationStyle
                        )
                        []
                    , viewTab
                    ]


viewPinPlaceholder : Model -> PinPlaceholder -> Html Msg
viewPinPlaceholder model pinPlaceholder =
    case pinPlaceholder of
        Pinning placeholder ->
            viewPinningPlaceholder model placeholder

        UnPinning placeholder ->
            viewUnPinningPlaceholder model placeholder


viewPlaceholderDetails : Bool -> Int -> Tab -> List (Html Msg)
viewPlaceholderDetails isPinned index tab =
    [ div [ class "tab-info" ]
        [ div [ class "tab-logo" ] [ tabToLogo tab ]
        , if isPinned then
            text ""
          else
            div [ class "tab-titles" ]
                [ h4 [ class "tab-title" ] [ text tab.title ]
                , p [ class "tab-subtitle" ] [ text "so good" ]
                ]
        ]
    , viewCloseButton index
    ]


tabToLogo : Tab -> Svg msg
tabToLogo tab =
    case toLogo tab.icon of
        Elm ->
            viewElmLogo


viewCloseButton : Int -> Html Msg
viewCloseButton index =
    button
        [ class "tab-close"
        , onClick <| CloseTabAtIndex index
        ]
        [ span
            [ class "accessible-hidden-text" ]
            [ text "Close" ]
        , viewClose
        ]


viewTabMenu : TabMenu -> Html Msg
viewTabMenu { tabIndex, position, tabRect, tab } =
    let
        px int =
            (toString int) ++ "px"

        menuItems =
            List.map (viewTabMenuItem tabRect tab position) (tabMenuItems tab.isPinned tabIndex)

        menuItemsWithDividers =
            List.concat
                [ List.take 1 menuItems
                , [ div [ class "tab-context-menu__divider" ] [] ]
                , List.drop 1 menuItems
                ]
    in
        div
            [ id "tab-menu"
            , Decode.map2 TabClickInfo Mouse.position (field "current" boundingClientRect)
                |> Decode.map (ToggleTabMenu tabIndex)
                |> onWithOptions "contextmenu" defaultPrevented
            , class "tab-context-menu"
            , style
                [ ( "top", px position.y )
                , ( "left", px position.x )
                ]
            ]
            [ ul [ class "tab-context-menu__list" ]
                menuItemsWithDividers
            ]


viewTabMenuItem : DOM.Rectangle -> Tab -> Mouse.Position -> TabMenuItem -> Html Msg
viewTabMenuItem tabRect tab pos menuItem =
    let
        menuItemBehaviors =
            case menuItem of
                PinTab tabIndex ->
                    [ onMouseDown (PinTabAtIndex tabIndex tab tabRect) ]

                UnpinTab tabIndex ->
                    [ onMouseDown (UnpinTabAtIndex tabIndex tab tabRect) ]

                CloseTab tabIndex ->
                    [ onMouseDown (CloseTabAtIndex tabIndex) ]

                CloseOtherTabs tabIndex ->
                    [ onMouseDown (CloseTabsOtherThanIndex tabIndex) ]

                CloseTabsToTheRight tabIndex ->
                    [ onMouseDown (CloseTabsToTheRightOfIndex tabIndex) ]
    in
        li [ class "tab-context-menu__item" ]
            [ button ([ class "tab-context-menu__button" ] ++ menuItemBehaviors)
                [ text <| tabMenuItemToString menuItem ]
            ]


viewPinningPlaceholder : Model -> PinningPlaceholder -> Html Msg
viewPinningPlaceholder model { start, startWidth, tab } =
    div
        ([ style
            [ ( "top", toPx 0 )
            , ( "left", toPx start.x )
            , ( "width", toPx startWidth )
            ]
         , class "moving-tab"
         , draggable "false"
         ]
            ++ Animation.render model.pinPlaceholderStyle
        )
        [ div
            [ classList
                [ ( "tab", False )
                , ( "tab--selected", tab.id == model.selected.id )
                , ( "tab--pinned", True )
                ]
            ]
            (viewPlaceholderDetails True 0 tab)
        ]


viewUnPinningPlaceholder : Model -> UnPinningPlaceholder -> Html Msg
viewUnPinningPlaceholder model { start, tab } =
    div
        ([ style
            [ ( "top", toPx 0 )
            , ( "left", toPx start.x )
            , ( "width", toPx model.pinnedTabWidth )
            ]
         , class "moving-tab"
         , draggable "false"
         ]
            ++ Animation.render model.pinPlaceholderStyle
        )
        [ div
            [ classList
                [ ( "tab", False )
                , ( "tab--selected", tab.id == model.selected.id )
                , ( "tab--pinned", True )
                ]
            ]
            (viewPlaceholderDetails True 0 tab)
        ]
