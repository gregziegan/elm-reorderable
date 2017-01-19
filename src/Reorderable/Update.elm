module Reorderable.Update exposing (update, DragMsg(..), getMostOverlappingBounds, subscriptions, dropAndShift)

import Array.Hamt as Array exposing (Array)
import Reorderable.State exposing (Point, Config, State, Bounds, Size, Placeholder)
import Reorderable.Ports as Ports


{-| When you're dragging within this percentage of the top or bottom portions
of the viewport, start scrolling in that direction. We do this as a percentage
rather than as fixed pixels because otherwise when you're really zoomed in,
you are constantly autoscrolling.
-}
autoscrollMaxPercentage : Float
autoscrollMaxPercentage =
    0.2


{-| When you drag something to the top or bottom of the screen, scroll by
this much.
-}
autoscrollAmount : Float
autoscrollAmount =
    10


type DragMsg
    = DragStart { sourceIndex : Int, point : Point }
    | DragHold
        { placeholder : { point : Point, bounds : Bounds }
        , reorderableBounds : List Bounds
        }
    | DragStop
    | DragMove
        { clientSize : Size
        , cursor : Point
        , placeholder :
            { point : Point
            , bounds : Bounds
            }
        , reorderableBounds : List Bounds
        }


update : DragMsg -> State item -> Result String ( State item, Cmd DragMsg )
update msg dragInfo =
    case msg of
        DragStart { sourceIndex, point } ->
            case Array.get sourceIndex dragInfo.items of
                Just draggable ->
                    ( { dragInfo
                        | placeholder =
                            Just
                                { point = point
                                , draggable = draggable
                                , sourceIndex = sourceIndex
                                , destIndex = Just sourceIndex
                                }
                        , reorderedItems = dragInfo.items
                      }
                    , Cmd.none
                    )
                        |> Ok

                Nothing ->
                    Err
                        ("Encountered a sourceIndex "
                            ++ toString sourceIndex
                            ++ " - which was not in dragInfo.items. This should never happen! dragInfo.items was: "
                            ++ toString dragInfo.items
                        )

        DragMove { clientSize, cursor, placeholder, reorderableBounds } ->
            let
                maxTopToScroll =
                    clientSize.height * autoscrollMaxPercentage

                minBottomToScroll =
                    clientSize.height * (1 - autoscrollMaxPercentage)

                commands =
                    if cursor.y > minBottomToScroll then
                        [ Ports.scrollBy 0 autoscrollAmount ]
                    else if cursor.y < maxTopToScroll then
                        [ Ports.scrollBy 0 -autoscrollAmount ]
                    else
                        []
            in
                if dragInfo.animating then
                    ( dragInfo, Cmd.none )
                        |> Ok
                else
                    -- Sometimes JS can send through a DragMove when we're not dragging.
                    -- If that happens, discard it.
                    case dragInfo.placeholder of
                        Nothing ->
                            ( dragInfo, Cmd.none )
                                |> Ok

                        Just { draggable, sourceIndex } ->
                            let
                                maybeReorderableIndex =
                                    getMostOverlappingBounds placeholder.bounds reorderableBounds

                                newPlaceholder =
                                    { point = placeholder.point
                                    , draggable = draggable
                                    , sourceIndex = sourceIndex
                                    , destIndex = Maybe.map Tuple.first maybeReorderableIndex
                                    }

                                reorderedItems =
                                    case maybeReorderableIndex of
                                        Just ( destIndex, freeze ) ->
                                            if freeze then
                                                dragInfo.reorderedItems
                                            else
                                                dropAndShift sourceIndex destIndex dragInfo.items

                                        Nothing ->
                                            dragInfo.items
                            in
                                ( { dragInfo
                                    | placeholder = Just newPlaceholder
                                    , reorderedItems = reorderedItems
                                  }
                                , Cmd.batch commands
                                )
                                    |> Ok

        DragStop ->
            case dragInfo.placeholder of
                Nothing ->
                    Err "Received a DragStop when dragInfo.placeholder was Nothing. This should never happen!"

                Just { sourceIndex, destIndex } ->
                    case destIndex of
                        Nothing ->
                            ( { dragInfo | placeholder = Nothing }, Cmd.none )
                                |> Ok

                        Just dest ->
                            let
                                newReorderables =
                                    dropAndShift sourceIndex dest dragInfo.items
                            in
                                ( { dragInfo
                                    | placeholder = Nothing
                                    , items = newReorderables
                                    , reorderedItems = newReorderables
                                    , animating = False
                                  }
                                , Cmd.none
                                )
                                    |> Ok

        DragHold moveData ->
            ( { dragInfo | animating = True }, Cmd.none )
                |> Ok


{-| Based on http://math.stackexchange.com/a/99576
-}
getOverlapArea : Bounds -> Bounds -> ( Float, Bool )
getOverlapArea d0 d1 =
    let
        d0Width =
            d0.right - d0.left

        d1Width =
            d1.right - d1.left

        xOverlap =
            (min d0.right d1.right - max d0.left d1.left)

        yOverlap =
            min d0.bottom d1.bottom - max d0.top d1.top

        halfWay =
            ((d1Width - d0Width) * yOverlap)
                |> max 0

        overlap =
            (xOverlap * yOverlap)
                |> max 0

        overlapPlusLeft =
            overlap
                + ((max 0 (d0.left - d1.left)) * yOverlap)
                |> max 0
    in
        -- For dragging placeholders that are smaller than other items, determine destination by distance
        if d0Width < d1Width then
            if overlapPlusLeft >= halfWay && d0.right < d1.right then
                ( d1Width * yOverlap, False )
            else if overlapPlusLeft < halfWay && d0.right < d1.right && d0.left > d1.left then
                ( 1, True )
            else
                ( 0, False )
        else
            ( overlap, False )


getMostOverlappingBounds : Bounds -> List Bounds -> Maybe ( Int, Bool )
getMostOverlappingBounds =
    getMostOverlappingBoundsHelp 0 0 Nothing


getMostOverlappingBoundsHelp :
    Int
    -> Float
    -> Maybe ( Int, Bool )
    -> Bounds
    -> List Bounds
    -> Maybe ( Int, Bool )
getMostOverlappingBoundsHelp draggableIndex area winner elem candidates =
    case candidates of
        [] ->
            winner

        candidateBounds :: rest ->
            let
                ( candidateOverlapArea, freeze ) =
                    getOverlapArea elem candidateBounds
            in
                if candidateOverlapArea > area then
                    getMostOverlappingBoundsHelp
                        (draggableIndex + 1)
                        candidateOverlapArea
                        (Just ( draggableIndex, freeze ))
                        elem
                        rest
                else
                    getMostOverlappingBoundsHelp
                        (draggableIndex + 1)
                        area
                        winner
                        elem
                        rest


subscriptions : Config -> State item -> Sub DragMsg
subscriptions config state =
    if state.placeholder == Nothing then
        Ports.dragStart DragStart
    else
        Sub.batch
            [ Ports.dragMove DragMove
            , Ports.dragStop
                (\moveData ->
                    if config.animate then
                        DragHold moveData
                    else
                        DragStop
                )
            ]


{-|
  Shift right
             |
  i = [1, 2, 3, 4, 5, 6]
                   |
  o = [1, 2, 4, 5, 3, 6]
  [1, 2] ++ [4, 5] ++ [3] ++ [6]
  Shift left
                   |
  i = [1, 2, 3, 4, 5, 6]
             |
  o = [1, 2, 5, 3, 4, 6]
  [1, 2] ++ [5] ++ [3, 4] ++ [6]
-}
dropAndShift : Int -> Int -> Array a -> Array a
dropAndShift sourceIndex destIndex items =
    let
        itemsUntilFirst first =
            Array.slice 0 first items

        itemsBetweenShiftRight first second =
            items
                |> Array.slice (first + 1) (second + 1)

        itemsBetweenShiftLeft first second =
            items
                |> Array.slice first second

        restOfTabs second =
            Array.slice (second + 1) (Array.length items) items

        shiftLeft first second selected =
            itemsUntilFirst first
                |> Array.push selected
                |> flip Array.append (itemsBetweenShiftLeft first second)
                |> flip Array.append (restOfTabs second)

        shiftRight first second selected =
            itemsUntilFirst first
                |> flip Array.append (itemsBetweenShiftRight first second)
                |> Array.push selected
                |> flip Array.append (restOfTabs second)

        shift tab =
            if sourceIndex < destIndex then
                shiftRight sourceIndex destIndex tab
            else
                shiftLeft destIndex sourceIndex tab
    in
        Array.get sourceIndex items
            |> Maybe.map shift
            |> Maybe.withDefault items
