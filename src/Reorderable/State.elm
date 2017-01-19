module Reorderable.State exposing (Point, Config, State, Placeholder, ViewableReorderable(..), Bounds, Size)

import Array.Hamt as Array exposing (Array)


type alias Point =
    { x : Float, y : Float }


type alias Config =
    { animate : Bool }


type alias State item =
    { placeholder : Maybe (Placeholder item)
    , items : Array item
    , reorderedItems : Array item
    , animating : Bool
    }


type ViewableReorderable item
    = PlaceholderReorderable item Point
    | NonPlaceholderReorderable item


type alias Placeholder item =
    { point : Point
    , draggable : item
    , sourceIndex : Int
    , destIndex : Maybe Int
    }


type alias Bounds =
    { top : Float, left : Float, bottom : Float, right : Float }


type alias Size =
    { height : Float, width : Float }
