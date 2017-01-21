port module Reorderable.Ports exposing (dragStart, dragStop, dragMove, scrollBy)

import Reorderable.State exposing (Point, Bounds, Size)


scrollBy : Float -> Float -> Cmd a
scrollBy =
    curry scrollByPort


port scrollByPort : ( Float, Float ) -> Cmd msg


port dragStart : ({ sourceIndex : Int, point : Point } -> msg) -> Sub msg


port dragStop :
    ({ placeholder : { point : Point, bounds : Bounds }
     , reorderableBounds : List Bounds
     }
     -> msg
    )
    -> Sub msg


port dragMove :
    ({ clientSize : Size
     , cursor : Point
     , placeholder :
        { point : Point
        , bounds : Bounds
        }
     , reorderableBounds : List Bounds
     }
     -> msg
    )
    -> Sub msg
