module Logos exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes exposing (..)


viewElmLogo : Html msg
viewElmLogo =
    Html.div []
        [ svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 323.141 322.95"
            , class "tab-logo-svg"
            ]
            [ polygon [ fill "#F0AD00", points "161.649,152.782 231.514,82.916 91.783,82.916" ] []
            , polygon [ fill "#7FD13B", points "8.867,0 79.241,70.375 232.213,70.375 161.838,0" ] []
            , rect
                [ fill "#7FD13B"
                , x "192.99"
                , y "107.392"
                , width "107.676"
                , height "108.167"
                , transform "matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)"
                ]
                []
            , polygon [ fill "#60B5CC", points "323.298,143.724 323.298,0 179.573,0" ] []
            , polygon [ fill "#5A6378", points "152.781,161.649 0,8.868 0,314.432" ] []
            , polygon [ fill "#F0AD00", points "255.522,246.655 323.298,314.432 323.298,178.879" ] []
            , polygon [ fill "#60B5CC", points "161.649,170.517 8.869,323.298 314.43,323.298" ] []
            ]
        ]


viewElixirLogo : Html msg
viewElixirLogo =
    Html.img
        [ Html.Attributes.class "tab-logo-svg"
        , Html.Attributes.src "assets/elixir-logo.png"
        ]
        []


viewHaskellLogo : Html msg
viewHaskellLogo =
    Html.div []
        [ svg
            [ class "tab-logo-svg"
            , viewBox "0 0 481.8897 340.1574"
            , version "1.1"
            ]
            [ defs
                []
                [ Svg.clipPath
                    [ id "clip1" ]
                    [ Svg.path
                        [ d "M 0 340.15625 L 481.890625 340.15625 L 481.890625 0 L 0 0 L 0 340.15625 Z M 0 340.15625 " ]
                        []
                    ]
                ]
            , g
                [ id "surface0" ]
                [ g
                    [ Svg.Attributes.clipPath "url(#clip1)", clipRule "nonzero" ]
                    [ Svg.path
                        [ Svg.Attributes.style " stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;", d "M 0 340.15625 L 113.386719 170.078125 L 0 0 L 85.039062 0 L 198.425781 170.078125 L 85.039062 340.15625 L 0 340.15625 Z M 0 340.15625 " ]
                        []
                    , Svg.path
                        [ Svg.Attributes.style " stroke:none;fill-rule: nonzero; fill: rgb(60%,60%,60%); fill-opacity: 1;", d "M 113.386719 340.15625 L 226.773438 170.078125 L 113.386719 0 L 198.425781 0 L 425.195312 340.15625 L 340.15625 340.15625 L 269.292969 233.859375 L 198.425781 340.15625 L 113.386719 340.15625 Z M 113.386719 340.15625 " ]
                        []
                    , Svg.path
                        [ Svg.Attributes.style " stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;", d "M 387.402344 240.945312 L 349.609375 184.253906 L 481.890625 184.25 L 481.890625 240.945312 L 387.402344 240.945312 Z M 387.402344 240.945312 " ]
                        []
                    , Svg.path
                        [ Svg.Attributes.style " stroke:none;fill-rule: nonzero; fill: rgb(40%,40%,40%); fill-opacity: 1;", d "M 330.710938 155.90625 L 292.914062 99.214844 L 481.890625 99.210938 L 481.890625 155.90625 L 330.710938 155.90625 Z M 330.710938 155.90625 " ]
                        []
                    ]
                ]
            ]
        ]


viewClose : Svg msg
viewClose =
    svg
        [ viewBox "0 0 7 7"
        , class "tab-close-svg"
        ]
        [ g
            [ stroke "currentColor", strokeWidth "1.5", strokeLinecap "round", strokeLinejoin "round" ]
            [ Svg.path
                [ d "M6,6 L1,1" ]
                []
            , Svg.path
                [ d "M1,6 L6,1" ]
                []
            ]
        ]
