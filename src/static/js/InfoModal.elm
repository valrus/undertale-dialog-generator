module InfoModal exposing (..)

import Either exposing (Either)
import Html exposing (..)
import Html.Attributes exposing (style)


-- Local

import ImageMap exposing (mapArea)
import Modal exposing (SizedHtml, expand)


infoImgMap : Html msg
infoImgMap =
    Html.node
        "map"
        [ Html.Attributes.id "infoMap"
        , Html.Attributes.name "infoMap"
        ]
        [ mapArea
            [ 62, 121, 161, 147 ]
            "IDTHV on Twitter!"
          <|
            Either.Left "https://twitter.com/IDTHV"
        , mapArea
            [ 430, 186, 529, 212 ]
            "IDTHV on GitHub!"
          <|
            Either.Left "https://github.com/valrus/undertale-dialog-generator/issues"
        ]


infoImg : String -> Html msg
infoImg staticRoot =
    img
        [ Html.Attributes.width 596
        , Html.Attributes.height 654
        , Html.Attributes.usemap "#infoMap"
        , Html.Attributes.src <| staticRoot ++ "images/info.png"
        ]
        []


infoDialog : String -> SizedHtml msg
infoDialog staticRoot =
    let
        innerDiv =
            div
                [ style <|
                    [ ( "backgroundColor", "white" )
                    , ( "color", "black" )
                    ]
                        ++ expand
                ]
                [ infoImg staticRoot
                , infoImgMap
                ]
    in
        SizedHtml innerDiv "596" "654"
