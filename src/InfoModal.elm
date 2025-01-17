module InfoModal exposing (..)

-- Local

import Either exposing (Either)
import Html exposing (..)
import Html.Attributes exposing (style)
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
            [ 430, 134, 529, 160 ]
            "IDTHV on GitHub!"
          <|
            Either.Left "https://github.com/valrus/undertale-dialog-generator/issues"
        ]


infoImg : String -> Html msg
infoImg staticRoot =
    img
        [ Html.Attributes.width 596
        , Html.Attributes.height 400
        , Html.Attributes.usemap "#infoMap"
        , Html.Attributes.src <| staticRoot ++ "images/info.png"
        ]
        []


infoDialog : String -> SizedHtml msg
infoDialog staticRoot =
    let
        innerDiv =
            div
                (style "color" "black"
                    :: expand
                )
                [ infoImg staticRoot
                , infoImgMap
                ]
    in
    SizedHtml innerDiv "596" "654"
