module CreditsModal exposing (..)

import Either exposing (Either)
import Html exposing (..)
import Html.Attributes exposing (style)


-- Local stuff

import ImageMap exposing (mapArea)
import Modal exposing (SizedHtml, expand)


creditsImg : String -> Html msg
creditsImg staticRoot =
    img
        [ Html.Attributes.width 596
        , Html.Attributes.height 654
        , Html.Attributes.usemap "#creditsMap"
        , Html.Attributes.src <| staticRoot ++ "images/credits.png"
        ]
        []



-- TODO: use map (?) to reduce duplication of Either.Left


creditsImgMap : Html msg
creditsImgMap =
    Html.node
        "map"
        [ Html.Attributes.id "creditsMap"
        , Html.Attributes.name "creditsMap"
        ]
        [ mapArea
            [ 331, 75, 441, 96 ]
            "valrus's Twitter!"
          <|
            Either.Left "http://twitter.com/valrus"
        , mapArea
            [ 299, 110, 475, 132 ]
            "This web page's source code!"
          <|
            Either.Left "https://github.com/valrus/undertale-dialog-generator"
        , mapArea
            [ 448, 192, 523, 218 ]
            "Determination, the Better Undertale Font!"
          <|
            Either.Left "https://www.behance.net/gallery/31268855/Determination-Better-Undertale-Font"
        , mapArea
            [ 152, 228, 264, 254 ]
            "Monster Friend, the Undertale Logo Font!"
          <|
            Either.Left "https://www.behance.net/gallery/31378523/Monster-Friend-Undertale-Logo-Font"
        , mapArea
            [ 152, 264, 495, 291 ]
            "JapanYoshi's Behance page!"
          <|
            Either.Left "https://www.behance.net/JapanYoshi"
        , mapArea
            [ 338, 359, 456, 391 ]
            "The official Undertale website!"
          <|
            Either.Left "http://undertale.com"
        ]


creditsDialog : String -> SizedHtml msg
creditsDialog staticRoot =
    let
        innerDiv =
            div
                [ style <|
                    [ ( "backgroundColor", "white" )
                    , ( "color", "black" )
                    ]
                        ++ expand
                ]
                [ creditsImg staticRoot
                , creditsImgMap
                ]
    in
        SizedHtml innerDiv "596" "654"
