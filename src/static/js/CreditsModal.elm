module CreditsModal where

import Html exposing (..)
import Html.Attributes exposing (style)
import String exposing (join)

import Modal exposing (SizedHtml)

expand : List (String, String)
expand = [ ("width", "100%"), ("height", "100%") ]


creditsImg : String -> Html
creditsImg staticRoot =
  img
  [ Html.Attributes.width 596
  , Html.Attributes.height 654
  , Html.Attributes.usemap "#creditsMap"
  , Html.Attributes.src <| staticRoot ++ "images/credits.png"
  ]
  [ ]


creditsMapArea : List Int -> String -> String -> Html
creditsMapArea coords caption url =
  Html.node "area"
  [ Html.Attributes.shape "rect"
  , Html.Attributes.title caption
  , Html.Attributes.alt caption
  , Html.Attributes.coords <| join ", " <| List.map toString coords
  , Html.Attributes.href url
  ]
  [ ]


creditsImgMap : Html
creditsImgMap =
  Html.node "map"
  [ Html.Attributes.id "creditsMap"
  , Html.Attributes.name "creditsMap"
  ]
  [ creditsMapArea [331, 75, 441, 96] "valrus's Twitter!" "http://twitter.com/valrus"
  , creditsMapArea [299, 110, 475, 132] "This web page's source code!" "https://github.com/valrus/undertale-dialog-generator"
  , creditsMapArea [448, 192, 523, 218] "Determination, the Better Undertale Font!" "https://www.behance.net/gallery/31268855/Determination-Better-Undertale-Font"
  , creditsMapArea [152, 228, 264, 254] "Monster Friend, the Undertale Logo Font!" "https://www.behance.net/gallery/31378523/Monster-Friend-Undertale-Logo-Font"
  , creditsMapArea [152, 264, 495, 291] "JapanYoshi's Behance page!" "https://www.behance.net/JapanYoshi"
  , creditsMapArea [338, 359, 456, 391] "The official Undertale website!" "http://undertale.com"
  ]


creditsDialog : String -> SizedHtml
creditsDialog staticRoot =
  let
    innerDiv =
        div [ style <| [ ("backgroundColor", "white")
                       , ("color", "black")
                       ] ++ expand
            ]
            [ creditsImg staticRoot
            , creditsImgMap ]
  in SizedHtml innerDiv "596" "654"

