module Modal where

import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe)


type alias Model =
  { backgroundColor : Color
  , childElement : Maybe Html
  }



init : Color -> Model
init color =
  { backgroundColor = color
  , childElement = Nothing
  }


type Action = Show (Maybe Html) | NoOp


update action model =
  case action of
    Show something ->
      { model
      | childElement = something
      }
    NoOp -> model


partlyTransparent : Color -> Color
partlyTransparent color =
  let rgb = (Color.toRgb color) in
    Color.rgba rgb.red rgb.green rgb.blue 0.7


backgroundAttrs address color =
  [ onClick address <| (Show Nothing)
  , style
    [ ("backgroundColor", toString (partlyTransparent color))
    , ("height", "100%")
    , ("width", "100%")
    , ("position", "fixed")
    , ("top", "0")
    , ("left", "0")
    , ("z-index", "99999")
    ]
  ]


view : Signal.Address Action -> Model -> Html
view address model =
  case model.childElement of
    Nothing -> div [ ] [ ]
    Just dialog ->
      div
      ( backgroundAttrs address model.backgroundColor )
      [ dialog ]
