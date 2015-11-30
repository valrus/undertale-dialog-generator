module UndertaleDialog where

import Character
import Color exposing (..)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (container, image)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, src, style)
import Maybe exposing (Maybe)
import StartApp.Simple as StartApp


-- Model

type alias Model =
  { characters : List Character.Name
  , selection : Maybe Character.Name
  , moodImg : Maybe String
  , text : String
  }


init : List Character.Name -> Model
init characters =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  , text = ""
  }


-- View

-- General styles

flatButton =
  [ style
    [ ("backgroundColor", "transparent")
    , ("border", "none")
    ]
  ]

header s =
  div
  [ ]
  [ hr [ ] [ ]
  , h1 [ style [ ("text-align", "center") ] ] [ text s ] ]

blank = div [ ] [ ]

-- Character section

characterHeader = header "Character"

spriteFolder : Character.Name -> String
spriteFolder c = "sprites" ++ "/" ++ toString c

spriteNumber : Character.Name -> Int -> String
spriteNumber c n = (spriteFolder c) ++ "/" ++ (toString n) ++ ".png"

defaultSprite : Character.Name -> String
defaultSprite c = spriteNumber c 0

characterButton : Signal.Address Action -> Character.Name -> Html
characterButton address c =
  button
  (flatButton ++ [ onClick address <| ChooseCharacter c ])
  [ img [ src <| defaultSprite c ] [] ]

characterButtons : Signal.Address Action -> List Character.Name -> Html
characterButtons address characters =
  div [ ]
    [ ul
      [ class "characters" ]
      <| List.map (characterButton address) characters
    ]

-- Mood section

moodHeader = header "Mood"

moodButton : Signal.Address Action -> Character.Name -> Int -> Html
moodButton address c n =
  let
    spriteStr = spriteNumber c n
  in
    button
      (flatButton ++ [ onClick address <| ChooseMood spriteStr ])
      [ img [ src <| spriteNumber c n ] [ ] ]

moodButtons : Signal.Address Action -> Character.Name -> Html
moodButtons address c =
  div [ ]
    [ ul
      [ class "moods" ]
      <| List.map (moodButton address c) [ 0..(Character.moodCount c) - 1 ]
    ]

moodSection : Signal.Address Action -> Maybe Character.Name -> Html
moodSection address maybeChar =
  case maybeChar of
    Nothing -> blank
    Just c -> moodButtons address c

-- Dialog box

centeredDialogBox e =
  div
  [ style [ ("width", "100%") ] ]
  [ div
    [ style [ ("width", "594px"), ("margin", "0 auto") ] ]
    [ e ]
  ]

dialogBox text imgSrc =
  centeredDialogBox <| fromElement <| collage 594 170
    [ filled (grayscale 1) (rect 594 170)  -- outer black border
    , filled (grayscale 0) (rect 578 152)  -- outer white border
    , filled (grayscale 1) (rect 566 140)  -- inner black box
    , (toForm <| image 120 120 imgSrc) |> move (-211, 0)
    ]

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ("padding", "30px") ] ]
    [ characterHeader
    , characterButtons address model.characters
    , moodHeader
    , moodSection address model.selection
    , hr [ ] [ ]
    , Maybe.withDefault blank <| Maybe.map (dialogBox model.text) model.moodImg
    ]


-- Update

type Action = ChooseCharacter Character.Name | ChooseMood String | EnterText String


noop whatever = whatever


update action model =
  case action of
    ChooseCharacter c ->
      { model
      | selection = Just c
      , moodImg = Nothing
      , text = ""
      }
    ChooseMood s ->
      { model
      | moodImg = Just s
      , text = ""
      }
    EnterText s -> noop model
