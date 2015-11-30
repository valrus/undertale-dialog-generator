module UndertaleDialog where

import Character
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
  }


init : List Character.Name -> Model
init characters =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  }


-- View

-- General styles

flatButton =
  [ style
    [ ("backgroundColor", "transparent")
    , ("border", "none")
    ]
  ]

-- Character section

characterHeader = h1 [] [ text "Character" ]

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

moodHeader = h1 [] [ text "Mood" ]

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
    Nothing -> div [] []
    Just c -> moodButtons address c

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ characterHeader
    , characterButtons address model.characters
    , moodHeader
    , moodSection address model.selection
    ]


-- Update

type Action = ChooseCharacter Character.Name | ChooseMood String | EnterText


noop whatever = whatever


update action model =
  case action of
    ChooseCharacter c -> { model | selection = Just c }
    ChooseMood s -> { model | moodImg = Just s }
    EnterText -> noop model
