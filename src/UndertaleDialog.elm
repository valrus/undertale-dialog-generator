module UndertaleDialog where

import Character
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, src)
import Maybe exposing (Maybe)
import StartApp.Simple as StartApp


-- Model

type alias Model =
  { characters : List Character.Name
  , selection : Maybe Character.Name
  , moodImg : Maybe String
  }


init : List Character.Name -> Model
init characters selection =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  }


-- View

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
  button [ onClick address <| ChooseCharacter c ] [ img [ src <| defaultSprite c ] [] ]

characterButtons : Signal.Address Action -> List Character.Name -> Html
characterButtons address characters =
  div [ ]
    [ ul
      [ class "characters" ]
      <| List.map (characterButton address) characters
    ]

-- Mood section

moodHeader = h1 [] [ text "Mood" ]

moodButton : Character.Name -> Int -> Html
moodButton c n =
  button [ ] [ img [ src <| spriteNumber c n ] [ ] ]

moodButtons : Character.Name -> Html
moodButtons c =
  div [ ]
    [ ul
      [ class "moods" ]
      <| List.map (moodButton c) [ 0..(Character.moodCount c) - 1 ]
    ]

moodSection : Maybe Character.Name -> Html
moodSection maybeChar =
  case maybeChar of
    Nothing -> div [] []
    Just c -> moodButtons c

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ characterHeader
    , characterButtons address model.characters
    , moodHeader
    , moodSection model.selection
    ]


-- Update

type Action = ChooseCharacter Character.Name| ChooseMood | EnterText


noop whatever = whatever


update action model =
  case action of
    ChooseCharacter c -> { model | selection = Just c }
    ChooseMood -> noop model
    EnterText -> noop model
