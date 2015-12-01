module UndertaleDialog where

import Character
import Color exposing (..)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (container, image)
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, classList, src, style)
import Maybe exposing (Maybe)
import Text
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
  , h1 [ style [ ("text-align", "center")
               , ("font-family", "8bitoperator JVE Regular") ]
       , class "header"
       ]
    [ text s ]
  ]

maybeHeader choice s =
  case choice of
    Nothing -> blank
    Just a -> header s

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

moodHeader choice = maybeHeader choice "Mood"

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

-- Text section

textHeader choice = maybeHeader choice "Text"

textBox : Signal.Address Action -> Html
textBox address =
  div
  [ ]
  [ textarea
    [ on "input" targetValue (\s -> Signal.message address <| EnterText s)
    , style [ ("width", "200px"), ("height", "170px"), ("float", "left") ] ]
    [ ]
  ]

textSection : Signal.Address Action -> Maybe a -> Html
textSection address x =
  case x of
    Nothing -> blank
    Just something -> textBox address

-- Dialog box

centeredDialogBox e =
  div
  [ style [ ("width", "100%") ] ]
  [ div
    [ style [ ("width", "594px"), ("float", "right") ] ]
    [ e ]
  ]

dialogText : String -> Text.Text
dialogText s =
  Text.typeface ["determination_monoregular"]
  <| Text.height 26  -- 26 px ~= 20 pt?
  <| Text.color (grayscale 0)
  <| Text.monospace <| Text.fromString s

dialogElement : String -> Graphics.Element.Element
dialogElement s =
  Graphics.Element.size 416 120 <| Graphics.Element.leftAligned <| dialogText s

dialogBox model =
  case model.moodImg of
    Nothing -> Nothing
    Just imgSrc ->
      Just <| centeredDialogBox <| fromElement <| collage 596 170
      [ filled (grayscale 1) (rect 596 170)  -- outer black border
      , filled (grayscale 0) (rect 580 152)  -- outer white border
      , filled (grayscale 1) (rect 568 140)  -- inner black box
      , (toForm <| image 120 120 imgSrc) |> move (-214, 0)
      , (toForm <| dialogElement model.text) |> move (64, 0) -- this is kind of a guess
      ]

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ("padding", "28px") ] ]
    [ characterHeader
    , characterButtons address model.characters
    , moodHeader model.selection
    , moodSection address model.selection
    , textHeader model.moodImg
    , textSection address model.moodImg
    , Maybe.withDefault blank <| dialogBox model
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
    EnterText s ->
      { model
      | text = s
      }
