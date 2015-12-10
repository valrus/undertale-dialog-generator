module UndertaleDialog where

import StartApp exposing (start)

import Character
import Color exposing (..)
import Effects exposing (none, Effects, Never)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (container, image)
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, classList, src, style)
import Http
import Json.Encode
import Json.Decode
import Maybe exposing (Maybe)
import String
import Task
import Text


-- Model

type alias Model =
  { characters : List Character.Name
  , selection : Maybe Character.Name
  , moodImg : Maybe String
  , text : String
  , staticRoot : String
  , scriptRoot : String
  , imageData : Maybe String
  }


init : List Character.Name -> Model
init characters =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  , text = ""
  , staticRoot = ""
  , scriptRoot = ""
  , imageData = Nothing
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
               , ("font-family", "determination_monoregular") ]
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

spriteFolder : String -> Character.Name -> String
spriteFolder root c = root ++ "images/sprites/" ++ toString c

spriteNumber : String -> Character.Name -> Int -> String
spriteNumber root c n = (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"

defaultSprite : String -> Character.Name -> String
defaultSprite root c = spriteNumber root c 0

characterButton : Signal.Address Action -> String -> Character.Name -> Html
characterButton address staticRoot c =
  button
  (flatButton ++ [ onClick address <| ChooseCharacter c ])
  [ img [ src <| defaultSprite staticRoot c ] [] ]

characterButtons : Signal.Address Action -> String -> List Character.Name -> Html
characterButtons address staticRoot characters =
  div [ ]
    [ ul
      [ class "characters" ]
      <| List.map (characterButton address staticRoot) characters
    ]

-- Mood section

moodHeader choice = maybeHeader choice "Mood"

moodButton : Signal.Address Action -> String -> Character.Name -> Int -> Html
moodButton address root c n =
  let
    spriteStr = spriteNumber root c n
  in
    button
      (flatButton ++ [ onClick address <| ChooseMood spriteStr ])
      [ img [ src <| spriteStr ] [ ] ]

moodButtons : Signal.Address Action -> String -> Character.Name -> Html
moodButtons address root c =
  div [ ]
    [ ul
      [ class "moods" ]
      <| List.map (moodButton address root c) [ 0..(Character.moodCount c) - 1 ]
    ]

moodSection : Signal.Address Action -> String -> Maybe Character.Name -> Html
moodSection address root maybeChar =
  case maybeChar of
    Nothing -> blank
    Just c -> moodButtons address root c

-- Text section

textHeader choice = maybeHeader choice "Text"

textBox : Signal.Address Action -> Html
textBox address =
  div [ ]
  [ textarea
    [ on "input" targetValue (\s -> Signal.message address <| EnterText s)
    , style
      [ ("font-family", "monospace")
      , ("font-size", "24px")
      , ("float", "left")
      , ("resize", "none")
      ]
    , Html.Attributes.cols 24
    , Html.Attributes.rows 3
    ]
    [ ]
  ]

textSection : Signal.Address Action -> Maybe a -> Html
textSection address x =
  case x of
    Nothing -> blank
    Just something -> textBox address

-- Dialog box

dialogCollage address e =
  div
  [ style [ ("width", "100%") ]
  , onClick address GetDownload
  ]
  [ div
    [ style [ ("width", "594px"), ("float", "right") ] ]
    [ e ]
  ]

dialogLine : Maybe Character.Name -> String -> Text.Text
dialogLine c s =
  Text.typeface (Character.fontFace c)
  <| Text.height (Character.fontSize c)
  <| Text.color (grayscale 0)
  <| Text.monospace <| Text.fromString s

padWithBlanks : List String -> Int -> List String
padWithBlanks l n = List.append l <| List.repeat (n - List.length l) ""

dialogText : Maybe Character.Name -> String -> List Text.Text
dialogText c s =
  List.map
  (\line -> dialogLine c line)
  <| padWithBlanks (String.split "\n" s) 3

dialogLineElement : Text.Text -> Graphics.Element.Element
dialogLineElement t =
  Graphics.Element.size 416 36 <| Graphics.Element.leftAligned <| t

dialogElement : Maybe Character.Name -> String -> Graphics.Element.Element
dialogElement c s =
  Graphics.Element.flow Graphics.Element.down <| List.map dialogLineElement <| dialogText c s

dialogBox address model =
  case model.moodImg of
    Nothing -> Nothing
    Just imgSrc ->
      Just <| dialogCollage address <| fromElement <| collage 596 168
      [ filled (grayscale 1) (rect 596 168)  -- outer black border
      , filled (grayscale 0) (rect 580 152)  -- outer white border
      , filled (grayscale 1) (rect 568 140)  -- inner black box
      , (toForm <| image 120 120 imgSrc) |> move (-214, 0)
      , (toForm <| dialogElement model.selection model.text) |> move (64, 0) -- this is kind of a guess
      ]

returnedDialogBox dialogBoxBase64 =
  Html.img
  [ style [ ("float", "right") ]
  , src <| "data:image/png;base64," ++ dialogBoxBase64
  ]
  [ ]

view : Signal.Address Action -> Model -> Html
view address model =
  div
    [ style [ ("padding", "26px") ] ]
    [ characterHeader
    , characterButtons address model.staticRoot model.characters
    , moodHeader model.selection
    , moodSection address model.staticRoot model.selection
    , textHeader model.moodImg
    , Maybe.withDefault blank <|
      Maybe.oneOf
      [ Maybe.map returnedDialogBox model.imageData
      , dialogBox address model]
    , textSection address model.moodImg
    ]


-- Update

type Action =
      ChooseCharacter Character.Name
    | ChooseMood String
    | EnterText String
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload (Maybe String)


update action model =
  case action of
    ChooseCharacter c ->
      ( { model
        | selection = Just c
        , moodImg = Nothing
        , text = ""
        , imageData = Nothing
        }
      , none
      )
    ChooseMood s ->
      ( { model
        | moodImg = Just s
        , imageData = Nothing
        }
      , none
      )
    EnterText s ->
      ( { model
        | text = s
        , imageData = Nothing
        }
      , none
      )
    SetScriptRoot s ->
      ( { model
        | scriptRoot = s
        }
      , none
      )
    SetStaticRoot s ->
      ( { model
        | staticRoot = s
        }
      , none
      )
    GetDownload ->
      ( model
      , getDialogBoxImg model
      )
    GotDownload url ->
      ( { model
        | imageData = url
        }
      , none
      )


-- Tasks

getSubmitURL root = root ++ "submit"

getDialogBoxImg : Model -> Effects Action
getDialogBoxImg model =
  case Maybe.map2 (,) model.selection model.moodImg of
    Nothing -> none
    Just (c, img) ->
      Http.url (getSubmitURL model.scriptRoot)
      [ ("character", toString c)
      , ("moodImg", img)
      , ("text", model.text)
      ]
      |> Http.getString
      |> Task.toMaybe
      |> Task.map GotDownload
      |> Effects.task


-- Main

port scriptRoot : Signal String
port staticRoot : Signal String

app =
  start
  { init =
    ( init
      [ Character.Toriel
      , Character.Sans
      , Character.Papyrus
      , Character.Undyne
      , Character.Alphys
      ]
    , none
    )
  , update = update
  , view = view
  , inputs =
    [ Signal.map SetScriptRoot scriptRoot
    , Signal.map SetStaticRoot staticRoot ]
  }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
