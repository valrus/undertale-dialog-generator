module UndertaleDialog where

import StartApp exposing (start)

import Character
import Color exposing (..)
import Effects exposing (Effects, Never, none)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (Element, container, image, above, flow, down, size)
import Graphics.Input
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, classList, src, style, download, downloadAs, href)
import Http
import Json.Encode
import Json.Decode
import Maybe exposing (Maybe)
import String
import Task
import Text

import Modal
import CreditsModal exposing (creditsDialog)

-- Model

type alias Model =
  { characters : List Character.Name
  , selection : Maybe Character.Name
  , moodImg : Maybe String
  , text : String
  , staticRoot : String
  , scriptRoot : String
  , imageData : Maybe String
  , modal : Modal.Model
  , jsAddress : Signal.Address JSAction
  }


init : List Character.Name -> Signal.Address JSAction -> Model
init characters jsAddress =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  , text = ""
  , staticRoot = "/static/"
  , scriptRoot = ""
  , imageData = Nothing
  , modal = Modal.init <| grayscale 1
  , jsAddress = jsAddress
  }


-- View

-- General styles

flatButton =
  [ ("backgroundColor", "transparent")
  , ("border", "none")
  ]

header =
  div
  [ ]
  [ hr [ style [ ("margin-bottom", "30px") ] ] [ ] ]

maybeDivider choice =
  case choice of
    Nothing -> blank
    Just a -> header

blank = div [ ] [ ]


title root =
  img
  [ style
    [ ("margin", "0 auto")
    , ("padding-top", "100px")
    , ("padding-bottom", "30px")
    , ("display", "block") ]
  , src <| root ++ "images/title.png" ]
  [ ]


-- Character section


spriteFolder : String -> Character.Name -> String
spriteFolder root c = root ++ "images/sprites/" ++ toString c


spriteNumber : String -> Character.Name -> Int -> String
spriteNumber root c n = (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"


defaultSprite : String -> Character.Name -> String
defaultSprite root c = spriteNumber root c 0


characterButton : Signal.Address Action -> String -> Character.Name -> Html
characterButton address staticRoot c =
  button
  [ onClick address <| ChooseCharacter c
  , style flatButton
  ]
  [ img [ src <| defaultSprite staticRoot c ] [] ]


characterButtons : Signal.Address Action -> String -> List Character.Name -> Html
characterButtons address root characters =
  div [ ]
        [ ul
          [ class "characters" ]
          <| List.map (characterButton address root) characters
        ]


-- Mood section

moodButton : Signal.Address Action -> String -> Character.Name -> Int -> Html
moodButton address root c n =
  let
    spriteStr = spriteNumber root c n
  in
    button
      [ onClick address <| ChooseMood spriteStr
      , style flatButton
      ]
      [ img [ src <| spriteStr ] [ ] ]

moodButtons : Signal.Address Action -> String -> Character.Name -> Html
moodButtons address root c =
  div [ ]
    [ ul
      [ class "moods" ]
      <| List.map (moodButton address root c) [ 1..(Character.moodCount c) ]
    ]

moodSection : Signal.Address Action -> String -> Maybe Character.Name -> Html
moodSection address root maybeChar =
  case maybeChar of
    Nothing -> blank
    Just c -> moodButtons address root c


-- Dialog box

textBox : Signal.Address Action -> Maybe Character.Name -> String -> Html
textBox address character text =
  textarea
  [ Html.Attributes.id "textBox"
  , on "input" targetValue (\s -> Signal.message address <| EnterText s)
  , style <|
    [ ("line-height", "36px")  -- TODO: Make the "36px" a function
    ] ++ (Character.fontStyles character)
  , Html.Attributes.rows 3
  ]
  [ Html.text text ]


dialogCollage e address model =
  div
  [ style [ ("width", "100%") ] ]
  [
   div
    [ style
      [ ("width", "596px")
      , ("position", "relative")
      , ("margin", "0 auto") ]
    ]
    [ div
      [ Html.Attributes.id "dialog" ]
      [ e
      , textBox address model.selection model.text
      , crunchyButton address ]
    ]
  ]


crunchyButton address =
  div
  [ style [ ("width", "100%") ] ]
  [
    Html.button
    [ onClick address <| GetDownload
    , Html.Attributes.id "crunchybutton"
    ]
    [ text "MAKE IT CRUNCHY" ]
  ]


doubleImage imgSrc (w, h)=
  image (w * 2) (h * 2) imgSrc


dialogBox : Signal.Address Action -> Model -> Maybe Html
dialogBox address model =
  case model.moodImg of
    Nothing -> Nothing
    Just imgSrc ->
      let (imgX, imgY) = Character.portraitOffset model.selection
      in
        Just <| dialogCollage
        ( fromElement <| collage 596 168
          [ filled (grayscale 1) (rect 596 168)  -- outer black border
          , filled (grayscale 0) (rect 580 152)  -- outer white border
          , filled (grayscale 1) (rect 568 140)  -- inner black box
          , (toForm <| doubleImage imgSrc <| Character.portraitSize model.selection) |> move (-214 + imgX, imgY)
          ]
        ) address model


returnedDialogBox text address dialogBoxBase64 =
  let pngData = "data:image/png;base64," ++ dialogBoxBase64
  in
    Html.a
    [ ]
    [ Html.img
        [ onClick address <| EnterText text
        , style
          [ ("margin", "0 auto")
          , ("display", "block")
          ]
        , src pngData
        ]
        [ ]
    ]


-- Button for credits modal

infoButton : Signal.Address Action -> String -> Html
infoButton address root =
  button
  [ onClick address
    <| UpdateModal
    <| Modal.Show (Just <| creditsDialog root)
  , style <|
    [ ("position", "fixed")
    , ("bottom", "10px")
    , ("right", "20px") ] ++ flatButton
  ]
  [ img [ src <| root ++ "images/creditsbutton.png" ] [ ] ]


-- Main view

view : Signal.Address Action -> Model -> Html
view address model =
  div

    [ Html.Attributes.id "content" ]

    [ title model.staticRoot

    , characterButtons address model.staticRoot model.characters

    , maybeDivider model.selection
    , moodSection address model.staticRoot model.selection

    , maybeDivider model.moodImg
    , Maybe.withDefault blank <|
      Maybe.oneOf
      [ Maybe.map (returnedDialogBox model.text address) model.imageData
      , dialogBox address model]

    , infoButton address model.staticRoot
    , Modal.view (Signal.forwardTo address UpdateModal) model.modal
    ]


-- Update

type Action =
      NoOp ()
    | ChooseCharacter Character.Name
    | ChooseMood String
    | EnterText String
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload (Maybe String)
    | UpdateModal Modal.Action


update action model =
  case action of
    NoOp () ->
      ( model
      , none
      )
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
      , toJSEffect model.jsAddress "textBox"
      )
    EnterText s ->
      ( { model
        | text = s
        , imageData = Nothing
        }
      , toJSEffect model.jsAddress "textBox"
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
    UpdateModal action ->
      ( { model
        | modal = Modal.update action model.modal
        }
      , none
      )


-- Tasks

getSubmitURL root = root ++ "/submit"


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


-- Focus (reference: https://gist.github.com/pdamoc/97ca5e1ad605f7e5ebcb)

type JSAction = Focus String | NoJSOp


toJSEffect : Signal.Address JSAction -> String -> Effects Action
toJSEffect address s =
  Signal.send address (Focus s) |> Task.map NoOp |> Effects.task


toJSMailbox = Signal.mailbox NoJSOp


focusFilter : JSAction -> Maybe String
focusFilter action =
  case action of
    Focus s -> Just s
    _ -> Nothing


port focus : Signal String
port focus = Signal.filterMap focusFilter "" toJSMailbox.signal


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
      , Character.Asgore
      , Character.Napstablook
      , Character.Mettaton
      ]
      toJSMailbox.address
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
