module UndertaleDialog where

import StartApp exposing (start)

import Character
import Color exposing (grayscale)
import Effects exposing (Effects, Never, none)
import Either exposing (Either)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (image)
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, src, style)
import Http
import Json.Decode exposing (object2, string, (:=))
import Maybe exposing (Maybe, andThen)
import String
import Task
import Text

import Imgur
import Modal

-- Could split the map stuff into a different module
import CreditsModal exposing (creditsDialog, mapArea)

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
  , imgur : Imgur.Model
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
  , imgur = Imgur.init
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


titleImgMap : Signal.Address Action -> Html
titleImgMap address =
  Html.node "map"
  [ Html.Attributes.id "titleMap"
  , Html.Attributes.name "titleMap"
  ]
  [ mapArea [606, 43, 626, 61] "hOI!"
    <| Either.Right
    <| (address, ChooseCharacter Character.Temmie)
  ]


title root address =
  div
  [ style
    [ ("padding-top", "100px")
    , ("padding-bottom", "30px")
    , ("display", "block")
    ]
  ]
  [ img
    [ style
      [ ("margin", "0 auto")
      , ("display", "block") ]
    , src <| root ++ "images/title.png"
    , Html.Attributes.usemap "#titleMap"
    ]
    [ ]
  , titleImgMap address
  ]


-- Character section


spriteFolder : String -> Character.Name -> String
spriteFolder root c = root ++ "images/sprites/" ++ toString c


spriteNumber : String -> Character.Name -> Int -> String
spriteNumber root c n = (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"


defaultSprite : String -> Character.Name -> String
defaultSprite root c = spriteNumber root c 0


characterButton : Signal.Address Action -> String -> Character.Name -> Html
characterButton address staticRoot c =
  case c of
    Character.Temmie -> blank
    _ ->
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

indentAsterisk : Maybe Character.Name -> Html
indentAsterisk character =
  div
  [ Html.Attributes.id "indent"
  , style <| Character.fontStyles character
  ]
  [ Html.text <| Character.dialogAsterisk character ]


textBox : Signal.Address Action -> Maybe Character.Name -> String -> Html
textBox address character text =
  textarea
  [ Html.Attributes.id "textBox"
  , on "input" targetValue (\s -> Signal.message address <| EnterText s False)
  , style <|
    [ ("line-height", "36px")  -- TODO: Make the "36px" a function
    ] ++ (Character.fontStyles character) ++ (Character.textboxStyles character)
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
      , indentAsterisk model.selection
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


dialogBox : Signal.Address Action -> Model -> Maybe (List Html)
dialogBox address model =
  case model.moodImg of
    Nothing -> Nothing
    Just imgSrc ->
      let (imgX, imgY) = Character.portraitOffset model.selection
      in
        Just
        [
          dialogCollage
          ( fromElement <| collage 596 168
            [ filled (grayscale 1) (rect 596 168)  -- outer black border
            , filled (grayscale 0) (rect 580 152)  -- outer white border
            , filled (grayscale 1) (rect 568 140)  -- inner black box
            , (toForm <| doubleImage imgSrc <| Character.portraitSize model.selection)
              |> move (-214 + imgX, imgY)
            ]
          ) address model
        ]


dialogBoxImg : String -> Signal.Address Action -> String -> Maybe (List Html)
dialogBoxImg text address pngData =
  Just <|
  [
    Html.a
    [ ]
    [ Html.img
        [ onClick address <| EnterText text True
        , style
        [ ("margin", "0 auto")
        , ("display", "block")
        ]
        , src pngData
        ]
        [ ]
    ]
  ]


returnedDialogBox : String -> Signal.Address Action -> Maybe String -> Maybe (List Html)
returnedDialogBox text address imgData =
  Maybe.map2 (++)
  (Just "data:image/png;base64,") imgData
  `andThen` dialogBoxImg text address


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


imgurButtonSrc root = root ++ "images/imgur2.png"

dialogBoxSection address model =
  div
  [ ]
  <| Maybe.withDefault [ blank ]
  <| Maybe.oneOf
     [ Maybe.map2 (++)
       (returnedDialogBox model.text address model.imageData)
       ( Just <|
         [ Imgur.view (Signal.forwardTo address UpdateImgur) model.imgur
           <| imgurButtonSrc model.staticRoot
         ]
       )
     , dialogBox address model]


view : Signal.Address Action -> Model -> Html
view address model =
  div

    [ Html.Attributes.id "content" ]

    [ title model.staticRoot address

    , characterButtons address model.staticRoot model.characters

    , maybeDivider model.selection
    , moodSection address model.staticRoot model.selection

    , maybeDivider model.moodImg
    , dialogBoxSection address model

    , infoButton address model.staticRoot
    , Modal.view (Signal.forwardTo address UpdateModal) model.modal
    ]


-- Update

type Action =
      NoOp ()
    | ChooseCharacter Character.Name
    | ChooseMood String
    | EnterText String Bool
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload (Maybe String)
    | UpdateModal Modal.Action
    | UpdateImgur Imgur.Action


update : Action -> Model -> (Model, Effects Action)
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
      , toJSEffect model.jsAddress
        { elementId = "textBox"
        , moveCursorToEnd = False
        }
      )
    EnterText s moveCursor ->
      ( { model
        | text = s
        , imageData = Nothing
        }
      , toJSEffect model.jsAddress
        { elementId = "textBox"
        , moveCursorToEnd = moveCursor
        }
      )
    SetScriptRoot s ->
      ( { model
        | scriptRoot = s
        }
      , getImgurParams s
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
    GotDownload data ->
      let (newImgur, fx) = Imgur.update (Imgur.SetImageData data) model.imgur
      in
        ( { model
          | imageData = data
          , imgur = newImgur
          }
        , none
        )
    UpdateModal action ->
      ( { model
        | modal = Modal.update action model.modal
        }
      , none
      )
    UpdateImgur action ->
      let (newImgur, fx) = Imgur.update action model.imgur
      in
        ( { model
          | imgur = newImgur
          }
        , Effects.map UpdateImgur fx
        )


-- Tasks

getSubmitUrl root = root ++ "/submit"


getDialogBoxImg : Model -> Effects Action
getDialogBoxImg model =
  case Maybe.map2 (,) model.selection model.moodImg of
    Nothing -> none
    Just (c, img) ->
      Http.url (getSubmitUrl model.scriptRoot)
        [ ("character", toString c)
        , ("moodImg", img)
        , ("text", model.text)
        ]
      |> Http.getString
      |> Task.toMaybe
      |> Task.map GotDownload
      |> Effects.task


getImgurParamsUrl root = root ++ "/imgur_id"


imgurParamsDecoder = object2 (,) ("clientId" := string) ("albumId" := string)


getImgurParams : String -> Effects Action
getImgurParams scriptRoot =
  getImgurParamsUrl scriptRoot
  |> Http.get imgurParamsDecoder
  |> Task.toMaybe
  |> Task.map (\ms -> UpdateImgur <| Imgur.SetParams ms)
  |> Effects.task


-- Focus (reference: https://gist.github.com/pdamoc/97ca5e1ad605f7e5ebcb)

type alias FocusParams =
  { elementId : String
  , moveCursorToEnd : Bool
  }


emptyParams = { elementId = "", moveCursorToEnd = False }


type JSAction = Focus FocusParams | NoJSOp


toJSEffect : Signal.Address JSAction -> FocusParams -> Effects Action
toJSEffect address params =
  Signal.send address (Focus params) |> Task.map NoOp |> Effects.task


toJSMailbox = Signal.mailbox NoJSOp


focusFilter : JSAction -> Maybe FocusParams
focusFilter action =
  case action of
    Focus params -> Just params
    _ -> Nothing


port focus : Signal FocusParams
port focus = Signal.filterMap focusFilter emptyParams toJSMailbox.signal


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
      , Character.Flowey
      , Character.Temmie
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
