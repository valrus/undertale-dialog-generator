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
import Modal
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
  , modal : Modal.Model
  }


init : List Character.Name -> Model
init characters =
  { characters = characters
  , selection = Nothing
  , moodImg = Nothing
  , text = ""
  , staticRoot = "/static/"
  , scriptRoot = ""
  , imageData = Nothing
  , modal = Modal.init <| grayscale 1
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
  [ on "input" targetValue (\s -> Signal.message address <| EnterText s)
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
    [ --onClick targetValue (\s -> Signal.message address <| EnterText text)
    -- , downloadAs "undertale-dialog.png"
    -- , href pngData
    ]
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


-- Credits modal

expand : List (String, String)
expand = [ ("width", "100%"), ("height", "100%") ]


creditsImg : String -> Html
creditsImg staticRoot =
  img
  [ Html.Attributes.width 596
  , Html.Attributes.height 654
  , Html.Attributes.usemap "#creditsMap"
  , src <| staticRoot ++ "images/credits.png"
  ]
  [ ]


creditsMapArea : List Int -> String -> String -> Html
creditsMapArea coords caption url =
  Html.node "area"
  [ Html.Attributes.shape "rect"
  , Html.Attributes.title caption
  , Html.Attributes.alt caption
  , Html.Attributes.coords <| String.join ", " <| List.map toString coords
  , href url
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


modalDialog : String -> Modal.SizedHtml
modalDialog staticRoot =
  let
    innerDiv =
        div [ style <| [ ("backgroundColor", "white")
                       , ("color", "black")
                       ] ++ expand
            ]
            [ creditsImg staticRoot
            , creditsImgMap ]
  in Modal.SizedHtml innerDiv "596" "654"


infoButton : Signal.Address Action -> String -> Html
infoButton address root =
  button
  [ onClick address
    <| UpdateModal
    <| Modal.Show (Just <| modalDialog root)
  , style <|
    [ ("position", "fixed")
    , ("bottom", "10px")
    , ("right", "20px") ] ++ flatButton
  ]
  [ img [ src <| root ++ "images/creditsbutton.png" ] [ ] ]


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
      ChooseCharacter Character.Name
    | ChooseMood String
    | EnterText String
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload (Maybe String)
    | UpdateModal Modal.Action


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
