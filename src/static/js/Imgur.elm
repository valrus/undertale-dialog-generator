module Imgur where

import Effects exposing (Effects, none)
import Either exposing (Either)
import Html exposing (..)
import Html.Attributes exposing (src, id, style)
import Html.Events exposing (onClick)
import Http exposing (send, defaultSettings, fromJson, stringData)
import Json.Decode exposing (Decoder, at)
import Json.Encode as JSON
import Maybe exposing (Maybe, withDefault)
import Task exposing (Task)


type Action =
  NoOp
  | SetParams (Maybe (String, String))
  | SetImageData (Maybe ImgData)
  | DoUpload
  | SetUploadUrl (Maybe ImgUrl)


type alias ImgData = String
type alias ImgUrl = String
type alias ImgState = Either ImgData ImgUrl


type alias Model =
  { clientId : Maybe String
  , albumId : Maybe String
  , imgState : Maybe ImgState
  }


init : Model
init =
  { clientId = Nothing
  , albumId = Nothing
  , imgState = Nothing
  }


uploadButton : Signal.Address Action -> ImgState -> String -> Html
uploadButton address state imgSrc =
  let attrs =
    case state of
      Either.Left data ->
        [ onClick address DoUpload
        , style [ ("border", "1px solid white") ]
        ]
      Either.Right url ->
        [ style [ ("border", "1px solid black") ] ]
  in
    button
    ( [ id "imgurButton" ] ++ attrs )
    [ img
      [ src imgSrc ]
      [ ]
    ]


uploadField : ImgState -> Html
uploadField state =
  let content =
    case state of
        Either.Left data -> div [ ] [ ]
        Either.Right url -> div [ ] [ Html.text url ]
  in
    div
    [ id "imgurUrl" ]
    [ content ]


uploadView : Signal.Address Action -> ImgState -> String -> Html
uploadView address state imgSrc =
  div
  [ id "imgurUpload" ]
  [ uploadButton address state imgSrc
  , uploadField state]


view address model buttonImgSrc =
  case model.imgState of
    Nothing -> div [] []
    Just state -> uploadView address state buttonImgSrc


responseDecoder : Decoder String
responseDecoder =
  at ["data", "link"] Json.Decode.string


albumData : String -> List (String, JSON.Value)
albumData id = [ ("album", JSON.string id) ]


doUpload : Model -> Effects Action
doUpload model =
  case Maybe.map2 (,) model.clientId model.imgState of
    Just (id, Either.Left data) ->
      send defaultSettings
        { verb = "POST"
        , url = "https://api.imgur.com/3/upload"
        , headers =
            [ ("Authorization", "Client-ID " ++ id)
            , ("Content-Type", "application/json")
            ]
        , body =
            Http.string <| JSON.encode 0 <| JSON.object <|
              [ ("image", JSON.string data)
              , ("type", JSON.string "base64")
              ] ++ (Maybe.withDefault [] <| Maybe.map albumData model.albumId)
        }
      |> fromJson responseDecoder
      |> Task.toMaybe
      |> Task.map SetUploadUrl
      |> Effects.task
    _ -> none


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    SetParams (Just (client, album)) ->
      ( { model
        | clientId = Just client
        , albumId = Just album
        }
      , none
      )
    SetImageData data ->
      ( { model
        | imgState = Maybe.map Either.Left data
        }
      , none
      )
    DoUpload ->
      ( model
      , doUpload model
      )
    SetUploadUrl maybeUrl ->
      case maybeUrl of
        Nothing -> (model, none) -- actually handle this error :3
        Just url ->
          ( { model
            | imgState = Just <| Either.Right url
            }
          , none
          )
    _ -> (model, none)

