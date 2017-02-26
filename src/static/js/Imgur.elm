module Imgur exposing (..)

import Either exposing (Either)
import Html exposing (..)
import Html.Attributes exposing (src, id, style, href)
import Html.Events exposing (onClick)
import Http exposing (request, send, expectJson, stringBody)
import Json.Decode exposing (Decoder, at)
import Json.Encode as JSON
import Maybe exposing (Maybe, withDefault)


type Msg
    = NoOp
    | SetParams (Result Http.Error ( String, String ))
    | SetImageData ImgData
    | DoUpload
    | SetUploadUrl (Maybe ImgUrl)


type UploadStatus
    = NotStarted
    | InProgress
    | Finished
    | Failed


type alias ImgData =
    String


type alias ImgUrl =
    String


type alias ImgState =
    Either ImgData ImgUrl


type alias Model =
    { clientId : Maybe String
    , albumId : Maybe String
    , imgState : Maybe ImgState
    , uploadStatus : UploadStatus
    }


init : Model
init =
    { clientId = Nothing
    , albumId = Nothing
    , imgState = Nothing
    , uploadStatus = NotStarted
    }


uploadButton : ImgState -> String -> Html Msg
uploadButton state imgSrc =
    let
        attrs =
            case state of
                Either.Left data ->
                    [ onClick DoUpload
                    , style [ ( "border", "1px solid white" ) ]
                    ]

                Either.Right url ->
                    [ style [ ( "border", "1px solid black" ) ] ]
    in
        button
            ([ id "imgurButton" ] ++ attrs)
            [ img
                [ src imgSrc ]
                []
            ]


uploadField : ImgState -> Html Msg
uploadField state =
    let
        content =
            case state of
                Either.Left data ->
                    div [] []

                Either.Right url ->
                    div
                        []
                        [ Html.a
                            [ href url ]
                            [ Html.text url ]
                        ]
    in
        div
            [ id "imgurUrl" ]
            [ content ]


uploadView : ImgState -> String -> Html Msg
uploadView state imgSrc =
    div
        [ id "imgurUpload" ]
        [ uploadButton state imgSrc
        , uploadField state
        ]


imgurButtonSrc : UploadStatus -> String -> String
imgurButtonSrc status root =
    let
        fileName =
            case status of
                NotStarted ->
                    "upload-start.png"

                InProgress ->
                    "upload-anim.gif"

                Finished ->
                    "upload-done.png"

                Failed ->
                    "upload-failed.png"
    in
        root ++ "images/" ++ fileName


view : Model -> String -> Html Msg
view model staticRoot =
    case model.imgState of
        Nothing ->
            div [] []

        Just state ->
            uploadView state <|
                imgurButtonSrc model.uploadStatus staticRoot


responseDecoder : Decoder String
responseDecoder =
    at [ "data", "link" ] Json.Decode.string


albumData : String -> List ( String, JSON.Value )
albumData id =
    [ ( "album", JSON.string id ) ]


doUpload : Model -> Cmd Msg
doUpload model =
    case Maybe.map2 (,) model.clientId model.imgState of
        Just ( id, Either.Left data ) ->
            request
                { method = "POST"
                , headers =
                    [ Http.header "Authorization" ("Client-ID " ++ id)
                    , Http.header "Content-Type" "application/json"
                    ]
                , url = "https://api.imgur.com/3/upload"
                , body =
                    stringBody "application/json" <|
                        JSON.encode 0 <|
                            JSON.object <|
                                [ ( "image", JSON.string data )
                                , ( "type", JSON.string "base64" )
                                ]
                                    ++ (Maybe.withDefault [] <| Maybe.map albumData model.albumId)
                , expect = expectJson responseDecoder
                , timeout = Nothing
                , withCredentials = False
                }
                |> send (Result.toMaybe >> SetUploadUrl)

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetParams (Ok ( client, album )) ->
            ( { model
                | clientId = Just client
                , albumId = Just album
              }
            , Cmd.none
            )

        SetParams (Err _) ->
            ( model, Cmd.none )

        SetImageData data ->
            ( { model
                | imgState = Just (Either.Left data)
                , uploadStatus = NotStarted
              }
            , Cmd.none
            )

        DoUpload ->
            ( { model
                | uploadStatus = InProgress
              }
            , doUpload model
            )

        SetUploadUrl maybeUrl ->
            case maybeUrl of
                Nothing ->
                    ( { model
                        | uploadStatus = Failed
                      }
                    , Cmd.none
                    )

                Just url ->
                    ( { model
                        | imgState = Just <| Either.Right url
                        , uploadStatus = Finished
                      }
                    , Cmd.none
                    )

        _ ->
            ( model, Cmd.none )
