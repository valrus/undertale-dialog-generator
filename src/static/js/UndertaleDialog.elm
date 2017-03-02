module UndertaleDialog exposing (..)

import Array exposing (Array, toList, fromList)
import Char exposing (KeyCode)
import Color exposing (grayscale)
import Either exposing (Either)
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick, keyCode)
import Html.Attributes exposing (class, src, style)
import Http
import Json.Decode as Json
import Keyboard
import Maybe exposing (Maybe, andThen)
import Maybe.Extra exposing (combine, isJust, join, maybeToList)
import Platform.Cmd
import Focus

import Debug exposing (log)


-- Local modules

import Helpers exposing (..)
import Character exposing (thumbnail)
import CheatCode
import Imgur
import Modal
import ImageMap exposing (mapArea)
import CreditsModal exposing (creditsDialog)
import InfoModal exposing (infoDialog)
import DialogBoxes


-- Model


type alias Model =
    { characters : List Character.Name
    , selection : Maybe Character.Name
    , dialogs : DialogBoxes.Model
    , staticRoot : String
    , scriptRoot : String
    , imageData : Maybe String
    , modal : Modal.Model
    , cheatCode : CheatCode.Model
    , imgur : Imgur.Model
    , exmode : Bool
    }


init : List Character.Name -> Flags -> ( Model, Cmd Msg )
init characters flags =
    ( { characters = characters
      , selection = Nothing
      , dialogs = DialogBoxes.init
      , staticRoot = flags.staticRoot
      , scriptRoot = flags.scriptRoot
      , imageData = Nothing
      , modal = Modal.init (grayscale 1)
      , cheatCode = CheatCode.init [ "EX" ]
      , imgur = Imgur.init
      , exmode = False
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | EnterCheatCode KeyCode
    | ActivateEXMode
    | UpdateDialogs DialogBoxes.Msg
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload String
    | UpdateModal Modal.Msg
    | UpdateImgur Imgur.Msg



-- View
-- General styles


flatButton : List ( String, String )
flatButton =
    [ ( "backgroundColor", "transparent" )
    , ( "border", "none" )
    , ( "display", "inline-block" )
    ]


header : Html Msg
header =
    div
        []
        [ hr [ style [ ( "margin-bottom", "30px" ) ] ] [] ]


maybeDivider : Maybe a -> Html Msg
maybeDivider choice =
    case choice of
        Nothing ->
            blank

        Just a ->
            header


blank : Html Msg
blank =
    div [] []


titleImgMap : String -> Html Msg
titleImgMap root =
    Html.node
        "map"
        [ Html.Attributes.id "titleMap"
        , Html.Attributes.name "titleMap"
        ]
        [ mapArea [ 606, 43, 626, 61 ] "hOI!" <|
            Either.Right <|
                (UpdateDialogs <|
                    DialogBoxes.SetImages Character.Temmie <|
                        defaultSprite root Character.Temmie False
                )
        ]


title : String -> Html Msg
title root =
    div
        [ style
            [ ( "padding-top", "60px" )
            , ( "padding-bottom", "30px" )
            , ( "display", "block" )
            ]
        ]
        [ img
            [ style
                [ ( "margin", "0 auto" )
                , ( "display", "block" )
                ]
            , src <| root ++ "images/title.png"
            , Html.Attributes.usemap "#titleMap"
            ]
            []
        , titleImgMap root
        ]



-- Character section


spriteFolder : String -> Character.Name -> String
spriteFolder root c =
    root ++ "images/sprites/" ++ toString c


spriteNumber : String -> Character.Name -> Int -> String
spriteNumber root c n =
    (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"


defaultSprite : String -> Character.Name -> Bool -> String
defaultSprite root c thumbnail =
    spriteNumber root c (if thumbnail then 0 else 1)


characterButton : String -> Character.Name -> Html Msg
characterButton staticRoot c =
    case c of
        Character.Temmie ->
            blank

        _ ->
            button
                [ onClick <|
                    UpdateDialogs <|
                        DialogBoxes.SetImages c <|
                            defaultSprite staticRoot c False
                , style flatButton
                ]
                [ img
                      -- Use Toriel here to avoid the Napstablook special case
                  (style (thumbnail Character.Toriel)
                  :: [ src <| defaultSprite staticRoot c True ])
                  []
                ]


characterButtons : String -> List Character.Name -> Html Msg
characterButtons root characters =
    div
        []
        [ ul
            [ class "characters" ]
          <|
            List.map (characterButton root) characters
        ]



-- Mood section


moodButton : String -> Character.Name -> Int -> Html Msg
moodButton root c n =
    let
        spriteStr =
            spriteNumber root c n
    in
        button
            [ onClick <|
                UpdateDialogs <|
                    DialogBoxes.SetImages c spriteStr
            , style flatButton
            ]
            [ img (style (thumbnail c) :: [ src <| spriteStr ]) [] ]


moodBlank : Html Msg
moodBlank =
    div
        [ style flatButton
        ]
        [ div
            [ style
                [ ( "height", "60px" )
                , ( "width", "60px" )
                ]
            ]
            []
        ]


moodSpace : String -> Character.Name -> Bool -> Int -> Html Msg
moodSpace root c exmode n =
    let
        numMoods =
            (Character.moodCount exmode c)
    in
        if n <= numMoods then
            (moodButton root c n)
        else
            moodBlank


moodButtons : String -> Character.Name -> Bool -> Html Msg
moodButtons root c exmode =
    div
        []
        [ ul
            [ class "moods" ]
          <|
            List.map
                (moodSpace root c exmode)
                (List.range 1 (Character.maxMoods exmode))
        ]


moodSection : String -> Maybe Character.Name -> Bool -> Html Msg
moodSection root maybeChar exmode =
    case maybeChar of
        Nothing ->
            blank

        Just c ->
            moodButtons root c exmode



-- Dialog boxes


crunchyButton : List (Html Msg)
crunchyButton =
    [ div
        [ style [ ( "width", "100%" ) ] ]
        [ Html.button
            [ onClick GetDownload
            , Html.Attributes.id "crunchybutton"
            ]
            [ text "MAKE IT CRUNCHY" ]
        ]
    ]


dialogBoxTexts : Array (Maybe String) -> List String
dialogBoxTexts arr =
    case join (Array.get 0 arr) of
        Nothing ->
            [ "" ]

        Just first ->
            [ first ] ++ takeJusts (Array.slice 1 3 arr)


numBoxes : Array (Maybe String) -> Int
numBoxes texts =
    List.length <| dialogBoxTexts texts


dialogBoxImg : DialogBoxes.Model -> String -> List (Html Msg)
dialogBoxImg boxes pngData =
    let
        boxCount =
            DialogBoxes.countBoxes boxes
    in
        [ Html.a
            []
            [ Html.img
                [ onClick <|
                    UpdateDialogs <|
                        DialogBoxes.UpdateText boxCount <|
                            DialogBoxes.getText boxCount boxes
                , style
                    [ ( "margin", "0 auto" )
                    , ( "display", "block" )
                    ]
                , src pngData
                ]
                []
            ]
        ]


returnedDialogBox : DialogBoxes.Model -> Maybe String -> Maybe (List (Html Msg))
returnedDialogBox boxes imgData =
      imgData |> Maybe.andThen (Just << dialogBoxImg boxes)



-- Buttons for modals


infoButton : String -> Html Msg
infoButton root =
    button
        [ onClick <|
            UpdateModal <|
                Modal.Show (Just <| infoDialog root)
        , style <|
            [ ( "position", "fixed" )
            , ( "bottom", "15px" )
            , ( "left", "20px" )
            ]
                ++ flatButton
        ]
        [ img [ src <| root ++ "images/heart.png" ] [] ]


creditsButton : String -> Html Msg
creditsButton root =
    button
        [ onClick <|
            UpdateModal <|
                Modal.Show (Just <| creditsDialog root)
        , style <|
            [ ( "position", "fixed" )
            , ( "bottom", "10px" )
            , ( "right", "20px" )
            ]
                ++ flatButton
        ]
        [ img [ src <| root ++ "images/creditsbutton.png" ] [] ]



-- Main view


textBoxId : Int -> String
textBoxId n =
    "textBox" ++ (toString n)


dialogBoxSection : Model -> Html Msg
dialogBoxSection model =
    div
        []
    <|
        Maybe.withDefault [ blank ] <|
            Maybe.Extra.or
                (Maybe.map2
                    (++)
                    (returnedDialogBox model.dialogs model.imageData)
                    (Just
                        [ Html.map UpdateImgur <|
                            Imgur.view model.imgur model.staticRoot
                        ]
                    )
                )
                (Just <|
                    List.map
                        (Html.map UpdateDialogs)
                        (DialogBoxes.view model.dialogs)
                        ++ (if DialogBoxes.viewable model.dialogs then
                                crunchyButton
                            else
                                []
                           )
                )


getEXModeValue : Maybe String -> Bool
getEXModeValue s =
    case s of
        Just "EX" ->
            True

        _ ->
            False


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.id "content"
        ]
        [ title model.staticRoot
        , characterButtons model.staticRoot model.characters
        , maybeDivider model.selection
        , moodSection model.staticRoot model.selection model.exmode
        , dialogBoxSection model
        , infoButton model.staticRoot
        , creditsButton model.staticRoot
        , Html.map UpdateModal (Modal.view model.modal)
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )

        EnterCheatCode ks ->
            let
                ( newCheatCode, cheatResult ) =
                    CheatCode.update ks model.cheatCode
            in
                ( { model
                    | cheatCode = newCheatCode
                    , exmode = getEXModeValue cheatResult
                  }
                , Cmd.none
                )

        ActivateEXMode ->
            ( { model
                | exmode = True
              }
            , Cmd.none
            )

        UpdateDialogs msg ->
            let
                ( newBoxes, moveCursor ) =
                    DialogBoxes.update msg model.dialogs
            in
                ( { model
                    | selection =
                        case msg of
                            DialogBoxes.SetImages c s ->
                                Just c

                            _ ->
                                model.selection
                    , dialogs = newBoxes
                    , imageData = Nothing
                  }
                , Focus.focus
                    { elementId = textBoxId newBoxes.focusIndex
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
            , Cmd.none
            )

        GetDownload ->
            let
                ( newDialogs, svgId ) = DialogBoxes.render model.dialogs

            in
                ( { model
                    | dialogs = newDialogs
                  }
                , DialogBoxes.getImg svgId
                )

        GotDownload data ->
            let
                ( newImgur, fx ) =
                    Imgur.update (Imgur.SetImageData data) model.imgur
            in
                ( { model
                    | imageData = Just data
                    , imgur = newImgur
                  }
                , Cmd.none
                )

        UpdateModal msg ->
            ( { model
                | modal = Modal.update msg model.modal
              }
            , Cmd.none
            )

        UpdateImgur msg ->
            let
                ( newImgur, cmd ) =
                    Imgur.update msg model.imgur
            in
                ( { model
                    | imgur = newImgur
                  }
                , Cmd.map UpdateImgur cmd
                )



-- Tasks


getSubmitUrl : String -> String
getSubmitUrl root =
    root ++ "/submit"


getImgurParamsUrl : String -> String
getImgurParamsUrl root =
    root ++ "/imgur_id"


imgurParamsDecoder : Json.Decoder ( String, String )
imgurParamsDecoder =
    Json.map2 (,)
        (Json.field "clientId" Json.string)
        (Json.field "albumId" Json.string)


getImgurParams : String -> Cmd Msg
getImgurParams scriptRoot =
    Http.send (Imgur.SetParams >> UpdateImgur) <|
        Http.get (getImgurParamsUrl scriptRoot) imgurParamsDecoder


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Keyboard.downs EnterCheatCode
        , DialogBoxes.getRenderData GotDownload
        ]



-- Main


type alias Flags =
    { scriptRoot : String
    , staticRoot : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init =
            init Character.allNames
        , update = update
        , view = view
        , subscriptions = subs
        }
