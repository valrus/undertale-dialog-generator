module UndertaleDialog exposing (..)

-- Local modules

import Array exposing (Array)
import Browser exposing (element)
import Browser.Events
import Character exposing (defaultSprite, spriteNumber, thumbnail)
import CheatCode
import Color exposing (rgb255)
import CreditsModal exposing (creditsDialog)
import DialogBoxes
import Either
import Focus
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, src, style)
import Html.Events exposing (onClick)
import ImageMap exposing (mapArea)
import InfoModal exposing (infoDialog)
import Json.Decode as Json
import Maybe exposing (Maybe)
import Maybe.Extra exposing (join)
import Modal



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
    , exmode : Bool
    }


init : List Character.Name -> Flags -> ( Model, Cmd Msg )
init characters flags =
    ( { characters = characters
      , selection = Nothing
      , dialogs = DialogBoxes.init flags.staticRoot
      , staticRoot = flags.staticRoot
      , scriptRoot = flags.scriptRoot
      , imageData = Nothing
      , modal = Modal.init (rgb255 0 0 0)
      , cheatCode = CheatCode.init [ "EX" ]
      , exmode = False
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | EnterCheatCode KeyCode
    | ActivateEXMode
    | UpdateDialogs DialogBoxes.Msg
    | GetDownload
    | GotDownload String
    | UpdateModal Modal.Msg



-- View
-- General styles


flatButton : StyleList msg
flatButton =
    [ style "backgroundColor" "transparent"
    , style "border" "none"
    , style "display" "inline-block"
    ]


header : Html Msg
header =
    div
        []
        [ hr [ style "margin-bottom" "30px" ] [] ]


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
        [ style "padding-top" "60px"
        , style "padding-bottom" "30px"
        , style "display" "block"
        ]
        [ img
            [ style "margin" "0 auto"
            , style "display" "block"
            , style "width" "672px"
            , src <| root ++ "images/title.png"
            , Html.Attributes.usemap "#titleMap"
            ]
            []
        , titleImgMap root
        ]



-- Character section


characterButton : String -> Character.Name -> Html Msg
characterButton staticRoot c =
    case c of
        Character.Temmie ->
            blank

        _ ->
            button
                ((onClick <|
                    UpdateDialogs <|
                        DialogBoxes.SetImages c <|
                            defaultSprite staticRoot c False
                 )
                    :: flatButton
                )
                [ img
                    -- Use Toriel here to avoid the Napstablook special case
                    ((src <| defaultSprite staticRoot c True)
                        :: thumbnail Character.Toriel
                    )
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
        ((onClick <|
            UpdateDialogs <|
                DialogBoxes.SetImages c spriteStr
         )
            :: flatButton
        )
        [ img ((src <| spriteStr) :: thumbnail c) [] ]


moodBlank : Html Msg
moodBlank =
    div
        flatButton
        [ div
            [ style "height" "60px"
            , style "width" "60px"
            ]
            []
        ]


moodSpace : String -> Character.Name -> Bool -> Int -> Html Msg
moodSpace root c exmode n =
    let
        numMoods =
            Character.moodCount exmode c
    in
    if n <= numMoods then
        moodButton root c n

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
        [ style "width" "100%" ]
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
            first :: takeJusts (Array.slice 1 3 arr)


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
            , style "margin" "0 auto"
            , style "display" "block"
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
        ([ onClick <|
            UpdateModal <|
                Modal.Show (Just <| infoDialog root)
         , style "position" "fixed"
         , style "bottom" "15px"
         , style "left" "20px"
         ]
            ++ flatButton
        )
        [ img [ src <| root ++ "images/heart.png" ] [] ]


creditsButton : String -> Html Msg
creditsButton root =
    button
        ([ onClick <|
            UpdateModal <|
                Modal.Show (Just <| creditsDialog root)
         , style "position" "fixed"
         , style "bottom" "10px"
         , style "right" "20px"
         ]
            ++ flatButton
        )
        [ img [ src <| root ++ "images/creditsbutton.png" ] [] ]



-- Main view


textBoxId : Int -> String
textBoxId n =
    "textBox" ++ String.fromInt n


dialogBoxSection : Model -> Html Msg
dialogBoxSection model =
    div
        []
    <|
        Maybe.withDefault [ blank ] <|
            Maybe.Extra.or
                (returnedDialogBox model.dialogs model.imageData)
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
        (Html.Attributes.id "content"
            :: stylesFromArgs crispyFontStyleArgs
        )
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

        UpdateDialogs dialogMsg ->
            let
                ( newBoxes, moveCursor ) =
                    DialogBoxes.update dialogMsg model.dialogs
            in
            ( { model
                | selection =
                    case dialogMsg of
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

        GetDownload ->
            let
                ( newDialogs, svgId ) =
                    DialogBoxes.render model.dialogs
            in
            ( { model
                | dialogs = newDialogs
              }
            , DialogBoxes.getImg svgId
            )

        GotDownload data ->
            ( { model
                | imageData = Just data
              }
            , Cmd.none
            )

        UpdateModal modalMsg ->
            ( { model
                | modal = Modal.update modalMsg model.modal
              }
            , Cmd.none
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
    Json.map2 Tuple.pair
        (Json.field "clientId" Json.string)
        (Json.field "albumId" Json.string)


toCheatCodeMsg : String -> Msg
toCheatCodeMsg s =
    case String.uncons s of
        Just ( char, "" ) ->
            EnterCheatCode <| Char.toCode char

        _ ->
            NoOp


cheatCodeDecoder : Json.Decoder Msg
cheatCodeDecoder =
    Json.map toCheatCodeMsg (Json.field "key" Json.string)


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onKeyDown cheatCodeDecoder
        , DialogBoxes.getRenderData GotDownload
        ]



-- Main


type alias Flags =
    { scriptRoot : String
    , staticRoot : String
    }


main : Program Flags Model Msg
main =
    element
        { init =
            init Character.allNames
        , update = update
        , view = view
        , subscriptions = subs
        }
