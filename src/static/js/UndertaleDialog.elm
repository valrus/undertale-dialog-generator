module UndertaleDialog (..) where

import StartApp exposing (start)
import Array exposing (Array, toList)
import Character
import Color exposing (grayscale)
import Effects exposing (Effects, Never, none)
import Either exposing (Either)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (image, Element)
import Html exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, src, style)
import Http
import Json.Decode exposing (object2, string, (:=))
import Maybe exposing (Maybe, andThen)
import Maybe.Extra exposing (combine, isJust, join, maybeToList)
import String exposing (lines)
import Task
import Focus
import Helpers exposing (..)
import Imgur
import Modal


-- Could split the map stuff into a different module

import CreditsModal exposing (creditsDialog, mapArea)


-- Model


type alias Model =
    { characters : List Character.Name
    , selection : Maybe Character.Name
    , moodImg : Maybe String
    , text : Array (Maybe String)
    , staticRoot : String
    , scriptRoot : String
    , imageData : Maybe String
    , modal : Modal.Model
    , focusAddress : Signal.Address Focus.Action
    , imgur : Imgur.Model
    }


init : List Character.Name -> Signal.Address Focus.Action -> Model
init characters focusAddress =
    { characters = characters
    , selection = Nothing
    , moodImg = Nothing
    , text = Array.fromList [ Just "", Nothing, Nothing ]
    , staticRoot = "/static/"
    , scriptRoot = ""
    , imageData = Nothing
    , modal = Modal.init <| grayscale 1
    , focusAddress = focusAddress
    , imgur = Imgur.init
    }



-- View
-- General styles


flatButton : List (String, String)
flatButton =
    [ ( "backgroundColor", "transparent" )
    , ( "border", "none" )
    ]


header : Html
header =
    div
        []
        [ hr [ style [ ( "margin-bottom", "30px" ) ] ] [] ]


maybeDivider : Maybe a -> Html
maybeDivider choice =
    case choice of
        Nothing ->
            blank

        Just a ->
            header


blank : Html
blank =
    div [] []


titleImgMap : Signal.Address Action -> Html
titleImgMap address =
    Html.node
        "map"
        [ Html.Attributes.id "titleMap"
        , Html.Attributes.name "titleMap"
        ]
        [ mapArea [ 606, 43, 626, 61 ] "hOI!"
            <| Either.Right
            <| ( address, ChooseCharacter Character.Temmie )
        ]


title root address =
    div
        [ style
            [ ( "padding-top", "100px" )
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
        , titleImgMap address
        ]



-- Character section


spriteFolder : String -> Character.Name -> String
spriteFolder root c =
    root ++ "images/sprites/" ++ toString c


spriteNumber : String -> Character.Name -> Int -> String
spriteNumber root c n =
    (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"


defaultSprite : String -> Character.Name -> String
defaultSprite root c =
    spriteNumber root c 0


characterButton : Signal.Address Action -> String -> Character.Name -> Html
characterButton address staticRoot c =
    case c of
        Character.Temmie ->
            blank

        _ ->
            button
                [ onClick address <| ChooseCharacter c
                , style flatButton
                ]
                [ img [ src <| defaultSprite staticRoot c ] [] ]


characterButtons : Signal.Address Action -> String -> List Character.Name -> Html
characterButtons address root characters =
    div
        []
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
            [ img [ src <| spriteStr ] [] ]


moodButtons : Signal.Address Action -> String -> Character.Name -> Html
moodButtons address root c =
    div
        []
        [ ul
            [ class "moods" ]
            <| List.map (moodButton address root c) [1..(Character.moodCount c)]
        ]


moodSection : Signal.Address Action -> String -> Maybe Character.Name -> Html
moodSection address root maybeChar =
    case maybeChar of
        Nothing ->
            blank

        Just c ->
            moodButtons address root c



-- Dialog box


indentAsterisk : Maybe Character.Name -> Html
indentAsterisk character =
    div
        [ Html.Attributes.id "indent"
        , style <| Character.fontStyles character
        ]
        [ Html.text <| Character.dialogAsterisk character ]


textBox : Signal.Address Action -> Maybe Character.Name -> Int -> String -> Html
textBox address character num text =
    textarea
        [ Html.Attributes.id <| "textBox" ++ (toString (num + 1))
        , on
            "input"
            targetValue
            (\s -> Signal.message address <| UpdateText num s False)
        , style
            <| [ ( "line-height", "36px" )
                 -- TODO: Make the "36px" a function
               ]
            ++ (Character.fontStyles character)
            ++ (Character.textboxStyles character)
        , Html.Attributes.rows 3
        ]
        [ Html.text text ]


dialogCollage : Html -> Signal.Address Action -> Maybe Character.Name -> Int -> String -> Html
dialogCollage e address character num text =
    div
        [ style [ ( "width", "100%" ) ] ]
        [ div
            [ style
                [ ( "width", "596px" )
                , ( "position", "relative" )
                , ( "margin", "0 auto" )
                ]
            ]
            [ div
                [ Html.Attributes.class "dialog" ]
                [ e
                , indentAsterisk character
                , textBox address character num text
                ]
            ]
        ]


dialogFrame : String -> Character.Name -> Html
dialogFrame imgSrc character =
    let
        ( imgX, imgY ) = Character.portraitOffset (Just character)
    in
        (fromElement
            <| collage
                596
                168
                [ filled (grayscale 1) (rect 596 168)
                  -- outer black border
                , filled (grayscale 0) (rect 580 152)
                  -- outer white border
                , filled (grayscale 1) (rect 568 140)
                  -- inner black box
                , (toForm
                    <| doubleImage imgSrc
                    <| Character.portraitSize (Just character)
                  )
                    |> move ( -214 + imgX, imgY )
                ]
        )


crunchyButton : Signal.Address Action -> List Html
crunchyButton address =
    [ div
        [ style [ ( "width", "100%" ) ] ]
        [ Html.button
            [ onClick address <| GetDownload
            , Html.Attributes.id "crunchybutton"
            ]
            [ text "MAKE IT CRUNCHY" ]
        ]
    ]


doubleImage : String -> ( Int, Int ) -> Graphics.Element.Element
doubleImage imgSrc ( w, h ) =
    image (w * 2) (h * 2) imgSrc


dialogBox : Signal.Address Action -> Character.Name -> String -> Int -> String -> Maybe Html
dialogBox address character imgSrc num text =
    Just
        <| dialogCollage
            (dialogFrame imgSrc character)
            address
            (Just character)
            num
            text


dialogBoxTexts : Array (Maybe String) -> List String
dialogBoxTexts arr =
      case join (Array.get 0 arr) of
          Nothing ->
              [ "" ]

          Just first ->
              [ first ] ++ takeJusts (Array.slice 1 3 arr)


dialogStringTexts : Bool -> String -> Array String
dialogStringTexts skipBlanks s =
    let filterFunc =
        if skipBlanks then takeNonEmpty else takeJusts
    in
        Array.fromList <| filterFunc <| Array.fromList <| splitLinesEvery 3 3 s


dialogBoxes : Signal.Address Action -> Model -> Maybe (List Html)
dialogBoxes address model =
    case Maybe.map2 (,) model.selection model.moodImg of
        Nothing ->
            Nothing

        Just ( character, imgSrc ) ->
            combine
                <| List.indexedMap (dialogBox address character imgSrc)
                <| dialogBoxTexts model.text


numBoxes : Array (Maybe String) -> Int
numBoxes texts =
    List.length <| dialogBoxTexts texts


dialogBoxImg : Array (Maybe String) -> Signal.Address Action -> String -> Maybe (List Html)
dialogBoxImg texts address pngData =
    let
        boxCount =
            numBoxes texts
    in
        Just
            <| [ Html.a
                    []
                    [ Html.img
                        [ onClick address
                            <| UpdateText
                                (numBoxes texts)
                                (Maybe.withDefault "" <| join <| Array.get boxCount texts)
                                True
                        , style
                            [ ( "margin", "0 auto" )
                            , ( "display", "block" )
                            ]
                        , src pngData
                        ]
                        []
                    ]
               ]


returnedDialogBox : Array (Maybe String) -> Signal.Address Action -> Maybe String -> Maybe (List Html)
returnedDialogBox texts address imgData =
    Maybe.map2
        (++)
        (Just "data:image/png;base64,")
        imgData
        `andThen` dialogBoxImg texts address



-- Button for credits modal


infoButton : Signal.Address Action -> String -> Html
infoButton address root =
    button
        [ onClick address
            <| UpdateModal
            <| Modal.Show (Just <| creditsDialog root)
        , style
            <| [ ( "position", "fixed" )
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


dialogBoxSection : Signal.Address Action -> Model -> Html
dialogBoxSection address model =
    div
        []
        <| Maybe.withDefault [ blank ]
        <| Maybe.oneOf
            [ Maybe.map2
                (++)
                (returnedDialogBox model.text address model.imageData)
                (Just
                    <| [ Imgur.view (Signal.forwardTo address UpdateImgur) model.imgur
                            <| model.staticRoot
                       ]
                )
            , Maybe.map2
                (++)
                (dialogBoxes address model)
                (Just <| crunchyButton address)
            ]


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


textsToString : Array (Maybe String) -> String
textsToString texts =
    String.join "\n" <| takeJusts texts


textWithUpdate : Int -> String -> Array (Maybe String) -> String
textWithUpdate entryBoxNum newBoxText oldText =
    textsToString
    <| Array.set entryBoxNum (Just newBoxText) oldText


updateText : String -> String -> ( Int, Array (Maybe String) )
updateText oldText newText =
    let
        skipBlanks =
            (String.length newText) < (String.length oldText)
        newTexts =
            dialogStringTexts skipBlanks newText
    in
        ( Array.length <| newTexts
        , Array.map (Just << takeLines 3) newTexts)


type Action
    = NoOp ()
    | ChooseCharacter Character.Name
    | ChooseMood String
    | UpdateText Int String Bool
    | SetScriptRoot String
    | SetStaticRoot String
    | GetDownload
    | GotDownload (Maybe String)
    | UpdateModal Modal.Action
    | UpdateImgur Imgur.Action


update : Action -> Model -> ( Model, Effects Action )
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
                , text = Array.fromList [ Just "" ]
                , imageData = Nothing
              }
            , none
            )

        ChooseMood s ->
            ( { model
                | moodImg = Just s
                , imageData = Nothing
              }
            , toFocusEffect
                model.focusAddress
                { elementId = textBoxId 1
                , moveCursorToEnd = False
                }
            )

        UpdateText boxNum s moveCursor ->
            let
                ( boxCount, newText ) =
                    updateText
                    (textsToString model.text)
                    (textWithUpdate boxNum s model.text)
            in
                ( { model
                    | text = newText
                    , imageData = Nothing
                  }
                , toFocusEffect
                    model.focusAddress
                    { elementId = textBoxId boxCount
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
            let
                ( newImgur, fx ) = Imgur.update (Imgur.SetImageData data) model.imgur
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
            let
                ( newImgur, fx ) = Imgur.update action model.imgur
            in
                ( { model
                    | imgur = newImgur
                  }
                , Effects.map UpdateImgur fx
                )



-- Tasks


getSubmitUrl : String -> String
getSubmitUrl root =
    root ++ "/submit"


getDialogBoxImg : Model -> Effects Action
getDialogBoxImg model =
    case Maybe.map2 (,) model.selection model.moodImg of
        Nothing ->
            none

        Just ( c, img ) ->
            Http.url
                (getSubmitUrl model.scriptRoot)
                [ ( "character", toString c )
                , ( "moodImg", img )
                , ( "text", String.join "\n" <| takeJusts model.text )
                ]
                |> Http.getString
                |> Task.toMaybe
                |> Task.map GotDownload
                |> Effects.task


getImgurParamsUrl : String -> String
getImgurParamsUrl root =
    root ++ "/imgur_id"


imgurParamsDecoder : Json.Decode.Decoder ( String, String )
imgurParamsDecoder =
    object2 (,) ("clientId" := string) ("albumId" := string)


getImgurParams : String -> Effects Action
getImgurParams scriptRoot =
    getImgurParamsUrl scriptRoot
        |> Http.get imgurParamsDecoder
        |> Task.toMaybe
        |> Task.map (\ms -> UpdateImgur <| Imgur.SetParams ms)
        |> Effects.task


toFocusEffect : Signal.Address Focus.Action -> Focus.Params -> Effects Action
toFocusEffect address params =
    Signal.send address (Focus.Focus params) |> Task.map NoOp |> Effects.task



-- Main


app : StartApp.App Model
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
                toFocusMailbox.address
            , none
            )
        , update = update
        , view = view
        , inputs =
            [ Signal.map SetScriptRoot scriptRoot
            , Signal.map SetStaticRoot staticRoot
            ]
        }


main : Signal Html
main =
    app.html



-- Interop


port scriptRoot : Signal String
port staticRoot : Signal String
port focus : Signal Focus.Params
port focus =
    Signal.filterMap Focus.focusFilter Focus.emptyParams toFocusMailbox.signal


toFocusMailbox =
    Signal.mailbox Focus.NoOp


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks
