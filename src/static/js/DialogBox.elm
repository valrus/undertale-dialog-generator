module DialogBox (..) where

import Debug exposing (log)
import Color exposing (grayscale)
import Graphics.Collage exposing (collage, move, filled, rect, toForm)
import Graphics.Element exposing (image)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (on, targetValue, onKeyDown)
import Maybe


-- Local modules

import Character
import Helpers exposing (takeLines)


-- Individual boxes


type alias Model =
    { imgSrc : Maybe String
    , text : Maybe String
    , index : Int
    }


type alias FullModel =
    { imgSrc : String
    , text : String
    , index : Int
    }


init : Maybe String -> Int -> Model
init s i =
    { imgSrc = Nothing
    , text = s
    , index = i
    }


-- View


indentAsterisk : Character.Name -> Html
indentAsterisk character =
    div
        [ Html.Attributes.id "indent"
        , style <| Character.fontStyles character
        ]
        [ Html.text <| Character.dialogAsterisk character ]


deleteEmptyBox : String -> Int -> Maybe String
deleteEmptyBox text keyCode =
    case (log "keycode" keyCode) of
      8 -> if text == "" then (log "delete" Nothing) else Just text

      _ -> Just (log "not delete?" text)


textBox : Signal.Address (Maybe String) -> FullModel -> Character.Name -> Html
textBox address model chara =
    textarea
        [ Html.Attributes.id <| "textBox" ++ (toString model.index)
        , on
            "input"
            targetValue
            (\s -> Signal.message address (Just s))
        , onKeyDown
            address
            (deleteEmptyBox model.text)
        , style
            <| [ ( "line-height", "36px" )
                 -- TODO: Make the "36px" a function
               ]
            ++ (Character.fontStyles chara)
            ++ (Character.textboxStyles chara)
        , Html.Attributes.rows 3
        , Html.Attributes.value (takeLines 3 model.text)
        ]
        []


dialogCollage : Html -> Signal.Address (Maybe String) -> FullModel -> Character.Name -> Html
dialogCollage elem address model chara =
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
                [ elem
                , indentAsterisk chara
                , textBox address model chara
                ]
            ]
        ]


dialogFrame : FullModel -> Character.Name -> Html
dialogFrame model chara =
    let
        ( imgX, imgY ) = Character.portraitOffset chara
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
                    <| doubleImage model.imgSrc
                    <| Character.portraitSize chara
                  )
                    |> move ( -214 + imgX, imgY )
                ]
        )


doubleImage : String -> ( Int, Int ) -> Graphics.Element.Element
doubleImage imgSrc ( w, h ) =
    image (w * 2) (h * 2) imgSrc


certifyModel : Model -> Maybe FullModel
certifyModel model =
    case Maybe.map2 (,) model.imgSrc model.text of
        Nothing ->
            Nothing

        Just ( src, txt ) ->
            Just
                { imgSrc = src
                , text = txt
                , index = model.index
                }


view : Signal.Address (Maybe String) -> Character.Name -> Model -> Html
view address chara model =
    case certifyModel model of
        Nothing ->
            div [ Html.Attributes.class ("emptyDialog" ++ toString model.index) ] []

        Just fullModel ->
            dialogCollage
                (dialogFrame fullModel chara)
                address
                fullModel
                chara


type Action
    = SetImage String
    | SetText (Maybe String)


update : Action -> Model -> Model
update action model =
    case action of
        SetImage src ->
            { model
              | imgSrc = Just src
            }

        SetText text ->
            { model
              | text = text
            }
