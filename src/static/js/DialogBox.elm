module DialogBox (..) where

import Color exposing (grayscale)
import Graphics.Collage exposing (collage, move, filled, rect, toForm, alpha)
import Graphics.Element exposing (Element, image)
import Graphics.Input exposing (customButton)
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
    , expectingImage : Bool
    }


type alias FullModel =
    { imgSrc : String
    , text : String
    , index : Int
    , expectingImage : Bool
    }


init : Maybe String -> Int -> Model
init s i =
    { imgSrc = Nothing
    , text = s
    , index = i
    , expectingImage = False
    }



-- View


indentAsterisk : Character.Name -> Html
indentAsterisk character =
    div
        [ Html.Attributes.id "indent"
        , style <| Character.fontStyles character
        ]
        [ Html.text <| Character.dialogAsterisk character ]


deleteEmptyBox : String -> Int -> Action
deleteEmptyBox text keyCode =
    case keyCode of
        8 ->
            if text == "" then
                SetText Nothing
            else
                (SetText (Just text))

        _ ->
            SetText (Just text)


textBox : Signal.Address Action -> FullModel -> Character.Name -> Html
textBox address model chara =
    textarea
        [ Html.Attributes.id <| "textBox" ++ (toString model.index)
        , on
            "input"
            targetValue
            (\s -> Signal.message address <| SetText <| Just s)
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


dialogCollage : Html -> Signal.Address Action -> FullModel -> Character.Name -> Html
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


portraitButton : Signal.Address Action -> String -> Character.Name -> Element
portraitButton address src chara =
    let
        img = doubleImage src (Character.portraitSize chara)
    in
        customButton
            (Signal.message address (ExpectImage True))
            img
            img
            img


dialogFrame : Signal.Address Action -> FullModel -> Character.Name -> Html
dialogFrame address model chara =
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
                , (toForm <| portraitButton address model.imgSrc chara)
                    |> alpha
                        (if model.expectingImage then
                            0.5
                         else
                            1
                        )
                    |> move ( -214 + imgX, imgY )
                ]
        )


doubleImage : String -> ( Int, Int ) -> Element
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
                , expectingImage = model.expectingImage
                }


view : Signal.Address Action -> Character.Name -> Model -> Html
view address chara model =
    case certifyModel model of
        Nothing ->
            div [ Html.Attributes.class ("emptyDialog" ++ toString model.index) ] []

        Just fullModel ->
            dialogCollage
                (dialogFrame address fullModel chara)
                address
                fullModel
                chara


updateSrc : Maybe String -> Maybe String -> Bool -> Maybe String
updateSrc old new expecting =
    if ((old == Nothing) || (new == Nothing) || expecting) then
        new
    else
        old


type Action
    = NoOp
    | SetImage (Maybe String)
    | SetText (Maybe String)
    | ExpectImage Bool


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        SetImage src ->
            { model
                | imgSrc =
                    updateSrc model.imgSrc src model.expectingImage
                , expectingImage = False
            }

        SetText text ->
            { model
                | text = text
            }

        ExpectImage b ->
            { model
                | expectingImage = b
            }
