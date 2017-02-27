module DialogBox exposing (..)

import Char
import Json.Decode as Json
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Svg.Events exposing (onClick)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (on, onInput, targetValue, keyCode)
import Maybe
import Helpers exposing (Position)

-- Local modules

import Character
import Helpers exposing (takeLines)


-- Individual boxes


type alias Model =
    { chara : Maybe Character.Name
    , imgSrc : Maybe String
    , text : Maybe String
    , index : Int
    , expectingImage : Bool
    }


type alias FullModel =
    { chara : Character.Name
    , imgSrc : String
    , text : String
    , index : Int
    , expectingImage : Bool
    }


init : Maybe String -> Int -> Model
init s i =
    { chara = Nothing
    , imgSrc = Nothing
    , text = s
    , index = i
    , expectingImage = False
    }



-- View


indentAsterisk : Character.Name -> Html Msg
indentAsterisk character =
    div
        [ HtmlAttr.id "indent"
        , HtmlAttr.style <| Character.fontStyles character
        ]
        [ Html.text <| Character.dialogAsterisk character ]


deleteEmptyBox : String -> Char.KeyCode -> Msg
deleteEmptyBox text keyCode =
    case keyCode of
        8 ->
            if text == "" then
                SetText Nothing
            else
                (SetText (Just text))

        _ ->
            SetText (Just text)


textBox : FullModel -> Html Msg
textBox model =
    Html.textarea
        [ HtmlAttr.id <| "textBox" ++ (toString model.index)
        , onInput
            (Just >> SetText)
        , on
            "keyDown"
            (Json.map (deleteEmptyBox model.text) keyCode)
        , HtmlAttr.style <|
            [ ( "line-height", "36px" )
              -- TODO: Make the "36px" a function
            ]
                ++ (Character.fontStyles model.chara)
                ++ (Character.textboxStyles model.chara)
        , HtmlAttr.rows 3
        , HtmlAttr.value (takeLines 3 model.text)
        ]
        []


dialogCollage : Html Msg -> FullModel -> Html Msg
dialogCollage elem model =
    div
        [ HtmlAttr.style [ ( "width", "100%" ) ] ]
        [ div
            [ HtmlAttr.style
                [ ( "width", "596px" )
                , ( "position", "relative" )
                , ( "margin", "0 auto" )
                ]
            ]
            [ div
                [ HtmlAttr.class "dialog" ]
                [ elem
                , indentAsterisk model.chara
                , textBox model
                ]
            ]
        ]


svgPosition : Position -> List (Svg.Attribute Msg)
svgPosition pos =
    [ SvgAttr.x (toString pos.x)
    , SvgAttr.y (toString pos.y)
    , SvgAttr.width (toString pos.w)
    , SvgAttr.height (toString pos.h)
    ]


portraitAlpha : Bool -> String
portraitAlpha dim =
    if dim then "0.5" else "1"


portraitButton : Position -> FullModel -> Html Msg
portraitButton pos model =
    Svg.image
        ([ SvgAttr.xlinkHref model.imgSrc
         , SvgAttr.opacity <| portraitAlpha model.expectingImage
         , onClick (ExpectImage <| not model.expectingImage)
         ]
            ++ (svgPosition pos)
        )
        []


svgBorder : Position -> String -> Svg Msg
svgBorder pos color =
    Svg.rect
        ([ SvgAttr.fill color ] ++ (svgPosition pos))
        []


-- TODO: Is it possible to crispify text client-side?
-- http://stackoverflow.com/questions/35434315/how-to-get-crispedges-for-svg-text
-- Use filter="url(#crispify)" on an svg text node
filterDefs : Svg Msg
filterDefs =
    Svg.defs []
        [ Svg.filter
              [ SvgAttr.id "crispify" ]
              [ Svg.node "feComponentTransfer" []
                    [ Svg.feFuncA
                          [ SvgAttr.type_ "discrete"
                          , SvgAttr.tableValues "0 1"
                          ]
                          []
                    ]
              ]
        ]


dialogFrame : FullModel -> Html Msg
dialogFrame model =
    let
        ( imgX, imgY ) =
            Character.portraitOffset model.chara

        ( sizeX, sizeY ) =
            Character.portraitSize model.chara
    in
        Svg.svg
            [ SvgAttr.width (toString 596)
            , SvgAttr.height (toString 168)
            ]
            [ filterDefs
            , svgBorder (Position 0 0 596 168) "black"
            , svgBorder (Position 8 8 580 152) "white"
            , svgBorder (Position 14 14 568 140) "black"
            , portraitButton
                (Position
                    (298 - 214 + imgX - sizeX)
                    (84 + imgY - sizeY)
                    (sizeX * 2)
                    (sizeY * 2)
                )
                model
            ]


certifyModel : Model -> Maybe FullModel
certifyModel model =
    case Maybe.map3 (,,) model.chara model.imgSrc model.text of
        Nothing ->
            Nothing

        Just ( chara, src, txt ) ->
            Just
                { chara = chara
                , imgSrc = src
                , text = txt
                , index = model.index
                , expectingImage = model.expectingImage
                }


view : Model -> Html Msg
view model =
    case certifyModel model of
        Nothing ->
            div [ HtmlAttr.class ("emptyDialog" ++ toString model.index) ] []

        Just fullModel ->
            dialogCollage
                (dialogFrame fullModel)
                fullModel


updateField : Maybe a -> Maybe a -> Bool -> Maybe a
updateField old new wantToSet =
    if ((old == Nothing) || wantToSet) then
        new
    else
        old


type Msg
    = NoOp
    | SetImage Character.Name (Maybe String) Bool
    | SetText (Maybe String)
    | ExpectImage Bool


update : Msg -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        SetImage chara src force ->
            let
                wantToSet =
                    model.expectingImage || force
            in
                { model
                    | imgSrc =
                        updateField model.imgSrc src wantToSet
                    , chara = updateField model.chara (Just chara) wantToSet
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
