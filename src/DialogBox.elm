module DialogBox exposing (..)

import Character exposing (RenderOverride, spriteNumber)
import Debug exposing (log)
import Helpers exposing (KeyCode, Position, offset, takeLines)
import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Html.Events exposing (keyCode, on, onInput)
import Json.Decode as Json
import Maybe
import Svg.String exposing (Svg)
import Svg.String.Attributes as SvgAttr
import Svg.String.Events exposing (onClick)
import TextCleaning
import UndertaleFonts exposing (allFonts)



-- Individual boxes


type alias Model =
    { chara : Maybe Character.Name
    , imgRoot : String
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


init : String -> Maybe String -> Int -> Model
init root txt i =
    { chara = Nothing
    , imgRoot = root
    , imgSrc = Nothing
    , text = txt
    , index = i
    , expectingImage = False
    }



-- View


indentAsterisk : Character.Name -> Html Msg
indentAsterisk character =
    div
        (HtmlAttr.id "indent"
            :: Character.fontStyles character
        )
        [ Html.text <| Character.dialogAsterisk 0 character ]


deleteEmptyBox : String -> KeyCode -> Msg
deleteEmptyBox text keyCode =
    case keyCode of
        8 ->
            if text == "" then
                SetText Nothing

            else
                SetText (Just text)

        _ ->
            SetText (Just text)


fontFaceStyle : ( String, String, String ) -> String
fontFaceStyle ( name, woff2, woff ) =
    String.join "\n"
        [ "@font-face {"
        , String.join ""
            [ "font-family: '"
            , name
            , "';"
            ]
        , String.join ""
            [ "src: url('data:application/font-woff2;base64,"
            , woff2
            , "') format('woff2'),"
            ]
        , String.join ""
            [ "     url('data:application/x-font-woff;base64,"
            , woff
            , "') format('woff');"
            ]
        , "}"
        ]


fontFaceStyles : String
fontFaceStyles =
    -- "<![CDATA[\n"
    String.join "\n" <| List.map fontFaceStyle allFonts



-- ++ "\n]]>"


filterDefs : Svg Msg
filterDefs =
    Svg.String.defs []
        [ Svg.String.filter
            [ SvgAttr.id "crispify" ]
            [ Svg.String.node "feComponentTransfer"
                []
                [ Svg.String.node "feFuncA"
                    [ SvgAttr.type_ "discrete"
                    , SvgAttr.attribute "table-values" "0 1"
                    ]
                    []
                ]
            ]
        , Svg.String.node "style"
            [ SvgAttr.type_ "text/css" ]
            [ Svg.String.text <| fontFaceStyles ]
        ]


textBox : FullModel -> Html Msg
textBox model =
    Html.textarea
        ([ HtmlAttr.id <| "textBox" ++ String.fromInt model.index
         , onInput
            (Just >> SetText)
         , on
            "keyDown"
            (Json.map (deleteEmptyBox model.text) keyCode)
         , Character.textboxLeft model.chara
         , Character.textboxWidth model.chara
         , HtmlAttr.style "line-height" "36px"

         -- TODO: Make the "36px" a function
         , HtmlAttr.rows 3
         , HtmlAttr.value (takeLines 3 model.text)
         ]
            ++ Character.fontStyles model.chara
        )
        []


dialogCollage : Html Msg -> FullModel -> Html Msg
dialogCollage elem model =
    div
        [ HtmlAttr.style "width" "100%" ]
        [ div
            [ HtmlAttr.style "width" (String.fromInt boxWidth ++ "px")
            , HtmlAttr.style "position" "relative"
            , HtmlAttr.style "margin" "0 auto"
            ]
            [ div
                [ HtmlAttr.class "dialog" ]
                [ elem
                , indentAsterisk model.chara
                , textBox model
                ]
            ]
        ]


svgPosition : Position -> List (Svg.String.Attribute Msg)
svgPosition pos =
    [ SvgAttr.x (String.fromInt pos.x)
    , SvgAttr.y (String.fromInt pos.y)
    , SvgAttr.width (String.fromInt pos.w)
    , SvgAttr.height (String.fromInt pos.h)
    ]


portraitAlpha : Bool -> String
portraitAlpha dim =
    if dim then
        "0.5"

    else
        "1"


portraitButton : Position -> String -> Bool -> Svg Msg
portraitButton pos src expectingImage =
    Svg.String.node "image"
        ([ SvgAttr.attribute "xlink:href" src
         , SvgAttr.attribute "opacity" <| portraitAlpha expectingImage
         , onClick (ExpectImage <| not expectingImage)
         , SvgAttr.filter "url(#crispify)"
         ]
            ++ svgPosition pos
        )
        []


svgBorder : Position -> String -> Svg Msg
svgBorder pos color =
    Svg.String.rect
        (SvgAttr.fill color :: svgPosition pos)
        []


boxHeight : Int -> Int
boxHeight num =
    168 * num


boxWidth : Int
boxWidth =
    596


singleBox : Int -> Int -> FullModel -> List (Svg Msg)
singleBox x y model =
    let
        ( imgX, imgY ) =
            Character.portraitOffset model.chara

        ( sizeX, sizeY ) =
            Character.portraitSize model.chara

        move =
            offset x y
    in
    [ svgBorder (Position 0 0 boxWidth (boxHeight 1) |> move) "black"
    , svgBorder (Position 8 8 580 152 |> move) "white"
    , svgBorder (Position 14 14 568 140 |> move) "black"
    , portraitButton
        (Position
            (298 - 214 + imgX - sizeX)
            (84 + imgY - sizeY)
            (sizeX * 2)
            (sizeY * 2)
            |> move
        )
        model.imgSrc
        model.expectingImage
    ]


dialogFrame : FullModel -> Svg.String.Html Msg
dialogFrame model =
    Svg.String.svg
        [ SvgAttr.width (String.fromInt boxWidth)
        , SvgAttr.height (String.fromInt (boxHeight 1))
        ]
    <|
        filterDefs
            :: singleBox 0 0 model


getOverrideImgSrc : String -> Character.Name -> RenderOverride -> ( String, String )
getOverrideImgSrc root chara override =
    let
        ( text, imgNum ) =
            override
    in
    ( text, spriteNumber root chara imgNum )


applyOverrides : String -> Character.Name -> String -> String -> ( String, String )
applyOverrides imgRoot chara src txt =
    case TextCleaning.matchHelper chara txt of
        TextCleaning.HasCusses ->
            getOverrideImgSrc imgRoot chara <| Character.cussParams chara

        TextCleaning.UnknownLanguage ->
            getOverrideImgSrc imgRoot chara <| Character.languageParams chara

        TextCleaning.TooLong ->
            ( txt, src )

        TextCleaning.NoMatch ->
            ( txt, src )


toFullModel : Bool -> Model -> Character.Name -> String -> String -> FullModel
toFullModel override model chara src txt =
    let
        ( overrideText, overrideSrc ) =
            if override then
                applyOverrides model.imgRoot chara src txt

            else
                ( txt, src )
    in
    { chara = chara
    , imgSrc = overrideSrc
    , text = overrideText
    , index = model.index
    , expectingImage = model.expectingImage
    }


certifyModel : Bool -> Model -> Maybe FullModel
certifyModel override model =
    Maybe.map3 (toFullModel override model) model.chara model.imgSrc model.text


view : Model -> Html Msg
view model =
    case certifyModel False model of
        Nothing ->
            div [ HtmlAttr.class ("emptyDialog" ++ String.fromInt model.index) ] []

        Just fullModel ->
            dialogCollage
                (Svg.String.toHtml <| dialogFrame fullModel)
                fullModel


updateField : Maybe a -> Maybe a -> Bool -> Maybe a
updateField old new wantToSet =
    if (old == Nothing) || wantToSet then
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
