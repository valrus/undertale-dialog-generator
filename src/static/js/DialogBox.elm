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

import Debug exposing (log)


-- Local modules

import Character exposing (RenderOverride, spriteNumber)
import Helpers exposing (Position, offset)
import Helpers exposing (takeLines)
import TextCleaning exposing (unicodeSanitizer, TextReplacement)
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
        [ HtmlAttr.id "indent"
        , HtmlAttr.style <| Character.fontStyles character
        ]
        [ Html.text <| Character.dialogAsterisk 0 character ]


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


fontFaceStyle : ( String, String, String ) -> String
fontFaceStyle ( name, woff2, woff ) =
    String.join "\n"
    [ "@font-face {"
    , (String.join ""
           [ "font-family: '"
           , name
           , "';"
           ]
      )
    , (String.join ""
           [ "src: url('data:application/font-woff2;base64,"
           , woff2
           , "') format('woff2'),"
           ]
      )
    , (String.join ""
           [ "     url('data:application/x-font-woff;base64,"
           , woff
           , "') format('woff');"
           ]
      )
    , "}"
    ]


fontFaceStyles : String
fontFaceStyles =
    -- "<![CDATA[\n"
    String.join "\n" <| List.map fontFaceStyle allFonts
    -- ++ "\n]]>"


filterDefs : Svg.Svg Msg
filterDefs =
    Svg.defs []
        [ Svg.filter
            [ SvgAttr.id "crispify" ]
            [ Svg.node "feComponentTransfer"
                []
                [ Svg.feFuncA
                    [ SvgAttr.type_ "discrete"
                    , SvgAttr.tableValues "0 1"
                    ]
                    []
                ]
            ]
        , Svg.style
            [ SvgAttr.type_ "text/css" ]
            [ Svg.text <| fontFaceStyles ]
        ]


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
            (Character.textboxLeft model.chara)
                :: (Character.textboxWidth model.chara)
                :: [ ( "line-height", "36px" )
                     -- TODO: Make the "36px" a function
                   ]
                ++ (Character.fontStyles model.chara)
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
                [ ( "width", (toString boxWidth) ++ "px" )
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
    if dim then
        "0.5"
    else
        "1"


portraitButton : Position -> String -> Bool -> Html Msg
portraitButton pos src expectingImage =
    Svg.image
        ([ SvgAttr.xlinkHref src
         , SvgAttr.opacity <| portraitAlpha expectingImage
         , onClick (ExpectImage <| not expectingImage)
         -- , SvgAttr.filter "url(#crispify)"
         ]
            ++ (svgPosition pos)
        )
        []


svgBorder : Position -> String -> Svg Msg
svgBorder pos color =
    Svg.rect
        ([ SvgAttr.fill color ] ++ (svgPosition pos))
        []


boxHeight : Int -> Int
boxHeight num =
    168 * num


boxWidth : Int
boxWidth =
    596


singleBox : Int -> Int -> FullModel -> List (Html Msg)
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


dialogFrame : FullModel -> Html Msg
dialogFrame model =
    Svg.svg
        [ SvgAttr.width (toString boxWidth)
        , SvgAttr.height (toString (boxHeight 1))
        ]
    <|
        [ filterDefs ]
            ++ (singleBox 0 0 model)


getOverrideImgSrc : String -> Character.Name -> RenderOverride -> ( String, String )
getOverrideImgSrc root chara override =
    let
        ( text, imgNum ) = override
    in
        ( text, spriteNumber root chara imgNum )


applyOverrides : String -> Character.Name -> String -> String -> ( String, String )
applyOverrides imgRoot chara src txt =
    let
        sanitizedTxt =
            String.map unicodeSanitizer txt
    in
        case TextCleaning.matchHelper chara txt of
            TextCleaning.HasCusses ->
                getOverrideImgSrc imgRoot chara <| Character.cussParams chara

            TextCleaning.UnknownLanguage ->
                getOverrideImgSrc imgRoot chara <| Character.languageParams chara

            TextCleaning.TooLong ->
                ( txt, src )

            TextCleaning.NoMatch ->
                ( txt, src )


certifyModel : Bool -> Model -> Maybe FullModel
certifyModel override model =
    case Maybe.map3 (,,) model.chara model.imgSrc model.text of
        Nothing ->
            Nothing

        Just ( chara, src, txt ) ->
            let
                ( overrideText, overrideSrc ) =
                    if override then applyOverrides model.imgRoot chara src txt
                    else ( txt, src )
            in
                Just
                    { chara = chara
                    , imgSrc = overrideSrc
                    , text = overrideText
                    , index = model.index
                    , expectingImage = model.expectingImage
                    }


view : Model -> Html Msg
view model =
    case certifyModel False model of
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
