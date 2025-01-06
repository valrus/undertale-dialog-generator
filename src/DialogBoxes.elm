module DialogBoxes exposing (..)

-- Local modules

import Array exposing (Array, fromList, toList)
import Bytes exposing (Bytes)
import Character
import Debug exposing (log)
import DialogBox
import Helpers exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http exposing (Response)
import Maybe.Extra exposing (isJust, join)
import String
import Svg.String exposing (Svg)
import Svg.String.Attributes as SvgAttr
import Svg.String.Events as SvgEvents



-- Helpers for multiple boxes


type alias Model =
    { boxes : Array DialogBox.Model
    , focusIndex : Int
    , render : Maybe (Svg Msg)
    }


initBoxes : String -> Array DialogBox.Model
initBoxes imgRoot =
    let
        initBoxWithRoot =
            DialogBox.init imgRoot
    in
    fromList
        [ initBoxWithRoot (Just "") 1
        , initBoxWithRoot Nothing 2
        , initBoxWithRoot Nothing 3
        ]


init : String -> Model
init imgRoot =
    { boxes = initBoxes imgRoot
    , focusIndex = 0
    , render = Nothing
    }


renderedSvgId : String
renderedSvgId =
    "renderedBox"


countBoxes : Model -> Int
countBoxes model =
    count model.boxes


count : Array DialogBox.Model -> Int
count boxes =
    Array.length <| Array.filter (isJust << .text) boxes


concat : Model -> String
concat model =
    String.join "\n" <| takeJusts <| Array.map .text model.boxes


getText : Int -> Model -> Maybe String
getText i model =
    Array.get i model.boxes |> Maybe.andThen .text


getTexts : Model -> Array (Maybe String)
getTexts model =
    Array.map .text model.boxes


getImgSrcs : Model -> List String
getImgSrcs model =
    takeJusts <| Array.map .imgSrc model.boxes


viewable : Model -> Bool
viewable model =
    List.any isJust (toList <| Array.map (DialogBox.certifyModel False) model.boxes)



-- View


convertViewMessage : Int -> DialogBox.Msg -> Msg
convertViewMessage boxNum boxMsg =
    case boxMsg of
        DialogBox.NoOp ->
            Unrender

        DialogBox.SetImage c s force ->
            case s of
                Nothing ->
                    Unrender

                Just src ->
                    SetImages c src

        DialogBox.SetData s ->
            SetImageData boxNum s

        DialogBox.SetText s ->
            UpdateText boxNum s

        DialogBox.ExpectImage b ->
            ExpectImage boxNum b


mapBoxView : Int -> DialogBox.Model -> Html Msg
mapBoxView i box =
    Html.map (convertViewMessage i) (DialogBox.view box)



-- Crispy rendering


renderBox : Int -> DialogBox.Model -> Svg Msg
renderBox i box =
    case DialogBox.certifyModel True box of
        Nothing ->
            Svg.String.g [] []

        Just fullModel ->
            Svg.String.g
                []
            <|
                List.map (Svg.String.map <| convertViewMessage i) <|
                    DialogBox.singleBox
                        0
                        (DialogBox.boxHeight i)
                        fullModel


textLineOffset : Int -> Int -> Character.Name -> Int
textLineOffset offset lineNum chara =
    DialogBox.boxHeight offset + 32 + (36 * lineNum) + Character.yOffset chara


renderTextLine : Character.Name -> Int -> Int -> String -> Svg Msg
renderTextLine chara offset lineNum text =
    let
        attrs =
            [ SvgAttr.y <| String.fromInt (textLineOffset offset lineNum chara)
            , SvgAttr.attribute "dominant-baseline" "text-before-edge"
            ]
    in
    Svg.String.g
        [ SvgAttr.attribute "text-anchor" "start"
        , SvgAttr.attribute "xml-space" "preserve"
        , SvgAttr.fill "white"
        , SvgAttr.filter "url(#crispify)"
        , SvgAttr.style <| styleCss (Character.fontStyleArgs chara ++ crispyFontStyleArgs)
        ]
        [ Svg.String.text_
            ((SvgAttr.x <| String.fromInt 153) :: attrs)
            [ Svg.String.text <| Character.dialogAsterisk lineNum chara ]
        , Svg.String.text_
            ((SvgAttr.x <| String.fromInt <| Character.textIndent chara + 4)
                :: attrs
            )
          <|
            [ Svg.String.text text ]
        ]


renderText : Int -> Character.Name -> String -> List (Svg Msg)
renderText i chara text =
    List.indexedMap (renderTextLine chara i) <| String.split "\n" text


renderTexts : Int -> DialogBox.Model -> Svg Msg
renderTexts i box =
    case DialogBox.certifyModel True box of
        Nothing ->
            Svg.String.g [] []

        Just fullModel ->
            Svg.String.g [] <|
                renderText i fullModel.chara fullModel.text


indexMapToList : (Int -> a -> b) -> Array a -> List b
indexMapToList f arr =
    Array.indexedMap f arr |> Array.toList


renderBoxes : Array DialogBox.Model -> Svg Msg
renderBoxes boxes =
    Svg.String.g
        []
    <|
        Svg.String.map (\_ -> Unrender) DialogBox.filterDefs
            :: indexMapToList renderBox boxes
            ++ indexMapToList renderTexts boxes


toSvgHtml : Model -> Maybe (Svg.String.Html Msg)
toSvgHtml model =
    Maybe.map
        (List.singleton
            >> Svg.String.svg
                [ SvgAttr.id renderedSvgId
                , SvgAttr.attribute "version" "1.1"
                , SvgAttr.attribute "xmlns" "http://www.w3.org/2000/svg"
                , SvgAttr.width (String.fromInt DialogBox.boxWidth)
                , SvgAttr.height (String.fromInt <| DialogBox.boxHeight <| count model.boxes)
                , SvgEvents.onClick Unrender
                ]
        )
        model.render


centerWrapper : Html Msg -> Html Msg
centerWrapper content =
    Html.div
        [ style "width" "100%" ]
        [ Html.div
            [ style "width" <| String.fromInt DialogBox.boxWidth ++ "px"
            , style "margin" "0 auto"
            , style "display" "block"
            ]
            [ content ]
        ]


view : Model -> List (Html Msg)
view model =
    case model.render of
        Just svg ->
            [ centerWrapper <| Svg.String.toHtml <| Svg.String.svg [] [ svg ] ]

        _ ->
            indexMapToList mapBoxView model.boxes



-- Update


dialogStringTexts : Bool -> String -> Array String
dialogStringTexts skipBlanks s =
    let
        filterFunc =
            if skipBlanks then
                takeNonEmpty

            else
                takeJusts

        newTexts =
            fromList <| filterFunc <| fromList <| splitLinesEvery 3 2 s
    in
    case toList newTexts of
        [] ->
            fromList [ "" ]

        something ->
            newTexts


textsToString : Array (Maybe String) -> String
textsToString texts =
    String.join "\n" <| takeJusts texts


textWithUpdate : Int -> Maybe String -> Array (Maybe String) -> String
textWithUpdate entryBoxIndex newBoxText oldTexts =
    textsToString <|
        Array.set entryBoxIndex newBoxText oldTexts


pad : Int -> a -> List a -> List a
pad len item xs =
    xs ++ List.repeat (len - List.length xs) item


updateText : Int -> Maybe String -> Array (Maybe String) -> ( Int, List (Maybe String) )
updateText boxIndex newBoxText oldTexts =
    let
        prevBoxText =
            Maybe.withDefault "" <| join <| Array.get boxIndex oldTexts

        -- if we're removing text, wipe out empty dialog boxes
        skipBlanks =
            case newBoxText of
                Nothing ->
                    True

                Just s ->
                    String.length s < String.length prevBoxText

        newTexts =
            dialogStringTexts skipBlanks <| textWithUpdate boxIndex newBoxText oldTexts
    in
    ( if Array.length newTexts /= List.length (takeJusts oldTexts) then
        Array.length newTexts

      else
        boxIndex + 1
    , pad 3 Nothing <| List.map (Just << takeLines 3) (toList newTexts)
    )


updateBoxes : DialogBox.Msg -> Array DialogBox.Model -> Array DialogBox.Model
updateBoxes action boxes =
    Array.map (DialogBox.update action) boxes


render : Model -> Model
render model =
    { model
        | render = Just <| renderBoxes model.boxes
    }


bytesResultMsg : Int -> Result Http.Error Bytes -> Msg
bytesResultMsg boxIndex result =
    case result of
        Ok bytes ->
            SetImageData boxIndex bytes

        Err _ ->
            NoOp


bytesResultFromResponse : Response Bytes -> Result Http.Error Bytes
bytesResultFromResponse resp =
    case resp of
        Http.GoodStatus_ _ body ->
            Ok body

        _ ->
            Err (Http.BadBody "")


getPortraitDatum : Int -> DialogBox.Model -> Cmd Msg
getPortraitDatum boxIndex dialogBox =
    case dialogBox.imgSrc of
        Just src ->
            Http.get
                { url = src
                , expect =
                    Http.expectBytesResponse
                        (bytesResultMsg boxIndex)
                        bytesResultFromResponse
                }

        Nothing ->
            Cmd.none


getPortraitsData : Model -> Cmd Msg
getPortraitsData model =
    Cmd.batch (List.indexedMap getPortraitDatum <| Array.toList model.boxes)


updateRendered : Model -> Int -> DialogBox.Model -> Model
updateRendered model index newBox =
    let
        newBoxes =
            Array.set index newBox model.boxes
    in
    { model
        | boxes = newBoxes
        , render = Just (renderBoxes newBoxes)
    }


type Msg
    = NoOp
    | Unrender
    | SetImages Character.Name String
    | UpdateText Int (Maybe String)
    | SetImageData Int Bytes.Bytes
    | ExpectImage Int Bool


update : Msg -> Model -> ( Model, Bool )
update action model =
    -- the Bool is whether to move the cursor
    case action of
        NoOp ->
            ( model, False )

        Unrender ->
            ( { model
                | render = Nothing
              }
            , False
            )

        SetImages chara src ->
            ( { model
                | boxes =
                    updateBoxes
                        (DialogBox.SetImage chara
                            (Just src)
                            (countBoxes model == 1)
                        )
                        model.boxes
                , render = Nothing
              }
            , False
            )

        UpdateText index txt ->
            let
                ( focusBoxNum, newTexts ) =
                    updateText
                        index
                        txt
                        (getTexts model)
            in
            ( { model
                | boxes =
                    fromList <|
                        List.map
                            (\( s, box ) -> DialogBox.update (DialogBox.SetText s) box)
                        <|
                            List.map2 Tuple.pair newTexts (toList model.boxes)
                , focusIndex = focusBoxNum
                , render = Nothing
              }
            , model.focusIndex /= focusBoxNum
            )

        SetImageData index data ->
            let
                box =
                    Array.get index model.boxes
            in
            case box of
                Nothing ->
                    ( model, False )

                Just oldBox ->
                    ( updateRendered model index (DialogBox.update (DialogBox.SetData data) oldBox)
                    , False
                    )

        ExpectImage index b ->
            let
                box =
                    Array.get index model.boxes
            in
            case box of
                Nothing ->
                    ( model, False )

                Just oldBox ->
                    ( { model
                        | boxes = Array.set index (DialogBox.update (DialogBox.ExpectImage b) oldBox) model.boxes
                      }
                    , False
                    )
