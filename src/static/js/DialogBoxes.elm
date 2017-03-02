port module DialogBoxes exposing (..)

import Array exposing (Array, toList, fromList)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Maybe.Extra exposing (isJust, join)
import String
import Svg
import Svg.Attributes as SvgAttr
import Svg.Events
import Debug exposing (log)


-- Local modules

import Character
import DialogBox
import Helpers exposing (..)


-- Helpers for multiple boxes


type alias Model =
    { boxes : Array DialogBox.Model
    , focusIndex : Int
    , renderId : Maybe String
    }


initBoxes : Array DialogBox.Model
initBoxes =
    fromList
        [ DialogBox.init (Just "") 1
        , DialogBox.init Nothing 2
        , DialogBox.init Nothing 3
        ]


init : Model
init =
    { boxes = initBoxes
    , focusIndex = 0
    , renderId = Nothing
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
    List.any isJust (toList <| Array.map DialogBox.certifyModel model.boxes)



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

        DialogBox.SetText s ->
            UpdateText boxNum s

        DialogBox.ExpectImage b ->
            ExpectImage boxNum b


mapBoxView : Int -> DialogBox.Model -> Html Msg
mapBoxView i box =
    Html.map (convertViewMessage i) (DialogBox.view box)



-- Crispy rendering


renderBox : Int -> DialogBox.Model -> Svg.Svg Msg
renderBox i box =
    case DialogBox.certifyModel box of
        Nothing ->
            Svg.g [] []

        Just fullModel ->
            Svg.g
                []
            <|
                List.map (Html.map <| convertViewMessage i) <|
                    DialogBox.singleBox
                        0
                        (DialogBox.boxHeight i)
                        fullModel


textLineOffset : Int -> Int -> Character.Name -> Int
textLineOffset offset lineNum chara =
    (DialogBox.boxHeight offset) + 32 + (36 * lineNum) + (Character.yOffset chara)


renderTextLine : Character.Name -> Int -> Int -> String -> Svg.Svg Msg
renderTextLine chara offset lineNum text =
    let
        attrs =
            [ SvgAttr.y <| toString (textLineOffset offset lineNum chara)
            , SvgAttr.alignmentBaseline "text-before-edge"
            ]
    in
        Svg.g
            [ SvgAttr.textAnchor "start"
            , SvgAttr.xmlSpace "preserve"
            , SvgAttr.fill "white"
            , SvgAttr.filter "url(#crispify)"
            , SvgAttr.style <| Character.styleCss (Character.fontStyles chara)
            ]
            [ Svg.text_
                ([ SvgAttr.x <| toString 153 ] ++ attrs)
                [ Svg.text <| Character.dialogAsterisk lineNum chara ]
            , Svg.text_
                ([ SvgAttr.x <| toString <| (Character.textIndent chara) + 4 ]
                    ++ attrs
                )
              <|
                [ Svg.text text ]
            ]


renderText : Int -> Character.Name -> String -> List (Svg.Svg Msg)
renderText i chara text =
    List.indexedMap (renderTextLine chara i) <| String.split "\n" text


renderTexts : Int -> DialogBox.Model -> Svg.Svg Msg
renderTexts i box =
    case DialogBox.certifyModel box of
        Nothing ->
            Svg.g [] []

        Just fullModel ->
            Svg.g [] <|
                renderText i fullModel.chara fullModel.text


indexMapToList : (Int -> a -> b) -> Array a -> List b
indexMapToList f arr =
    Array.toList <| Array.indexedMap f arr


renderBoxes : Array DialogBox.Model -> String -> Html Msg
renderBoxes boxes id =
    Svg.svg
        [ SvgAttr.id id
        , SvgAttr.version "1.1"
        , SvgAttr.xmlSpace "http://www.w3.org/2000/svg"
        , SvgAttr.width (toString DialogBox.boxWidth)
        , SvgAttr.height (toString <| DialogBox.boxHeight <| count boxes)
        , Svg.Events.onClick Unrender
        ]
    <|
        [ (Svg.map (\_ -> Unrender) DialogBox.filterDefs) ]
            ++ (indexMapToList renderBox boxes)
            ++ (indexMapToList renderTexts boxes)


centerWrapper : Html Msg -> Html Msg
centerWrapper content =
    Html.div
        [ style [ ( "width", "100%" ) ] ]
        [ Html.div
            [ style
                [ ( "width", (toString DialogBox.boxWidth) ++ "px" )
                , ( "margin", "0 auto" )
                , ( "display", "block" )
                ]
            ]
            [ content ]
        ]


view : Model -> List (Html Msg)
view model =
    case model.renderId of
        Just id ->
            [ centerWrapper <| renderBoxes model.boxes id ]

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
                    (String.length s) < (String.length prevBoxText)

        newTexts =
            dialogStringTexts skipBlanks <| textWithUpdate boxIndex newBoxText oldTexts
    in
        ( if Array.length newTexts /= List.length (takeJusts oldTexts) then
            Array.length newTexts
          else
            (boxIndex + 1)
        , pad 3 Nothing <| List.map (Just << takeLines 3) (toList newTexts)
        )


updateBoxes : DialogBox.Msg -> Array DialogBox.Model -> Array DialogBox.Model
updateBoxes action boxes =
    Array.map (DialogBox.update action) boxes


resetTexts : Array DialogBox.Model -> Array DialogBox.Model
resetTexts boxes =
    let
        boxList =
            toList boxes

        ( first, rest ) =
            ( List.head boxList, List.tail boxList )
    in
        case Maybe.map2 (,) first rest of
            Nothing ->
                Array.repeat 1 (DialogBox.init (Just "") 1)

            Just ( blank, empty ) ->
                initBoxes


render : Model -> ( Model, String )
render model =
    ( { model
        | renderId = Just renderedSvgId
      }
    , renderedSvgId
    )


type Msg
    = Unrender
    | SetImages Character.Name String
    | UpdateText Int (Maybe String)
    | ExpectImage Int Bool


update : Msg -> Model -> ( Model, Bool )
update action model =
    case action of
        Unrender ->
            ( { model
                | renderId = Nothing
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
                                List.map2 (,) newTexts (toList model.boxes)
                    , focusIndex = focusBoxNum
                    , renderId = Nothing
                  }
                , (model.focusIndex /= focusBoxNum)
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


port getImg : String -> Cmd msg
port getRenderData : (String -> msg) -> Sub msg
