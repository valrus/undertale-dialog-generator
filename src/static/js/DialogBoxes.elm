module DialogBoxes (..) where

import Array exposing (Array, toList, fromList)
import Html exposing (Html)
import Maybe.Extra exposing (isJust, join)
import String


-- Local modules

import Character
import DialogBox
import Helpers exposing (..)


-- Helpers for multiple boxes


type alias Model =
    { boxes : Array DialogBox.Model
    , focusIndex : Int
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
    }


count : Model -> Int
count model =
    Array.length <| Array.filter (isJust << .text) model.boxes


concat : Model -> String
concat model =
    String.join "\n" <| takeJusts <| Array.map .text model.boxes


getText : Int -> Model -> Maybe String
getText i model =
    Array.get i model.boxes `Maybe.andThen` .text


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


convertViewMessage : Int -> DialogBox.Action -> Action
convertViewMessage boxNum boxAction =
    case boxAction of
        DialogBox.NoOp ->
            NoOp

        DialogBox.SetImage c s force ->
            case s of
                Nothing ->
                    NoOp

                Just src ->
                    SetImages c src

        DialogBox.SetText s ->
            UpdateText boxNum s

        DialogBox.ExpectImage b ->
            ExpectImage boxNum b


view : Signal.Address Action -> Model -> List Html
view address model =
    Array.toList
        <| Array.indexedMap
            (\i -> DialogBox.view (Signal.forwardTo address (convertViewMessage i)))
            model.boxes



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
    textsToString
        <| Array.set entryBoxIndex newBoxText oldTexts


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


updateBoxes : DialogBox.Action -> Array DialogBox.Model -> Array DialogBox.Model
updateBoxes action boxes =
    Array.map (DialogBox.update action) boxes


resetTexts : Array DialogBox.Model -> Array DialogBox.Model
resetTexts boxes =
    let
        boxList = toList boxes

        ( first, rest ) = ( List.head boxList, List.tail boxList )
    in
        case Maybe.map2 (,) first rest of
            Nothing ->
                Array.repeat 1 (DialogBox.init (Just "") 1)

            Just ( blank, empty ) ->
                initBoxes


type Action
    = NoOp
    | SetImages Character.Name String
    | UpdateText Int (Maybe String)
    | ExpectImage Int Bool


update : Action -> Model -> ( Model, Bool )
update action model =
    case action of
        NoOp ->
            ( model, False )

        SetImages chara src ->
            ( { model
                | boxes =
                    updateBoxes
                        (DialogBox.SetImage chara (Just src) (count model == 1))
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
                        fromList
                            <| List.map
                                (\( s, box ) -> DialogBox.update (DialogBox.SetText s) box)
                            <| List.map2 (,) newTexts (toList model.boxes)
                    , focusIndex = focusBoxNum
                  }
                , (model.focusIndex /= focusBoxNum)
                )

        ExpectImage index b ->
            let
                box = Array.get index model.boxes
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
