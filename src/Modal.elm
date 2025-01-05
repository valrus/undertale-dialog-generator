module Modal exposing (..)

import Color exposing (Color, toRgba)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick)
import Json.Decode
import Maybe exposing (Maybe)


type alias Model =
    { backgroundColor : Color
    , childElement : Maybe (SizedHtml Msg)
    }


type alias SizedHtml msg =
    { html : Html msg
    , width : String
    , height : String
    }


init : Color -> Model
init color =
    { backgroundColor = color
    , childElement = Nothing
    }


type Msg
    = NoOp
    | Show (Maybe (SizedHtml Msg))


update : Msg -> Model -> Model
update message model =
    case message of
        Show something ->
            { model
                | childElement = something
            }

        NoOp ->
            model


expand : List (Html.Attribute msg)
expand =
    [ style "width" "100%", style "height" "100%" ]


partlyTransparent : Color -> String
partlyTransparent color =
    let
        rgba =
            toRgba color
    in
    Color.toCssString <|
        Color.rgba rgba.red rgba.green rgba.blue 0.7


backgroundAttrs : Color -> List (Attribute Msg)
backgroundAttrs color =
    [ onClick (Show Nothing)
    , style "backgroundColor" (partlyTransparent color)
    , style "height" "100vh"
    , style "width" "100vw"
    , style "position" "fixed"
    , style "top" "0"
    , style "left" "0"
    , style "z-index" "99999"
    , class "modalWrapper"
    ]


swallowClick : msg -> Attribute msg
swallowClick msg =
    custom "click" (Json.Decode.succeed { message = msg, stopPropagation = True, preventDefault = True })


wrapperDiv : SizedHtml Msg -> List (Html Msg)
wrapperDiv inner =
    [ div
        -- [ swallowClick NoOp
        [ style "width" inner.width
        , style "height" inner.height
        , style "overflow" "auto"
        , style "margin" "auto"
        , style "position" "absolute"
        ]
        [ inner.html ]
    ]


view : Model -> Html Msg
view model =
    case model.childElement of
        Nothing ->
            div [] []

        Just dialog ->
            div
                (backgroundAttrs model.backgroundColor)
                (wrapperDiv dialog)
