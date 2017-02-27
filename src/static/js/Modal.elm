module Modal exposing (..)

import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (Options, onClick, onWithOptions)
import Json.Decode
import Maybe exposing (Maybe)
import String


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


expand : List ( String, String )
expand =
    [ ( "width", "100%" ), ( "height", "100%" ) ]


partlyTransparent : Color -> String
partlyTransparent color =
    let
        rgb = (Color.toRgb color)
    in
        "rgba("
            ++ (String.join ", "
                    <| List.map
                        toString
                        [ rgb.red
                        , rgb.green
                        , rgb.blue
                        ]
               )
            ++ ", 0.7)"


backgroundAttrs : Color -> List (Attribute Msg)
backgroundAttrs color =
    [ onClick (Show Nothing)
    , style
        [ ( "backgroundColor", partlyTransparent color )
        , ( "height", "100%" )
        , ( "width", "100%" )
        , ( "position", "fixed" )
        , ( "top", "0" )
        , ( "left", "0" )
        , ( "z-index", "99999" )
        ]
    ]


messageOn : String -> Options -> msg -> Attribute msg
messageOn event options message =
    onWithOptions event options (Json.Decode.succeed message)


noBubble : Options
noBubble =
    { stopPropagation = True
    , preventDefault = False
    }


swallowClick : msg -> Attribute msg
swallowClick =
    messageOn "click" noBubble


wrapperDiv : SizedHtml Msg -> List (Html Msg)
wrapperDiv inner =
    [ div
        [ swallowClick NoOp
        , style
            [ ( "width", inner.width )
            , ( "height", inner.height )
            , ( "overflow", "auto" )
            , ( "margin", "auto" )
            , ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "bottom", "0" )
            , ( "right", "0" )
            ]
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
