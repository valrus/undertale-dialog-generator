module Helpers exposing (..)

import Array exposing (Array, fromList)
import Html
import Html.Attributes exposing (style)
import List
import List.Extra exposing (splitAt)
import Maybe.Extra exposing (isJust)
import Url exposing (percentEncode)


type alias KeyCode =
    Int


type alias Position =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias StyleList msg =
    List (Html.Attribute msg)


offset : Int -> Int -> Position -> Position
offset x y pos =
    Position (x + pos.x) (y + pos.y) pos.w pos.h


takeJusts : Array (Maybe a) -> List a
takeJusts arr =
    let
        justs =
            Array.filter isJust arr
    in
    List.foldr (++) [] <| List.map Maybe.Extra.toList <| Array.toList justs


takeNonEmpty : Array (Maybe String) -> List String
takeNonEmpty arr =
    let
        justs =
            takeJusts arr
    in
    List.filter ((/=) "") justs


splitEvery : Int -> Int -> List a -> List (Maybe (List a))
splitEvery n times xs =
    case ( times, xs ) of
        ( _, [] ) ->
            [ Nothing ]

        ( 0, bottom ) ->
            [ Just bottom ]

        ( _, nonEmpty ) ->
            let
                ( first, rest ) =
                    splitAt n nonEmpty
            in
            Just first :: splitEvery n (times - 1) rest


splitLinesEvery : Int -> Int -> String -> List (Maybe String)
splitLinesEvery n times s =
    List.map (Just << String.join "\n") <|
        takeJusts <|
            fromList <|
                splitEvery n times (String.lines s)


takeLines : Int -> String -> String
takeLines n s =
    String.lines s
        |> List.take n
        |> String.join "\n"



-- From https://github.com/evancz/elm-http/blob/3.0.1/src/Http.elm#L56-L73


makeUrl : String -> List ( String, String ) -> String
makeUrl baseUrl args =
    case args of
        [] ->
            baseUrl

        _ ->
            baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape string =
    String.join "+" (String.split "%20" (percentEncode string))


styleCss : List ( String, String ) -> String
styleCss style =
    String.join ";\n" <| List.map (\( a, b ) -> a ++ ": " ++ b) style


crispyFontStyleArgs : List ( String, String )
crispyFontStyleArgs =
    [ ( "font-smooth", "never" )
    , ( "-moz-osx-font-smoothing", "grayscale" )
    , ( "-webkit-font-smoothing", "none" )
    ]


stylesFromArgs : List ( String, String ) -> StyleList msg
stylesFromArgs args =
    List.map (\( a, b ) -> style a b) args
