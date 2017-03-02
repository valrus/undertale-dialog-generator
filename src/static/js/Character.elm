module Character exposing (..)

import List exposing (maximum, map)
import Maybe exposing (withDefault)

import UndertaleFonts exposing (..)


type Name
    = Toriel
    | Sans
    | Papyrus
    | Undyne
    | Alphys
    | Asgore
    | Flowey
    | Napstablook
    | Mettaton
    | Asriel
    | Temmie


allNames : List Name
allNames =
    [ Toriel
    , Sans
    , Papyrus
    , Undyne
    , Alphys
    , Asgore
    , Flowey
    , Napstablook
    , Mettaton
    , Asriel
    , Temmie
    ]


type alias StyleList =
    List ( String, String )


moodCount : Bool -> Name -> Int
moodCount exmode c =
    case c of
        Toriel ->
            40

        Sans ->
            if exmode then 14 else 6

        Papyrus ->
            19

        Undyne ->
            if exmode then 39 else 24

        Alphys ->
            22

        Asgore ->
            21

        Napstablook ->
            2

        Mettaton ->
            if exmode then 29 else 22

        Flowey ->
            22

        Asriel ->
            if exmode then 26 else 19

        Temmie ->
            3


maxMoods : Bool -> Int
maxMoods exmode =
    withDefault 0 <| maximum (map (moodCount exmode) allNames)


portraitSize : Name -> ( Int, Int )
portraitSize c =
    case c of
        Napstablook ->
            ( 60, 66 )

        _ ->
            ( 60, 60 )


portraitOffset : Name -> ( Int, Int )
portraitOffset c =
    case c of
        Napstablook ->
            ( 0, 4 )

        _ ->
            ( 0, 0 )


styleCss : StyleList -> String
styleCss style =
    String.join ";\n" <| List.map (\(a, b) -> a ++ ": " ++ b) style


fontFamilyStyle : Name -> String
fontFamilyStyle c =
    case c of
        Papyrus ->
            "font-family: 'UndertalePapyrus';"

        Sans ->
            "font-family: 'UndertaleSans';"

        _ ->
            "font-family: 'determination_monoregular';"


fontStyles : Name -> StyleList
fontStyles c =
    case c of
        Papyrus ->
            [ ( "font-family", "UndertalePapyrus, Smooth_Papyrus, Papyrus" )
            , ( "font-size", "32px" )
            , ( "text-transform", "uppercase" )
            ]

        Sans ->
            [ ( "font-family"
              , "UndertaleSans, Comic Sans, Comic Sans MS Regular, Comic Sans MS" )
            , ( "font-size", "32px" )
            , ( "letter-spacing", "1px" )
            , ( "text-transform", "lowercase" )
            ]

        _ ->
            [ ( "font-family", "determination_monoregular" )
            , ( "font-size", "26px" )
            ]


textboxWidth : Name -> ( String, String)
textboxWidth c =
    case c of
        Papyrus ->
            ( "width", "416px" )

        _ ->
            ( "width", "382px" )


textboxLeft : Name -> ( String, String )
textboxLeft c =
    ( "left", (toString <| textIndent c) ++ "px" )


textIndent : Name -> Int
textIndent c =
    case c of
        Papyrus ->
            150
        _ ->
            184


yOffset : Name -> Int
yOffset c =
    case c of
        Papyrus ->
            -4

        Sans ->
            -2

        _ ->
            0


dialogAsterisk : Int -> Name -> String
dialogAsterisk lineIndex c =
    case c of
        Papyrus ->
            ""

        _ ->
            if lineIndex == 0 then "*" else ""


thumbnail : Name -> List ( String, String )
thumbnail c =
    [ ("width", "60px")
    , ("height", if c == Napstablook then "66px" else "60px")
    ]
