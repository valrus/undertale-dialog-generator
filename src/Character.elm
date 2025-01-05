module Character exposing (..)

import Helpers exposing (StyleList, stylesFromArgs)
import Html
import Html.Attributes exposing (style)
import List exposing (map, maximum)
import Maybe exposing (withDefault)
import Set
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


nameAsString : Name -> String
nameAsString name =
    case name of
        Toriel ->
            "Toriel"

        Sans ->
            "Sans"

        Papyrus ->
            "Papyrus"

        Undyne ->
            "Undyne"

        Alphys ->
            "Alphys"

        Asgore ->
            "Asgore"

        Flowey ->
            "Flowey"

        Napstablook ->
            "Napstablook"

        Mettaton ->
            "Mettaton"

        Asriel ->
            "Asriel"

        Temmie ->
            "Temmie"


moodCount : Bool -> Name -> Int
moodCount exmode c =
    case c of
        Toriel ->
            40

        Sans ->
            if exmode then
                14

            else
                6

        Papyrus ->
            19

        Undyne ->
            if exmode then
                39

            else
                24

        Alphys ->
            22

        Asgore ->
            21

        Napstablook ->
            2

        Mettaton ->
            if exmode then
                29

            else
                22

        Flowey ->
            22

        Asriel ->
            if exmode then
                26

            else
                19

        Temmie ->
            3


maxMoods : Bool -> Int
maxMoods exmode =
    withDefault 0 <| maximum (map (moodCount exmode) allNames)


spriteFolder : String -> Name -> String
spriteFolder root c =
    root ++ "images/sprites/" ++ nameAsString c


spriteNumber : String -> Name -> Int -> String
spriteNumber root c n =
    spriteFolder root c ++ "/" ++ String.fromInt n ++ ".png"


defaultSprite : String -> Name -> Bool -> String
defaultSprite root c asThumbnail =
    spriteNumber root
        c
        (if asThumbnail then
            0

         else
            1
        )


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


fontFamilyStyle : Name -> String
fontFamilyStyle c =
    case c of
        Papyrus ->
            "font-family: 'UndertalePapyrus';"

        Sans ->
            "font-family: 'UndertaleSans';"

        _ ->
            "font-family: 'determination_monoregular';"


fontStyleArgs : Name -> List ( String, String )
fontStyleArgs c =
    case c of
        Papyrus ->
            [ ( "font-family", "UndertalePapyrus, Smooth_Papyrus, Papyrus" )
            , ( "font-size", "32px" )
            , ( "text-transform", "uppercase" )
            ]

        Sans ->
            [ ( "font-family"
              , "UndertaleSans, Comic Sans, Comic Sans MS Regular, Comic Sans MS"
              )
            , ( "font-size", "32px" )
            , ( "letter-spacing", "1px" )
            , ( "text-transform", "lowercase" )
            ]

        _ ->
            [ ( "font-family", "determination_monoregular" )
            , ( "font-size", "26px" )
            ]


fontStyles : Name -> StyleList msg
fontStyles c =
    stylesFromArgs <| fontStyleArgs c


textboxWidth : Name -> Html.Attribute msg
textboxWidth c =
    case c of
        Papyrus ->
            style "width" "416px"

        _ ->
            style "width" "382px"


textboxLeft : Name -> Html.Attribute msg
textboxLeft c =
    style "left" ((String.fromInt <| textIndent c) ++ "px")


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
            if lineIndex == 0 then
                "*"

            else
                ""


thumbnail : Name -> StyleList msg
thumbnail c =
    [ style "width" "60px"
    , style "height" <|
        if c == Napstablook then
            "66px"

        else
            "60px"
    ]


type alias RenderOverride =
    ( String, Int )


cussParams : Name -> RenderOverride
cussParams c =
    case c of
        Toriel ->
            ( "I don't use nasty words\nlike that.", 27 )

        Sans ->
            ( "i don't really like\nto say things like that.", 4 )

        Papyrus ->
            ( "I DON'T THINK I WANT\nTO SAY THAT.", 15 )

        Undyne ->
            ( "Don't go putting\nwords like that\nin my mouth.", 10 )

        Alphys ->
            ( "Um... no!\nI don't want to\nsay that!", 10 )

        Asgore ->
            ( "That's not a very nice\nthing to say.", 5 )

        Napstablook ->
            ( "oh.......\ni can't say that....", 1 )

        Mettaton ->
            ( "Oh, my. Being nasty on\nthe Internet, are we?", 20 )

        Flowey ->
            ( "Ooh, look at you.\nYou sure are edgy.", 8 )

        Asriel ->
            ( "Maybe I used to\nsay things like that,\nbut not anymore.", 10 )

        Temmie ->
            ( "NO!!!\nnasty wordds r...\nNOT CUTE", 1 )


languageParams : Name -> ( String, Int )
languageParams c =
    case c of
        Toriel ->
            ( "I'm sorry, my child.\nI don't understand!", 7 )

        Sans ->
            ( "buddy.\ndo i look like\na polyglot to you?", 2 )

        Papyrus ->
            ( "I DON'T KNOW HOW\nTO SAY THOSE\nLETTERS!", 11 )

        Undyne ->
            ( "Heh! Um... what???", 19 )

        Alphys ->
            ( "Um... huh?", 7 )

        Asgore ->
            ( "Oh... I'm sorry.\nI don't speak that\nlanguage.", 5 )

        Napstablook ->
            ( "oh.......\ni don't understand.....", 1 )

        Mettaton ->
            ( "What?\nDarling, I only\nspeak English!", 15 )

        Flowey ->
            ( "Uhhhhhh...\nOkay, you lost me.", 3 )

        Asriel ->
            ( "Sorry, I don't\nunderstand.", 3 )

        Temmie ->
            ( "um.....\n...... ... ...... .\n... .. . ... ..wut????!", 1 )


asciiSet : Set.Set Char
asciiSet =
    Set.fromList <|
        String.toList <|
            " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"


cp1252Set : Set.Set Char
cp1252Set =
    Set.union asciiSet <|
        Set.fromList <|
            String.toList <|
                "€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"


illegalStringForChar : Name -> String -> Bool
illegalStringForChar c s =
    let
        permissibleCharsSet =
            case c of
                Sans ->
                    asciiSet

                Papyrus ->
                    asciiSet

                _ ->
                    cp1252Set
    in
    (Set.size <|
        Set.diff
            (Set.fromList <| String.toList s)
            permissibleCharsSet
    )
        > 0
