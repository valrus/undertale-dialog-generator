module Character exposing (..)

import List exposing (maximum, map)
import Maybe exposing (withDefault)
import Regex exposing (escape, regex, Regex)

import Helpers exposing (StyleList, styleCss)
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


spriteFolder : String -> Name -> String
spriteFolder root c =
    root ++ "images/sprites/" ++ toString c


spriteNumber : String -> Name -> Int -> String
spriteNumber root c n =
    (spriteFolder root c) ++ "/" ++ (toString n) ++ ".png"


defaultSprite : String -> Name -> Bool -> String
defaultSprite root c thumbnail =
    spriteNumber root c (if thumbnail then 0 else 1)


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


thumbnail : Name -> StyleList
thumbnail c =
    [ ("width", "60px")
    , ("height", if c == Napstablook then "66px" else "60px")
    ]


type alias RenderOverride =
    ( String, Int )


cussParams : Name -> RenderOverride
cussParams c =
    case c of
        Toriel ->
            ("I don't use nasty words\nlike that.", 27)

        Sans ->
            ("i don't really like\nto say things like that.", 4)

        Papyrus ->
            ("I DON'T THINK I WANT\nTO SAY THAT.", 15)

        Undyne ->
            ("Don't go putting\nwords like that\nin my mouth.", 10)

        Alphys ->
            ("Um... no!\nI don't want to\nsay that!", 10)

        Asgore ->
            ("That's not a very nice\nthing to say.", 5)

        Napstablook ->
            ("oh.......\ni can't say that....", 1)

        Mettaton ->
            ("Oh, my. Being nasty on\nthe Internet, are we?", 20)

        Flowey ->
            ("Ooh, look at you.\nYou sure are edgy.", 8)

        Asriel ->
            ("Maybe I used to\nsay things like that,\nbut not anymore.", 10)

        Temmie ->
            ("NO!!!\nnasty wordds r...\nNOT CUTE", 1)


languageParams : Name -> ( String, Int )
languageParams c =
    case c of
        Toriel ->
            ("I'm sorry, my child.\nI don't understand!", 7)

        Sans ->
            ("buddy.\ndo i look like\na polyglot to you?", 2)

        Papyrus ->
            ("I DON'T KNOW HOW\nTO SAY THOSE\nLETTERS!", 11)

        Undyne ->
            ("Heh! Um... what???", 19)

        Alphys ->
            ("Um... huh?", 7)

        Asgore ->
            ("Oh... I'm sorry.\nI don't speak that\nlanguage.", 5)

        Napstablook ->
            ("oh.......\ni don't understand.....", 1)

        Mettaton ->
            ("What?\nDarling, I only\nspeak English!", 15)

        Flowey ->
            ("Uhhhhhh...\nOkay, you lost me.", 3)

        Asriel ->
            ("Sorry, I don't\nunderstand.", 3)

        Temmie ->
            ("um.....\n...... ... ...... .\n... .. . ... ..wut????!", 1)


asciiString : String
asciiString =
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"


cp1252String : String
cp1252String =
    "€‚ƒ„…†‡ˆ‰Š‹ŒŽ‘’“”•–—˜™š›œžŸ¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ"


outsideAscii : Regex
outsideAscii =
    regex <| "[^" ++ (escape asciiString) ++ "]"


outsideCP1252 : Regex
outsideCP1252 =
    regex <| "[^" ++ (escape asciiString) ++ (escape cp1252String) ++ "]"


illegalCharRegex : Name -> Regex
illegalCharRegex c =
    case c of
        Sans ->
            outsideAscii

        Papyrus ->
            outsideAscii

        _ ->
            outsideCP1252
