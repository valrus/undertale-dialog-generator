module TextCleaning exposing (..)

import Character exposing (illegalStringForChar)
import Regex exposing (Regex, contains)


type TextReplacement
    = HasCusses
    | UnknownLanguage
    | TooLong
    | NoMatch


matchHelper : Character.Name -> String -> TextReplacement
matchHelper chara s =
    if contains cussRegex s then
        HasCusses

    else if illegalStringForChar chara s then
        UnknownLanguage

    else if isTooLong s then
        TooLong

    else
        NoMatch


isTooLong : String -> Bool
isTooLong s =
    String.length s > 210


cussRegex : Regex
cussRegex =
    Maybe.withDefault Regex.never <|
        Regex.fromString "\\b(fag|faggot|tranny|nigger|kike)\\b"


unicodeSanitizer : Char -> Char
unicodeSanitizer c =
    case c of
        '‘' ->
            '\''

        '’' ->
            '\''

        '“' ->
            '"'

        '”' ->
            '"'

        -- non-breaking space
        '\u{00A0}' ->
            ' '

        _ ->
            c
