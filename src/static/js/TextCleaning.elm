module TextCleaning exposing (..)

import Regex exposing (contains, escape, regex, Regex)

import Character exposing (illegalCharRegex)


type TextReplacement
  = HasCusses
  | UnknownLanguage
  | TooLong
  | NoMatch


matchHelper : Character.Name -> String -> TextReplacement
matchHelper chara s =
  if contains cussRegex s then HasCusses
  else if contains (illegalCharRegex chara) s then UnknownLanguage
  else if isTooLong s then TooLong
  else NoMatch


isTooLong : String -> Bool
isTooLong s =
    String.length s > 210


cussRegex : Regex
cussRegex =
    regex "\\b(fag|faggot|tranny|nigger|kike)\\b"


unicodeSanitizer : Char -> Char
unicodeSanitizer c =
    case c of
        '‘' -> '\''

        '’' -> '\''

        '“' -> '"'

        '”' -> '"'

        -- non-breaking space
        ' ' -> ' '

        _ -> c
