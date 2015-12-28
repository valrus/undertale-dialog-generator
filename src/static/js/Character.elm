module Character where

type Name =
  Toriel
  | Sans
  | Papyrus
  | Undyne
  | Alphys
  | Asgore
  | Flowey
  | Napstablook
  | Mettaton


type alias StyleList = List (String, String)


moodCount : Name -> Int
moodCount c =
  case c of
    Toriel -> 40
    Sans -> 6
    Papyrus -> 19
    Undyne -> 24
    Alphys -> 22
    Asgore -> 6
    Napstablook -> 2
    Mettaton -> 22
    Flowey -> 0


portraitSize : Maybe Name -> (Int, Int)
portraitSize c =
  case c of
    Just Napstablook -> (60, 66)
    _ -> (60, 60)


portraitOffset : Maybe Name -> (Float, Float)
portraitOffset c =
  case c of
    Just Napstablook -> (0, -4)
    _ -> (0, 0)


fontStyles : Maybe Name -> StyleList
fontStyles c =
  case c of
    Just Papyrus ->
      [ ("font-family", "Smooth_Papyrus, Papyrus")
      , ("font-size", "26px")
      , ("font-weight", "bold")
      , ("text-transform", "uppercase")
      ]
    Just Sans ->
      [ ("font-family", "Comic Sans, Comic Sans MS Regular, Comic Sans MS")
      , ("font-size", "30px")
      , ("font-weight", "bold")
      , ("letter-spacing", "1px")
      , ("text-transform", "lowercase")
      ]
    _ ->
      [ ("font-family", "determination_monoregular")
      , ("font-size", "26px")
      ]


textboxStyles : Maybe Name -> StyleList
textboxStyles c =
  case c of
    Just Papyrus ->
      [ ("width", "416px")
      , ("left", "150px")
      ]
    Just Sans ->
      [ ("width", "382px")
      , ("left", "184px")
      ]
    _ ->
      [ ("width", "382px")
      , ("left", "184px")
      ]


dialogAsterisk : Maybe Name -> String
dialogAsterisk c =
  case c of
    Just Papyrus -> ""
    _ -> "*"

