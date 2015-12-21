module Character where

type Name = Toriel | Sans | Papyrus | Undyne | Alphys | Asgore | Flowey | Napstablook


moodCount : Name -> Int
moodCount c =
  case c of
    Toriel -> 46
    Sans -> 6
    Papyrus -> 19
    Undyne -> 26
    Alphys -> 22
    Asgore -> 14
    Napstablook -> 2
    Flowey -> 0


fontFace : Maybe Name -> List String
fontFace c =
  case c of
    Just Papyrus -> [ "Papyrus" ]
    Just Sans -> [ "Comic Sans", "Comic Sans MS Regular", "Comic Sans MS" ]
    _ -> [ "determination_monoregular" ]


fontSize : Maybe Name -> Float
fontSize c =
  case c of
    Just Papyrus ->  34.0
    Just Sans -> 26.0
    _ -> 26.0


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
