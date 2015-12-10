module Character where

type Name = Toriel | Sans | Papyrus | Undyne | Alphys | Asgore | Flowey

moodCount : Name -> Int
moodCount c =
  case c of
    Toriel -> 45
    Sans -> 5
    Papyrus -> 18
    Undyne -> 25
    Alphys -> 22
    Asgore -> 0
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
    Just Papyrus -> 36.0
    Just Sans -> 26.0
    _ -> 26.0
