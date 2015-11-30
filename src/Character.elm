module Character where

type Name = Toriel | Sans | Papyrus | Undyne | Alphys | Asgore | Flowey

moodCount : Name -> Int
moodCount c =
  case c of
    Toriel -> 0
    Sans -> 0
    Papyrus -> 0
    Undyne -> 0
    Alphys -> 22
    Asgore -> 0
    Flowey -> 0
