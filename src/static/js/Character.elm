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
