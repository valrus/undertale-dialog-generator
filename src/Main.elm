import Character
import UndertaleDialog exposing (init, update, view)
import StartApp.Simple exposing (start)


main =
  start
  { model = init [ Character.Alphys ]
  , update = update
  , view = view
  }
