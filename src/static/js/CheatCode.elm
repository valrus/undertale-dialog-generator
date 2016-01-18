module CheatCode (..) where


import Debug exposing (log)
import Dict exposing (Dict)
import Effects exposing (Effects, none)
import String
import Task


type alias Model =
    { codeStatus : Dict String Int
    , mailbox : Signal.Mailbox String
    }


init : List String -> Signal.Mailbox String -> Model
init codes mailbox =
    { codeStatus = Dict.fromList <| List.map (\s -> (s, 0)) codes
    , mailbox = mailbox
    }


checkChar : Char -> String -> Int -> Int
checkChar c code matches =
    case String.uncons (String.dropLeft matches code) of
      Just (next, _) -> if next == c then matches + 1 else 0

      _ -> 0


isComplete : (String, Int) -> Bool
isComplete (s, n) =
    String.length s == n


update : Char -> Model -> (Model, Effects String)
update c model =
    let
        status =
            Dict.map (checkChar c) model.codeStatus

        complete =
            List.head <| List.filter isComplete <| Dict.toList status

    in case complete of
         Nothing ->
             (
             { model
             | codeStatus = status
             }
             , none)

         Just (match, _) ->
             (
             { model
             | codeStatus = Dict.map (\_ _ -> 0) status
             }
             , Signal.send model.mailbox.address match |> Task.map (\_ -> match) |> Effects.task)


mailbox : Signal.Mailbox String
mailbox =
    Signal.mailbox ""
