module CheatCode exposing (..)

import Debug exposing (log)
import Dict exposing (Dict)
import Set exposing (Set)
import String
import Task


type alias Model =
    { codeStatus : Dict String Int
    , mailbox : Signal.Mailbox String
    }


init : List String -> Signal.Mailbox String -> Model
init codes mailbox =
    { codeStatus = Dict.fromList <| List.map (\s -> ( s, 0 )) codes
    , mailbox = mailbox
    }


soleMember : Set Char -> Char -> Int -> Int
soleMember cs c prev =
    case Set.size cs of
        0 ->
            prev

        1 ->
            if (Set.member c cs) then
                prev + 1
            else
                0

        _ ->
            if (Set.member c cs) then
                prev
            else
                0


checkChar : Set Char -> String -> Int -> Int
checkChar cs code matches =
    case String.uncons (String.dropLeft matches code) of
        Just ( next, _ ) ->
            soleMember cs next matches

        _ ->
            0


isComplete : ( String, Int ) -> Bool
isComplete ( s, n ) =
    String.length s == n


update : Set Char -> Model -> ( Model, Effects String )
update cs model =
    let
        status =
            Dict.map (checkChar cs) model.codeStatus

        complete =
            List.head <| List.filter isComplete <| Dict.toList status
    in
        case complete of
            Nothing ->
                ( { model
                    | codeStatus = status
                  }
                , none
                )

            Just ( match, _ ) ->
                ( { model
                    | codeStatus = Dict.map (\_ _ -> 0) status
                  }
                , Signal.send model.mailbox.address match |> Task.map (\_ -> match) |> Effects.task
                )


mailbox : Signal.Mailbox String
mailbox =
    Signal.mailbox ""
