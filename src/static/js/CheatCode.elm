module CheatCode exposing (..)

import Char exposing (KeyCode)
import Dict exposing (Dict)
import Maybe
import Set exposing (Set)
import String


type alias Model =
    { codeStatus : Dict String Int
    }


init : List String -> Model
init codes =
    { codeStatus = Dict.fromList <| List.map (\s -> ( s, 0 )) codes
    }


onlyShift : Set KeyCode -> Bool
onlyShift =
    Set.toList >> List.all ((==) 16)


soleMember : Set KeyCode -> Char -> Int -> Int
soleMember ks c prev =
    let
        k = Char.toCode c
    in
        case Set.size ks of
            0 ->
                prev

            _ ->
                let
                    accept = onlyShift (Set.remove k ks)
                in
                    if (Set.member k ks) then
                        if accept then
                            prev + 1
                        else
                            0
                    else
                        if accept then
                            prev
                        else
                            0


checkChar : Set KeyCode -> String -> Int -> Int
checkChar ks code matches =
    case String.uncons (String.dropLeft matches code) of
        Just ( next, _ ) ->
            soleMember ks next matches

        _ ->
            0


isComplete : ( String, Int ) -> Bool
isComplete ( s, n ) =
    String.length s == n


-- TODO: Should probably make the first arg a Msg (Set KeyCode) to be proper
update : Set KeyCode -> Model -> ( Model, Maybe String )
update ks model =
    let
        status =
            Dict.map (checkChar ks) model.codeStatus

        complete =
            List.head <| List.filter isComplete <| Dict.toList status
    in
        case complete of
            Nothing ->
                ( { model
                    | codeStatus = status
                  }
                , Nothing
                )

            Just ( match, _ ) ->
                ( { model
                    | codeStatus = Dict.map (\_ _ -> 0) status
                  }
                , Just match
                )
