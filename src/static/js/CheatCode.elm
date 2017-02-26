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


matchCount : KeyCode -> Char -> Int -> Int
matchCount k nextChar prevCount =
    let
        next =
            Char.toCode nextChar
    in
        if k == next then
            prevCount + 1
        else
            0


checkChar : KeyCode -> String -> Int -> Int
checkChar k code matches =
    case String.uncons (String.dropLeft matches code) of
        Just ( next, _ ) ->
            matchCount k next matches

        _ ->
            0


isComplete : ( String, Int ) -> Bool
isComplete ( s, n ) =
    String.length s == n



-- TODO: Should probably make the first arg a Msg (Set KeyCode) to be proper


update : KeyCode -> Model -> ( Model, Maybe String )
update k model =
    let
        status =
            Dict.map (checkChar k) model.codeStatus

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
