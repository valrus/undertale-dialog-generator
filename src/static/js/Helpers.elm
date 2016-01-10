module Helpers (..) where

import Array exposing (Array, fromList, toList)
import List
import List.Extra exposing (splitAt, takeWhile)
import Maybe.Extra exposing (isJust, maybeToList)
import String exposing (split, join)


takeJusts : Array (Maybe a) -> List a
takeJusts arr =
    let justs =
            Array.filter isJust arr
    in
      List.foldr (++) [ ] <| List.map maybeToList <| toList justs


takeNonEmpty : Array (Maybe String) -> List String
takeNonEmpty arr =
    let
        justs = takeJusts arr
    in
        List.filter ((/=) "") justs


splitEvery : Int -> Int -> List a -> List (Maybe (List a))
splitEvery n times xs =
    case (times, xs) of
        (_, []) ->
            [ Nothing ]

        (0, bottom) ->
            [ Just bottom ]

        (_, nonEmpty) ->
            let
                ( first, rest ) = splitAt n nonEmpty
            in
                (Just first) :: splitEvery n (times - 1) rest


splitLinesEvery : Int -> Int -> String -> List (Maybe String)
splitLinesEvery n times s =
    List.map (Just << String.join "\n")
        <| takeJusts
        <| fromList
        <| splitEvery n times (String.lines s)


takeLines : Int -> String -> String
takeLines n s =
    String.lines s
        |> List.take n
        |> String.join "\n"
