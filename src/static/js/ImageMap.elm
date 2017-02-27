module ImageMap exposing (mapArea)

import Either exposing (Either)
import Html exposing (Html, node)
import Html.Attributes exposing (href, shape, title, alt, coords)
import Html.Events exposing (onClick)
import String exposing (join)


mapArea : List Int -> String -> Either String msg -> Html msg
mapArea coords caption action =
    let
        clickAction =
            case action of
                Either.Left url ->
                    Html.Attributes.href url

                Either.Right msg ->
                    onClick msg
    in
        Html.node
            "area"
            [ Html.Attributes.shape "rect"
            , Html.Attributes.title caption
            , Html.Attributes.alt caption
            , Html.Attributes.coords <| join ", " <| List.map toString coords
            , clickAction
            ]
            []
