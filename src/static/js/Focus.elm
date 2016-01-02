module Focus (..) where

import Effects
import Task


-- Reference: https://gist.github.com/pdamoc/97ca5e1ad605f7e5ebcb


type alias Params =
    { elementId : String
    , moveCursorToEnd : Bool
    }


emptyParams =
    { elementId = "", moveCursorToEnd = False }


type Action
    = Focus Params
    | NoOp


focusFilter : Action -> Maybe Params
focusFilter action =
    case action of
        Focus params ->
            Just params

        _ ->
            Nothing
