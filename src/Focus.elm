port module Focus exposing (..)

type alias Params =
    { elementId : String
    , moveCursorToEnd : Bool
    }


emptyParams : Params
emptyParams =
    { elementId = "", moveCursorToEnd = False }


type Msg
    = Focus Params
    | NoOp


focusFilter : Msg -> Maybe Params
focusFilter action =
    case action of
        Focus params ->
            Just params

        _ ->
            Nothing

port focus : Params -> Cmd msg
