module Page.Login exposing (..)


type alias Model =
    { username : String
    , password : String
    , in_progress : Bool
    , error_message : Maybe String
    }


initialModel =
    Model "" "" False Nothing
