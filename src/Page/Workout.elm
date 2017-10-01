module Page.Workout exposing (Model, initialModel)

import Array exposing (Array)
import Data exposing (Exercise)


type alias Model =
    { sets : Array Set
    , sidebarOpen : Bool
    }


type alias Set =
    { exercise : Exercise
    , reps_input : String
    , reps : Maybe Int
    , weight_input : String
    , weight : Maybe Float
    }


initialModel =
    Model Array.empty False
