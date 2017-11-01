module Data.Set exposing (..)

import Data.Exercise as Exercise exposing (..)


type alias Reps a =
    { a | reps : Maybe Int, reps_input : String }


type alias Weight a =
    { a | weight : Maybe Float, weight_input : String }


reps : Reps a -> Maybe Int
reps { reps } =
    reps


weight : Weight a -> Maybe Float
weight { weight } =
    weight


updateReps : String -> Reps a -> Reps a
updateReps str rec =
    { rec | reps = Result.toMaybe (String.toInt str), reps_input = str }


updateWeight : String -> Weight a -> Weight a
updateWeight str rec =
    { rec | weight = Result.toMaybe (String.toFloat str), weight_input = str }
