module Editor.Set
    exposing
        ( Reps
        , Set
        , Weight
        , decode
        , emptySet
        , exercise
        , isValid
        , makeJustReps
        , makeWeighted
        , toString
        , updateJustReps
        , updateReps
        , updateWeight
        )

import Data.Exercise as Exercise exposing (Exercise, ExerciseRecord)
import Data.Kind as Kind exposing (Kind(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Tuple
import Utils exposing (stringToFloat, stringToInt)


type alias Reps a =
    { a | reps : Maybe Int, reps_input : String }


type alias Weight a =
    { a | weight : Maybe Float, weight_input : String }


type alias Set =
    Kind ( Exercise, Reps {} ) ( Exercise, Reps (Weight {}) )


updateJustReps : (Reps {} -> Reps {}) -> Kind ( Exercise, Reps {} ) a -> Kind ( Exercise, Reps {} ) a
updateJustReps f =
    Kind.mapJustReps (Tuple.mapSecond f)


emptySet : Exercise -> Set
emptySet exercise =
    Kind.replace
        ( exercise, makeJustReps "" )
        ( exercise, makeWeighted "" "" )
        exercise


makeJustReps : String -> Reps {}
makeJustReps reps =
    { reps = stringToInt reps
    , reps_input = reps
    }


makeWeighted : String -> String -> Reps (Weight {})
makeWeighted reps weight =
    { reps = stringToInt reps
    , reps_input = reps
    , weight = stringToFloat weight
    , weight_input = weight
    }


exercise : Set -> Exercise
exercise =
    Kind.fold Tuple.first Tuple.first


updateReps : String -> Reps a -> Reps a
updateReps str rec =
    { rec
        | reps = Result.toMaybe (String.toInt str)
        , reps_input = str
    }


updateWeight : String -> Weight a -> Weight a
updateWeight str rec =
    { rec
        | weight = Result.toMaybe (String.toFloat str)
        , weight_input = str
    }


isValid : Set -> Bool
isValid set =
    case set of
        JustReps ( _, { reps } ) ->
            Utils.isJust reps

        Weighted ( _, { reps, weight } ) ->
            Utils.isJust reps && Utils.isJust weight


toString : Set -> Encode.Value
toString set =
    case set of
        JustReps ( exercise, { reps_input } ) ->
            Encode.object
                [ ( "exercise_id", Encode.int (Exercise.id exercise) )
                , ( "reps", Encode.string reps_input )
                ]

        Weighted ( exercise, { reps_input, weight_input } ) ->
            Encode.object
                [ ( "exercise_id", Encode.int (Exercise.id exercise) )
                , ( "reps", Encode.string reps_input )
                , ( "weight", Encode.string weight_input )
                ]


decode : List Exercise -> Decoder Set
decode exercises =
    Decode.field "exercise_id" Decode.int
        |> Decode.andThen
            (Exercise.find exercises
                >> Maybe.map decodeSetType
                >> Maybe.withDefault (Decode.fail "")
            )


decodeSetType : Exercise -> Decoder Set
decodeSetType exercise =
    let
        justReps reps =
            JustReps ( exercise, makeJustReps reps )

        weighted reps weight =
            Weighted ( exercise, makeWeighted reps weight )
    in
    Kind.fold
        (always (Decode.map justReps (Decode.field "reps" Decode.string)))
        (always
            (Decode.map2 weighted
                (Decode.field "reps" Decode.string)
                (Decode.field "weight" Decode.string)
            )
        )
        exercise
