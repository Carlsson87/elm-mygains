module Editor.Set
    exposing
        ( Set(..)
        , decode
        , emptySet
        , exercise
        , isValid
        , toString
        , updateReps
        , updateWeight
        )

import Data.Exercise as Exercise exposing (Exercise)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Utils exposing (stringToFloat, stringToInt)


type alias Reps a =
    { a | reps : Maybe Int, reps_input : String }


type alias Weight a =
    { a | weight : Maybe Float, weight_input : String }


type Set
    = JustReps Exercise (Reps {})
    | Weighted Exercise (Reps (Weight {}))


exercise : Set -> Exercise
exercise set =
    case set of
        JustReps ex _ ->
            ex

        Weighted ex _ ->
            ex


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


emptySet : Exercise -> Set
emptySet exercise =
    case exercise.type_ of
        Exercise.JustReps ->
            makeJustReps exercise ""

        Exercise.Weighted ->
            makeWeighted exercise "" ""


makeJustReps : Exercise -> String -> Set
makeJustReps exercise reps =
    JustReps exercise
        { reps = stringToInt reps
        , reps_input = reps
        }


makeWeighted : Exercise -> String -> String -> Set
makeWeighted exercise reps weight =
    Weighted exercise
        { reps = stringToInt reps
        , reps_input = reps
        , weight = stringToFloat weight
        , weight_input = weight
        }


isValid : Set -> Bool
isValid set =
    case set of
        JustReps _ { reps } ->
            Utils.isJust reps

        Weighted _ { reps, weight } ->
            Utils.isJust reps && Utils.isJust weight


toString : Set -> Encode.Value
toString set =
    case set of
        JustReps { id } { reps_input } ->
            Encode.object
                [ ( "exercise_id", Encode.int id )
                , ( "reps", Encode.string reps_input )
                ]

        Weighted { id } { reps_input, weight_input } ->
            Encode.object
                [ ( "exercise_id", Encode.int id )
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
    case exercise.type_ of
        Exercise.JustReps ->
            Decode.map (makeJustReps exercise) (Decode.field "reps" Decode.string)

        Exercise.Weighted ->
            Decode.map2 (makeWeighted exercise) (Decode.field "reps" Decode.string) (Decode.field "weight" Decode.string)
