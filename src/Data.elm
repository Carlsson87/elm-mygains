module Data exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Json.Decode as Decode
import Json.Encode as Encode
import Utils
    exposing
        ( dateToString
        , isJust
        , removeFromArray
        , traverseArray
        , updateInArray
        )


type alias Exercise =
    { id : Int
    , name : String
    }


type alias Set =
    { exercise : Exercise
    , reps_input : String
    , reps : Maybe Int
    , weight_input : String
    , weight : Maybe Float
    }


type alias ValidSet =
    { exercise_id : Int
    , reps : Int
    , weight : Float
    }


type alias EditableWorkout =
    { sets : Array Set
    }


type alias ValidWorkout =
    { date : Date
    , sets : List ValidSet
    }


type alias SavedWorkout =
    { id : Int
    , date : Date
    , sets : List ValidSet
    }


exerciseDecoder : Decode.Decoder (List Exercise)
exerciseDecoder =
    Decode.list
        (Decode.map2 Exercise
            (Decode.field "id" Decode.int)
            (Decode.field "name" Decode.string)
        )


addSet : Exercise -> EditableWorkout -> EditableWorkout
addSet ex w =
    { w | sets = Array.push (newSet ex) w.sets }


cloneSet : Int -> EditableWorkout -> EditableWorkout
cloneSet index w =
    Array.get index w.sets
        |> Maybe.map (\set -> { w | sets = Array.push set w.sets })
        |> Maybe.withDefault w


removeSet : Int -> EditableWorkout -> EditableWorkout
removeSet index w =
    { w | sets = removeFromArray index w.sets }


updateReps : String -> Int -> EditableWorkout -> EditableWorkout
updateReps reps index w =
    let
        update s =
            { s | reps_input = reps, reps = Result.toMaybe (String.toInt reps) }
    in
    updateSet index update w


updateWeight : String -> Int -> EditableWorkout -> EditableWorkout
updateWeight weight index w =
    let
        update s =
            { s | weight_input = weight, weight = Result.toMaybe (String.toFloat weight) }
    in
    updateSet index update w


updateSet : Int -> (Set -> Set) -> EditableWorkout -> EditableWorkout
updateSet index f w =
    { w | sets = updateInArray index f w.sets }


toValidWorkout : EditableWorkout -> Date -> Maybe ValidWorkout
toValidWorkout workout date =
    traverseArray toValidSet workout.sets
        |> Maybe.map Array.toList
        |> Maybe.map (ValidWorkout date)


toValidSet : Set -> Maybe ValidSet
toValidSet { reps, weight, exercise } =
    Maybe.map2 (ValidSet exercise.id) reps weight


isValidSet : Set -> Bool
isValidSet { reps, weight } =
    isJust reps && isJust weight


workoutEncoder : ValidWorkout -> Encode.Value
workoutEncoder { date, sets } =
    Encode.object
        [ ( "date", Encode.string (dateToString date) )
        , ( "sets", Encode.list (List.map setEncoder sets) )
        ]


setEncoder : ValidSet -> Encode.Value
setEncoder { reps, weight, exercise_id } =
    Encode.object
        [ ( "exercise_id", Encode.int exercise_id )
        , ( "reps", Encode.int reps )
        , ( "weight", Encode.float weight )
        ]


newSet : Exercise -> Set
newSet ex =
    Set ex "" Nothing "" Nothing
