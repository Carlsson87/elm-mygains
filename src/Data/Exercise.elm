module Data.Exercise
    exposing
        ( Exercise
        , ExerciseType(..)
        , decoder
        , encode
        , find
        )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Utils


type ExerciseType
    = JustReps
    | Weighted


type alias Exercise a =
    { a | id : Int, name : String, type_ : ExerciseType }


find : List (Exercise a) -> Int -> Maybe (Exercise a)
find exercises id =
    Utils.find (.id >> (==) id) exercises


decoder : Decoder (Exercise {})
decoder =
    Decode.map3 makeExercise
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


decodeType : String -> Decoder ExerciseType
decodeType str =
    case str of
        "REPS" ->
            Decode.succeed JustReps

        "REPS*WEIGHT" ->
            Decode.succeed Weighted

        _ ->
            Decode.fail ("Unknown type: " ++ str)


makeExercise : Int -> String -> ExerciseType -> Exercise {}
makeExercise id name type_ =
    { id = id
    , name = name
    , type_ = type_
    }


encode : String -> ExerciseType -> Encode.Value
encode name type_ =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "type"
          , Encode.string
                (case type_ of
                    JustReps ->
                        "REPS"

                    Weighted ->
                        "REPS*WEIGHT"
                )
          )
        ]
