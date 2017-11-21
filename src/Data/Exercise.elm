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


type alias Exercise =
    { id : Int, name : String, type_ : ExerciseType }


exercise : Int -> String -> ExerciseType -> Exercise
exercise id name type_ =
    { id = id, name = name, type_ = type_ }


find : List Exercise -> Int -> Maybe Exercise
find exercises id =
    Utils.find (.id >> (==) id) exercises


decoder : Decoder Exercise
decoder =
    Decode.map3 Exercise
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string |> Decode.andThen decodeType)


decodeType : String -> Decoder ExerciseType
decodeType str =
    if str == "REPS" then
        Decode.succeed JustReps
    else if str == "REPS*WEIGHT" then
        Decode.succeed Weighted
    else
        Decode.fail ("Unknown type: " ++ str)


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
