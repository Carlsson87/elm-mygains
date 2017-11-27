module Data.Exercise
    exposing
        ( Exercise
        , ExerciseRecord
        , decoder
        , encode
        , find
        , id
        , name
        )

import Data.Kind as Kind exposing (Kind(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Utils


type alias ExerciseRecord =
    { id : Int, name : String }


type alias Exercise =
    Kind ExerciseRecord ExerciseRecord


id : Exercise -> Int
id =
    Kind.fold .id .id


name : Exercise -> String
name =
    Kind.fold .name .name


find : List Exercise -> Int -> Maybe Exercise
find exercises id_ =
    Utils.find (id >> (==) id_) exercises


decoder : Decoder Exercise
decoder =
    let
        make id name type_ =
            case type_ of
                "REPS" ->
                    Decode.succeed (JustReps (ExerciseRecord id name))

                "REPS*WEIGHT" ->
                    Decode.succeed (Weighted (ExerciseRecord id name))

                _ ->
                    Decode.fail ("Unknown exercise type: " ++ type_)
    in
    Decode.map3 make
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)
        |> Decode.andThen identity


encode : String -> Kind () () -> Encode.Value
encode name type_ =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "type"
          , Encode.string
                (case type_ of
                    JustReps _ ->
                        "REPS"

                    Weighted _ ->
                        "REPS*WEIGHT"
                )
          )
        ]
