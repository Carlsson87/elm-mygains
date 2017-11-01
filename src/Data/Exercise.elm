module Data.Exercise exposing (..)

import Json.Decode as Decode
    exposing
        ( Decoder
        , andThen
        , fail
        , field
        , int
        , map3
        , string
        , succeed
        )


type ExerciseType
    = Reps
    | RepsAndWeight


type alias Exercise =
    { id : Int
    , name : String
    , type_ : ExerciseType
    }


justReps : Int -> String -> Exercise
justReps id name =
    Exercise id name Reps


weighted : Int -> String -> Exercise
weighted id name =
    Exercise id name RepsAndWeight


exerciseDecoder : Decoder Exercise
exerciseDecoder =
    let
        typeDecoder =
            string
                |> andThen
                    (\typ ->
                        case typ of
                            "REPS" ->
                                succeed Reps

                            "REPS*WEIGHT" ->
                                succeed RepsAndWeight

                            _ ->
                                fail ("Unknown type " ++ typ)
                    )
    in
    map3 Exercise
        (field "id" int)
        (field "name" string)
        (field "type" typeDecoder)
