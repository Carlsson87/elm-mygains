module Data.History exposing (..)

import Data.Exercise as Exercise exposing (Exercise)
import Data.Kind as Kind exposing (Kind(..))
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Utils


type alias Set =
    { exerciseId : Int
    , reps : Int
    , weight : Int
    }


type alias History =
    Kind JustRepsStats WeightedStats


type alias JustRepsStats =
    { maxRepsSet : Int
    , totalReps : Int
    , totalSets : Int
    }


type alias WeightedStats =
    { maxVolumeSet : Float
    , maxWeightRep : Float
    , totalVolume : Float
    }


empty : Exercise -> History
empty =
    Kind.replace
        emptyJustRepsStats
        emptyWeightedStats


toStats : Exercise -> Set -> History
toStats exercise set =
    Kind.replace
        (toJustRepsStats set.reps)
        (toWeightedStats set.reps (toFloat set.weight))
        exercise


decoder : List Exercise -> Decode.Value -> Dict Int History
decoder exercises workouts =
    let
        map : Set -> Dict Int (List Set) -> Dict Int (List Set)
        map set dict =
            if Dict.member set.exerciseId dict then
                Dict.update set.exerciseId (Maybe.map ((::) set)) dict
            else
                Dict.insert set.exerciseId [ set ] dict

        sets : Dict Int (List Set)
        sets =
            Decode.decodeValue
                (Decode.list (Decode.field "sets" (Decode.list decodeSet))
                    |> Decode.map List.concat
                )
                workouts
                |> Result.withDefault []
                |> List.foldl map Dict.empty

        makeHistory : Exercise -> ( Int, History )
        makeHistory ex =
            case Dict.get (Exercise.id ex) sets of
                Just sets ->
                    ( Exercise.id ex
                    , List.map (toStats ex) sets
                        |> List.foldl concatHistory (empty ex)
                    )

                Nothing ->
                    ( Exercise.id ex, empty ex )

        -- |> Maybe.map (List.map (toStats ex))
    in
    List.map makeHistory exercises
        |> Dict.fromList


decodeSet : Decoder Set
decodeSet =
    Decode.map3 Set
        (Decode.field "exercise_id" Decode.int)
        (Decode.field "reps" Decode.int)
        (Decode.field "weight" Decode.int)


emptyJustRepsStats =
    { maxRepsSet = 0
    , totalReps = 0
    , totalSets = 0
    }


emptyWeightedStats =
    { maxVolumeSet = 0
    , maxWeightRep = 0
    , totalVolume = 0
    }


toJustRepsStats : Int -> JustRepsStats
toJustRepsStats reps =
    JustRepsStats reps reps 1


toWeightedStats : Int -> Float -> WeightedStats
toWeightedStats reps weight =
    WeightedStats (toFloat reps * weight) weight (toFloat reps * weight)


concatJustRepsStats : JustRepsStats -> JustRepsStats -> JustRepsStats
concatJustRepsStats a b =
    { maxRepsSet = max a.maxRepsSet b.maxRepsSet
    , totalReps = a.totalReps + b.totalReps
    , totalSets = a.totalSets + b.totalSets
    }


concatWeightedStats : WeightedStats -> WeightedStats -> WeightedStats
concatWeightedStats a b =
    { maxVolumeSet = max a.maxVolumeSet b.maxVolumeSet
    , maxWeightRep = max a.maxWeightRep b.maxWeightRep
    , totalVolume = a.totalVolume + b.totalVolume
    }


concatHistory : History -> History -> History
concatHistory h1 h2 =
    case ( h1, h2 ) of
        ( JustReps a, JustReps b ) ->
            JustReps (concatJustRepsStats a b)

        ( Weighted a, Weighted b ) ->
            Weighted (concatWeightedStats a b)

        _ ->
            JustReps emptyJustRepsStats
