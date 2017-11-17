module Data.History exposing (..)


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
