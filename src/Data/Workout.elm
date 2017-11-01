module Data.Workout
    exposing
        ( CompletedWorkout
        , Set(..)
        , Workout
        , addEmptySet
        , copySet
        , decodeWorkout
        , empty
        , encodeWorkout
        , exercise
        , isInWorkout
        , isValidSet
        , isValidWorkout
        , makeEditable
        , removeSet
        , sets
        , updateReps
        , updateWeight
        )

import Array exposing (Array)
import Data.Exercise exposing (Exercise, ExerciseType(..))
import Data.Set exposing (Reps, Weight, reps, updateReps, updateWeight, weight)
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder, field, float, int, list, map2, map3, string)
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Utils
    exposing
        ( dateToString
        , insertInArray
        , isJust
        , removeFromArray
        , traverseArray
        , updateInArray
        )


type Set
    = JustReps ( Exercise, Reps {} )
    | Weighted ( Exercise, Reps (Weight {}) )


type Workout
    = Workout (Array Set)


type alias CompletedWorkout =
    { date : Date
    , sets : List CompletedSet
    }


type alias CompletedSet =
    { exercise_id : Int
    , reps : Int
    , weight : Float
    }


makeEditable : List Exercise -> CompletedWorkout -> Maybe Workout
makeEditable exercises { sets } =
    let
        findEx : Int -> List Exercise -> Maybe Exercise
        findEx id exs =
            List.filter (\ex -> ex.id == id) exs
                |> List.head

        makeEditableSet : CompletedSet -> Maybe Set
        makeEditableSet set =
            findEx set.exercise_id exercises
                |> Maybe.map
                    (\ex ->
                        case ex.type_ of
                            Reps ->
                                JustReps ( ex, { reps = Just set.reps, reps_input = toString set.reps } )

                            RepsAndWeight ->
                                Weighted
                                    ( ex
                                    , { reps = Just set.reps
                                      , weight = Just set.weight
                                      , reps_input = toString set.reps
                                      , weight_input = toString set.weight
                                      }
                                    )
                    )
    in
    traverseArray makeEditableSet (Array.fromList sets)
        |> Maybe.map Workout


empty : Workout
empty =
    Workout Array.empty


emptySet : Exercise -> Set
emptySet ({ type_ } as e) =
    case type_ of
        Reps ->
            JustReps ( e, { reps = Nothing, reps_input = "" } )

        RepsAndWeight ->
            Weighted ( e, { reps = Nothing, reps_input = "", weight = Nothing, weight_input = "" } )


sets : Workout -> List Set
sets (Workout sets) =
    Array.toList sets


addSet : Set -> Workout -> Workout
addSet set (Workout sets) =
    Workout (Array.push set sets)


addEmptySet : Exercise -> Workout -> Workout
addEmptySet e w =
    addSet (emptySet e) w


copySet : Int -> Workout -> Workout
copySet index ((Workout sets) as w) =
    Array.get index sets
        |> Maybe.map (insertInArray index sets >> Workout)
        |> Maybe.withDefault w


removeSet : Int -> Workout -> Workout
removeSet index (Workout sets) =
    Workout (removeFromArray index sets)


updateSet : Int -> (Set -> Set) -> Workout -> Workout
updateSet index f (Workout sets) =
    Workout (updateInArray index f sets)


updateReps : String -> Int -> Workout -> Workout
updateReps reps index w =
    let
        update set =
            case set of
                JustReps ( e, s ) ->
                    JustReps ( e, Data.Set.updateReps reps s )

                Weighted ( e, s ) ->
                    Weighted ( e, Data.Set.updateReps reps s )
    in
    updateSet index update w


updateWeight : String -> Int -> Workout -> Workout
updateWeight weight index w =
    let
        update set =
            case set of
                JustReps _ ->
                    set

                Weighted ( e, s ) ->
                    Weighted ( e, Data.Set.updateWeight weight s )
    in
    updateSet index update w


decodeWorkout : Decoder CompletedWorkout
decodeWorkout =
    map2 CompletedWorkout
        (field "date" decodeDate)
        (field "sets" (list decodeSet))


decodeDate : Decoder Date
decodeDate =
    string
        |> Decode.map Date.fromString
        |> Decode.map (Result.map Decode.succeed)
        |> Decode.andThen (Result.withDefault (Decode.fail "no date"))


decodeSet : Decoder CompletedSet
decodeSet =
    map3 CompletedSet
        (field "exercise_id" int)
        (field "reps" int)
        (field "weight" float)


encodeWorkout : Date -> Workout -> Maybe Encode.Value
encodeWorkout date (Workout sets) =
    traverseArray setEncoder sets
        |> Maybe.map
            (\sets ->
                Encode.object
                    [ ( "date", Encode.string (dateToString date) )
                    , ( "sets", Encode.list (Array.toList sets) )
                    ]
            )


setEncoder : Set -> Maybe Encode.Value
setEncoder set =
    let
        encode id =
            Maybe.map2
                (\r w ->
                    Encode.object
                        [ ( "exercise_id", Encode.int id )
                        , ( "reps", Encode.int r )
                        , ( "weight", Encode.float w )
                        ]
                )
    in
    case set of
        JustReps ( { id }, { reps } ) ->
            encode id reps (Just 0)

        Weighted ( { id }, { reps, weight } ) ->
            encode id reps weight


isValidSet : Set -> Bool
isValidSet set =
    case set of
        JustReps ( _, { reps } ) ->
            isJust reps

        Weighted ( _, { reps, weight } ) ->
            isJust reps && isJust weight


isValidWorkout : Workout -> Bool
isValidWorkout (Workout sets) =
    not (Array.isEmpty sets)
        && List.all isValidSet (Array.toList sets)


exercise : Set -> Exercise
exercise set =
    case set of
        JustReps ( ex, _ ) ->
            ex

        Weighted ( ex, _ ) ->
            ex


isInWorkout : Workout -> Exercise -> Bool
isInWorkout (Workout sets) ex =
    let
        same set found =
            if found then
                True
            else
                ex == exercise set
    in
    Array.foldl same False sets
