module Editor.Editor
    exposing
        ( Msg(..)
        , State
        , Workout
        , empty
        , encodeWorkout
        , exerciseList
        , fromString
        , isValid
        , setList
        , sets
        , state
        , toString
        , update
        )

import Array exposing (Array)
import Colors
import Data.Exercise as Exercise exposing (Exercise)
import Date exposing (Date)
import Editor.Set as Set exposing (Set)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Html.Keyed
import Icon
import Json.Decode as Decode exposing (Decoder, field, float, int, list, map2, map3, string)
import Json.Encode as Encode
import Maybe exposing (Maybe)
import Process
import Task
import Utils
import View.Form as Form


type Msg
    = AddSet (Exercise {})
    | SetWasAdded Int
    | CloneSet Int
    | SetWasCloned Int
    | RemoveSet Int
    | SetWasRemoved Int
    | UpdateSet Int Set
    | TransitionEnded


type Workout
    = Workout State


type alias State =
    { sets : List ( Int, Set )
    , nextId : Int
    , transition : Maybe Transition
    }


type Transition
    = CloningSet Int
    | ClonedSet Int
    | AddingSet Int
    | AddedSet Int
    | RemovingSet Int


update : Msg -> Workout -> ( Workout, Cmd Msg )
update msg (Workout ({ sets, nextId, transition } as state)) =
    case msg of
        AddSet ex ->
            ( Workout
                { sets = sets ++ [ ( nextId, Set.emptySet ex ) ]
                , nextId = nextId + 1
                , transition = Just (AddingSet (List.length sets))
                }
            , Process.sleep 17 |> Task.perform (always (SetWasAdded (List.length sets)))
            )

        SetWasAdded index ->
            ( Workout { state | transition = Just (AddedSet index) }, Cmd.none )

        UpdateSet index set ->
            ( Workout
                { state
                    | sets =
                        List.indexedMap
                            (\i ( id, s ) ->
                                if i == index then
                                    ( id, set )
                                else
                                    ( id, s )
                            )
                            sets
                }
            , Cmd.none
            )

        CloneSet index ->
            let
                copyIndex i set =
                    if i == index then
                        [ set, Tuple.mapFirst (always nextId) set ]
                    else
                        [ set ]
            in
            ( Workout
                { nextId = nextId + 1
                , transition = Just (CloningSet (index + 1))
                , sets =
                    sets
                        |> List.indexedMap copyIndex
                        |> List.concat
                }
            , Process.sleep 17 |> Task.perform (always (SetWasCloned index))
            )

        SetWasCloned index ->
            ( Workout { state | transition = Just (ClonedSet index) }, Cmd.none )

        RemoveSet index ->
            ( Workout { state | transition = Just (RemovingSet index) }
            , Process.sleep 200 |> Task.perform (always (SetWasRemoved index))
            )

        SetWasRemoved index ->
            ( Workout
                { state
                    | sets =
                        List.append
                            (List.take index sets)
                            (List.drop (index + 1) sets)
                    , transition = Nothing
                }
            , Cmd.none
            )

        TransitionEnded ->
            ( Workout { state | transition = Nothing }
            , Cmd.none
            )


fromSets : List Set -> Workout
fromSets sets =
    Workout
        { sets = List.indexedMap (,) sets
        , nextId = List.length sets
        , transition = Nothing
        }


empty : Workout
empty =
    fromSets []


sets : Workout -> List ( Int, Set )
sets (Workout { sets }) =
    sets


state : Workout -> State
state (Workout state) =
    state


isValid : Workout -> Bool
isValid (Workout { sets }) =
    not (List.isEmpty sets)
        && List.all (Tuple.second >> .set >> Set.isValid) sets


toString : Workout -> String
toString workout =
    sets workout
        |> List.map Tuple.second
        |> List.map Set.toString
        |> Encode.list
        |> Encode.encode 0


fromString : List (Exercise {}) -> String -> Result String Workout
fromString exercises str =
    Decode.decodeString (Decode.map fromSets (Decode.list (Set.decode exercises))) str


encodeWorkout : Date -> Workout -> Maybe Encode.Value
encodeWorkout date workout =
    Array.fromList (sets workout)
        |> Array.map Tuple.second
        |> Utils.traverseArray setEncoder
        |> Maybe.map
            (Encode.array
                >> (,) "sets"
                >> List.singleton
                >> (::) ( "date", Encode.string (Utils.dateToString date) )
                >> Encode.object
            )


setEncoder : Set -> Maybe Encode.Value
setEncoder { id, set } =
    let
        encoder id reps weight =
            Encode.object
                [ ( "exercise_id", Encode.int id )
                , ( "reps", Encode.int reps )
                , ( "weight", Encode.float weight )
                ]
    in
    case set of
        Set.JustReps { reps } ->
            Maybe.map2 (encoder id) reps (Just 0)

        Set.Weighted { reps, weight } ->
            Maybe.map2 (encoder id) reps weight


setList : Workout -> Html Msg
setList (Workout { sets, transition }) =
    let
        renderSet : Int -> ( Int, Set ) -> ( String, Html Msg )
        renderSet index ( id, { name, set } as workSet ) =
            let
                noop =
                    Events.on "noop" (Decode.fail "")

                ( handler, style ) =
                    case transition of
                        Just (RemovingSet i) ->
                            if i == index then
                                ( noop
                                , [ ( "transform", "translateX(-100vh)" )
                                  , ( "transition", "transform 0.2s" )
                                  ]
                                )
                            else if i < index then
                                ( noop
                                , [ ( "transform", "translateY(-100%)" )
                                  , ( "transition", "transform 0.2s" )
                                  ]
                                )
                            else
                                ( noop, [] )

                        Just (CloningSet i) ->
                            if i <= index then
                                ( noop
                                , [ ( "transform", "translateY(-100%)" )
                                  ]
                                )
                            else
                                ( noop, [] )

                        Just (ClonedSet i) ->
                            if i <= index then
                                ( if i == index then
                                    Utils.onTransitionEnd TransitionEnded
                                  else
                                    noop
                                , [ ( "transition", "transform 0.2s" )
                                  ]
                                )
                            else
                                ( noop, [] )

                        Just (AddingSet i) ->
                            if i == index then
                                ( noop
                                , [ ( "transform", "translateX(100vh)" )
                                  ]
                                )
                            else
                                ( noop, [] )

                        Just (AddedSet i) ->
                            if i == index then
                                ( Utils.onTransitionEnd TransitionEnded
                                , [ ( "transition", "transform 0.2s" )
                                  ]
                                )
                            else
                                ( noop, [] )

                        _ ->
                            ( noop, [] )
            in
            ( Basics.toString id
            , Html.div
                [ Attr.class "bg-white m-xs shadow-sm pb-md relative"
                , Attr.style (( "z-index", List.length sets - index |> Basics.toString ) :: style)
                , handler
                ]
                [ Html.h5
                    [ Attr.class "uc fz-sm ls-sm color-silver"
                    , Attr.style
                        [ ( "line-height", "24px" )
                        , ( "margin-left", "6px" )
                        ]
                    ]
                    [ Html.text (Basics.toString (index + 1) ++ ". " ++ name) ]
                , Html.div
                    [ Attr.class "flex" ]
                    [ Html.div
                        [ Attr.class "flex-fill pl-sm pr-sm"
                        ]
                        (case set of
                            Set.JustReps ({ reps_input } as set) ->
                                [ Form.numberInput reps_input
                                    (flip Set.updateReps set
                                        >> Set.JustReps
                                        >> flip Set.replaceSet workSet
                                        >> UpdateSet index
                                    )
                                ]

                            Set.Weighted ({ reps_input, weight_input } as set) ->
                                [ Form.numberInput reps_input
                                    (flip Set.updateReps set
                                        >> Set.Weighted
                                        >> flip Set.replaceSet workSet
                                        >> UpdateSet index
                                    )
                                , Form.numberInput weight_input
                                    (flip Set.updateWeight set
                                        >> Set.Weighted
                                        >> flip Set.replaceSet workSet
                                        >> UpdateSet index
                                    )
                                ]
                        )
                    , Html.div
                        [ Attr.class "flex-fit"
                        , Attr.style
                            [ ( "padding", "0 8px" )
                            ]
                        ]
                        [ Form.iconButton (Icon.clone Colors.silver 16) (CloneSet index)
                        , Form.iconButton (Icon.times Colors.silver 16) (RemoveSet index)
                        ]
                    ]
                ]
            )
    in
    Html.Keyed.node "div"
        [ Attr.class "relative"
        , Attr.style
            [ ( "z-index", "1" )
            ]
        ]
        (List.indexedMap renderSet sets)


exerciseList : List (Exercise {}) -> Html Msg
exerciseList exercises =
    Html.div
        []
        (List.map exerciseListItem exercises)


exerciseListItem : Exercise {} -> Html Msg
exerciseListItem exercise =
    Html.button
        [ Attr.class "full-width text-left lh-xl pl-md fz-sm bold ls-sm bg-white active-bg-clouds no-outline square mb-sm"
        , Events.onClick (AddSet exercise)
        ]
        [ Html.text exercise.name
        ]
