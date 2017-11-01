port module Main exposing (..)

import Array exposing (Array)
import Data.Exercise exposing (..)
import Data.Workout as Workout exposing (..)
import Date exposing (Date)
import Html exposing (Html, programWithFlags)
import Html.Attributes exposing (class, disabled, min, pattern, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Requests
    exposing
        ( createWorkout
        , getExercises
        , getWorkouts
        , login
        , refreshToken
        )
import Task
import Time
import Utils exposing (..)


-- Ports


port remember : ( String, String ) -> Cmd msg


port forget : String -> Cmd msg


rememberToken : String -> Cmd Msg
rememberToken token =
    remember ( "jwt", token )


forgetToken : Cmd Msg
forgetToken =
    forget "jwt"


main : Program Decode.Value Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Model
    = LoggedIn LoggedInState
    | LoggedOut LoggedOutState


type RemoteData a e
    = NotAsked
    | Loading
    | Success a
    | Error e


type SetAnimation
    = CloningSet Int
    | RemovingSet Int
    | AddingSet Int


type alias LoggedInState =
    { token : String
    , exercises : List Exercise
    , workout : Workout
    , date : Date
    , uiState : UIState
    , workoutSavedState : RemoteData CompletedWorkout String
    , setAnimation : Maybe SetAnimation
    }


type UIState
    = EditingWorkout
    | AddingSets
    | ConfirmingWorkout
    | SavingWorkout
    | WorkoutWasSaved
    | WorkoutWasNotSaved String


type alias LoggedOutState =
    { email : String
    , password : String
    , logging_in : Bool
    , error : Maybe String
    }


initialModel : Model
initialModel =
    LoggedOut (LoggedOutState "" "" False Nothing)


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        token : Maybe (Task.Task Http.Error String)
        token =
            flags
                |> Decode.decodeValue (Decode.field "api_token" Decode.string)
                |> Result.map refreshToken
                |> Result.toMaybe
                |> Maybe.map Http.toTask

        date : Task.Task x Date
        date =
            Date.now

        exercises : String -> Task.Task Http.Error (List Exercise)
        exercises =
            getExercises >> Http.toTask

        workout : List Exercise -> Workout
        workout exs =
            flags
                |> Decode.decodeValue (Decode.field "workout" Decode.string)
                |> Result.andThen (Decode.decodeString decodeWorkout)
                |> Result.toMaybe
                |> Maybe.andThen (makeEditable exs)
                |> Maybe.withDefault Workout.empty

        makeLogin : List Exercise -> Date -> String -> LoggedInState
        makeLogin exs date token =
            LoggedInState token exs (workout exs) date EditingWorkout NotAsked Nothing

        attemptLogin : String -> Task.Task Http.Error LoggedInState
        attemptLogin token =
            Task.map3 makeLogin (exercises token) date (Task.succeed token)
    in
    ( initialModel
    , Maybe.map (Task.andThen attemptLogin) token
        |> Maybe.map (Task.attempt (LoggedOutMsg << LoginCompleted))
        |> Maybe.withDefault Cmd.none
    )


type Msg
    = Noop
    | LoggedInMsg LoggedInAction
    | LoggedOutMsg LoggedOutAction


type LoggedInAction
    = TransitionTo UIState
    | AddSet Exercise
    | SetWasAdded
    | UpdateReps Int String
    | UpdateWeight Int String
    | CloneSet Int
    | SetWasCloned
    | RemoveSet Int
    | SetWasRemoved Int
    | SaveWorkout
    | WorkoutSaved
    | UpdateDate String


type LoggedOutAction
    = UpdateEmail String
    | UpdatePassword String
    | SendLoginRequest
    | LoginCompleted (Result Http.Error LoggedInState)


saveWorkout : String -> Date -> Workout -> Cmd Msg
saveWorkout token date workout =
    encodeWorkout date workout
        |> Maybe.map (createWorkout token)
        |> Maybe.map Http.toTask
        |> Maybe.withDefault (Task.succeed 0)
        |> Task.attempt
            (\res ->
                case res of
                    Err _ ->
                        LoggedInMsg (TransitionTo (WorkoutWasNotSaved "Could not save workout"))

                    Ok _ ->
                        LoggedInMsg WorkoutSaved
            )


rememberWorkout : Workout -> Date -> Cmd Msg
rememberWorkout w d =
    encodeWorkout d w
        |> Maybe.map (Encode.encode 0)
        |> Maybe.map (\str -> remember ( "workout", str ))
        |> Maybe.withDefault Cmd.none


forgetWorkout : Cmd Msg
forgetWorkout =
    forget "workout"


loggedInUpdate : LoggedInAction -> LoggedInState -> ( Model, Cmd Msg )
loggedInUpdate msg model =
    case msg of
        TransitionTo state ->
            ( LoggedIn { model | uiState = state }, Cmd.none )

        SaveWorkout ->
            ( LoggedIn { model | uiState = SavingWorkout }, saveWorkout model.token model.date model.workout )

        WorkoutSaved ->
            ( LoggedIn { model | workout = Workout.empty, uiState = WorkoutWasSaved }, forgetWorkout )

        AddSet ex ->
            let
                workout =
                    addEmptySet ex model.workout
            in
            ( LoggedIn { model | workout = workout, setAnimation = Just (AddingSet (List.length (Workout.sets workout) - 1)) }
            , Cmd.batch
                [ rememberWorkout workout model.date
                , delay 200 (LoggedInMsg SetWasAdded)
                ]
            )

        SetWasAdded ->
            ( LoggedIn { model | setAnimation = Nothing }, Cmd.none )

        UpdateReps index reps ->
            let
                workout =
                    updateReps reps index model.workout
            in
            ( LoggedIn { model | workout = workout }, rememberWorkout workout model.date )

        UpdateWeight index weight ->
            let
                workout =
                    updateWeight weight index model.workout
            in
            ( LoggedIn { model | workout = workout }, rememberWorkout workout model.date )

        CloneSet index ->
            let
                workout =
                    copySet index model.workout
            in
            ( LoggedIn { model | workout = workout, setAnimation = Just (CloningSet index) }
            , Cmd.batch
                [ rememberWorkout workout model.date
                , delay 200 (LoggedInMsg SetWasCloned)
                ]
            )

        SetWasCloned ->
            ( LoggedIn { model | setAnimation = Nothing }, Cmd.none )

        RemoveSet index ->
            ( LoggedIn { model | setAnimation = Just (RemovingSet index) }, delay 180 (LoggedInMsg (SetWasRemoved index)) )

        SetWasRemoved index ->
            let
                workout =
                    removeSet index model.workout
            in
            ( LoggedIn { model | workout = workout, setAnimation = Nothing }, rememberWorkout workout model.date )

        UpdateDate date ->
            ( LoggedIn { model | date = Result.withDefault model.date (Date.fromString date) }, Cmd.none )


delay : Float -> msg -> Cmd msg
delay ms msg =
    Process.sleep (ms * Time.millisecond)
        |> Task.perform (always msg)


loggedOutUpdate : LoggedOutAction -> LoggedOutState -> ( Model, Cmd Msg )
loggedOutUpdate msg model =
    case msg of
        UpdateEmail str ->
            ( LoggedOut { model | email = str }, Cmd.none )

        UpdatePassword str ->
            ( LoggedOut { model | password = str }, Cmd.none )

        SendLoginRequest ->
            let
                request =
                    login model.email model.password
                        |> Http.toTask
                        |> Task.andThen
                            (\token ->
                                Http.toTask (getExercises token)
                                    |> Task.andThen
                                        (\exs ->
                                            Date.now
                                                |> Task.map (\date -> LoggedInState token exs Workout.empty date EditingWorkout NotAsked Nothing)
                                        )
                            )
                        |> Task.attempt (LoggedOutMsg << LoginCompleted)
            in
            ( LoggedOut { model | logging_in = True }, request )

        LoginCompleted (Ok model) ->
            ( LoggedIn model, rememberToken model.token )

        LoginCompleted (Err _) ->
            ( LoggedOut { model | error = Just "Cant log in" }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoggedInMsg msg, LoggedIn model ) ->
            loggedInUpdate msg model

        ( LoggedOutMsg msg, LoggedOut model ) ->
            loggedOutUpdate msg model

        ( _, _ ) ->
            let
                log =
                    Debug.log "Unrecognized msg model combo" ( msg, model )
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        LoggedOut state ->
            Html.map LoggedOutMsg
                (Html.div
                    []
                    [ loginForm state
                    ]
                )

        LoggedIn model ->
            let
                sidebarOpen =
                    case model.uiState of
                        AddingSets ->
                            True

                        _ ->
                            False
            in
            Html.map LoggedInMsg
                (Html.div
                    [ class "relative"
                    , style
                        [ ( "padding-bottom", "100px" )
                        , ( "z-index", "0" )
                        ]
                    ]
                    [ setList (Workout.sets model.workout) model.setAnimation
                    , navbar model
                    , sidebar model.exercises model.workout sidebarOpen
                    , confirmWorkout model
                    ]
                )


navbar : LoggedInState -> Html LoggedInAction
navbar model =
    Html.div
        [ class "fixed shadow-1 bg-white flex flex-align-center"
        , style
            [ ( "top", "0" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "height", "58px" )
            , ( "padding", "0 10px" )
            , ( "z-index", "2" )
            ]
        ]
        [ Html.div
            [ class "flex-fit"
            ]
            [ saveButton (isValidWorkout model.workout)
            ]
        , Html.div
            [ class "flex-fill text-right"
            ]
            [ Html.button
                [ class "no-focus"
                , style
                    [ ( "height", "48px" )
                    , ( "width", "48px" )
                    , ( "background-color", "transparent" )
                    , ( "font-size", "1.5em" )
                    ]
                , onClick (TransitionTo AddingSets)
                ]
                [ Html.text "+"
                ]
            ]
        ]


modal : Bool -> List (Html msg) -> Html msg
modal visible =
    Html.div
        [ class "fixed transition-transform bg-white padding-1 shadow-2"
        , style
            [ ( "z-index", "2" )
            , ( "top", "50px" )
            , ( "width", "250px" )
            , ( "left", "50vw" )
            , ( "margin-left", "-125px" )
            , ( "transform"
              , if visible then
                    "translateY(0)"
                else
                    "translateY(-400px"
              )
            ]
        ]


confirmWorkout { uiState, date } =
    let
        ( visible, content ) =
            case uiState of
                ConfirmingWorkout ->
                    ( True
                    , [ Html.input
                            [ type_ "date"
                            , class "font-size-1 padding-2"
                            , value (dateToString date)
                            , onInput UpdateDate
                            ]
                            []
                      , Html.button
                            [ class (successLinkBtn ++ " block full-width no-focus")
                            , onClick SaveWorkout
                            ]
                            [ Html.text "save the workout" ]
                      ]
                    )

                SavingWorkout ->
                    ( True
                    , [ Html.button
                            [ class (disabledLinkBtn ++ " block full-width no-focus")
                            ]
                            [ Html.text "saving..." ]
                      ]
                    )

                WorkoutWasSaved ->
                    ( True
                    , [ Html.h5 [] [ Html.text "Your workout was saved" ]
                      , Html.button
                            [ class (disabledLinkBtn ++ " block full-width no-focus")
                            , onClick (TransitionTo EditingWorkout)
                            ]
                            [ Html.text "OK" ]
                      ]
                    )

                _ ->
                    ( False, [] )
    in
    Html.div
        [ style [ ( "z-index", "2" ) ], class "relative" ]
        [ Html.div
            [ class "fixed cover bg-trans-1 transition-opacity"
            , style
                [ ( "z-index", "1" )
                , ( "opacity"
                  , if visible then
                        "1"
                    else
                        "0"
                  )
                , ( "bottom"
                  , if visible then
                        "0"
                    else
                        "auto"
                  )
                ]
            ]
            []
        , modal visible content
        ]


loginForm : LoggedOutState -> Html LoggedOutAction
loginForm { email, password, error } =
    Html.div
        []
        [ Html.div
            [ class "fixed cover bg-trans-1 transition-opacity"
            , style
                [ ( "z-index", "1" )
                , ( "opacity", "0" )
                , ( "bottom", "0" )
                ]
            ]
            []
        , modal True
            [ Html.text (Maybe.withDefault "" error)
            , Html.input
                [ type_ "email"
                , class "block full-width padding-1 margin-b-1 border-gray border-radius-1"
                , value email
                , onInput UpdateEmail
                ]
                []
            , Html.input
                [ type_ "password"
                , class "block full-width padding-1 margin-b-1 border-gray border-radius-1"
                , value password
                , onInput UpdatePassword
                ]
                []
            , Html.button
                [ onClick SendLoginRequest
                , class (successLinkBtn ++ " block full-width")
                ]
                [ Html.text "Log in" ]
            ]
        ]


sidebar : List Exercise -> Workout -> Bool -> Html LoggedInAction
sidebar exercises workout isOpen =
    Html.div
        [ class "relative"
        , style [ ( "z-index", "3" ) ]
        ]
        [ Html.div
            [ class "fixed bg-trans-1 transition-opacity"
            , onClick (TransitionTo EditingWorkout)
            , style
                [ ( "top", "0" )
                , ( "left", "0" )
                , ( "right", "0" )
                , ( "bottom"
                  , if isOpen then
                        "0"
                    else
                        ""
                  )
                , ( "opacity"
                  , if isOpen then
                        "1"
                    else
                        "0"
                  )
                ]
            ]
            []
        , Html.div
            [ class "bg-white fixed transition-transform shadow-1 overflow-scroll"
            , style
                [ ( "top", "0" )
                , ( "bottom", "0" )
                , ( "right", "-210px" )
                , ( "width", "200px" )
                , ( "transform"
                  , if isOpen then
                        "translateX(-210px)"
                    else
                        ""
                  )
                ]
            ]
            [ exerciseList exercises
            ]
        ]


btnClass : String
btnClass =
    String.join " "
        [ "padding-1"
        , "border-radius-1"
        , "bold"
        , "uppercase"
        , "letter-spacing-2"
        , "bg-white"
        ]


successLinkBtn =
    btnClass ++ " color-success"


disabledLinkBtn =
    btnClass ++ " color-gray-1 cursor-disabled"


saveButton : Bool -> Html LoggedInAction
saveButton active =
    let
        attrs =
            if active then
                [ class (successLinkBtn ++ " no-focus")
                , onClick (TransitionTo ConfirmingWorkout)
                ]
            else
                [ class (disabledLinkBtn ++ " no-focus")
                , disabled True
                ]
    in
    Html.button
        attrs
        [ Html.text "Save" ]


exerciseList : List Exercise -> Html LoggedInAction
exerciseList exercises =
    Html.div
        [ class "margin-b-2"
        ]
        (List.map
            addExerciseButton
            exercises
        )


primaryButton : String -> msg -> Html msg
primaryButton text handler =
    Html.button
        [ onClick handler
        , class "button"
        ]
        [ Html.text text ]


addExerciseButton : Exercise -> Html LoggedInAction
addExerciseButton ({ name } as ex) =
    Html.button
        [ class "button block full-width text-left"
        , onClick (AddSet ex)
        ]
        [ Html.text name
        ]


setList : List Set -> Maybe SetAnimation -> Html LoggedInAction
setList sets animation =
    Html.div
        [ style
            [ ( "margin-top", "62px" )
            , ( "z-index", "1" )
            ]
        ]
        (List.indexedMap (setItem animation) sets)


setItem : Maybe SetAnimation -> Int -> Set -> Html LoggedInAction
setItem animation index set =
    let
        inputs =
            case set of
                JustReps ( { name }, { reps_input } ) ->
                    [ numberInput reps_input (UpdateReps index)
                    ]

                Weighted ( { name }, { reps_input, weight_input } ) ->
                    [ numberInput reps_input (UpdateReps index)
                    , numberInput weight_input (UpdateWeight index)
                    ]

        classes =
            case animation of
                Just (AddingSet i) ->
                    if i == index then
                        "add-set"
                    else
                        ""

                Just (CloningSet i) ->
                    if i < index then
                        "copy-set"
                    else
                        ""

                Just (RemovingSet i) ->
                    if i == index then
                        "remove-set"
                    else if i < index then
                        "copy-set a-reverse"
                    else
                        ""

                _ ->
                    ""
    in
    Html.div
        [ class classes
        , Html.Events.onDoubleClick (CloneSet index)
        ]
        [ Html.div
            [ class "bg-white margin-1 shadow-1"
            , style
                [ ( "z-index", "1" )
                , ( "padding-bottom", "16px" )
                ]
            ]
            [ Html.h5
                [ class "uppercase font-size-2 letter-spacing-2 color-gray-2"
                , style
                    [ ( "line-height", "24px" )
                    , ( "margin-left", "6px" )
                    ]
                ]
                [ Html.text (toString (index + 1) ++ ". " ++ .name (Workout.exercise set)) ]
            , Html.div
                [ class "flex" ]
                [ Html.div
                    [ class "flex-fill padding-h-2"
                    ]
                    inputs
                , Html.div
                    [ class "flex-fit"
                    , style
                        [ ( "padding", "0 8px" )
                        ]
                    ]
                    [ Html.button
                        [ style
                            [ ( "height", "48px" )
                            , ( "width", "48px" )
                            , ( "background-color", "transparent" )
                            ]
                        , class "no-focus"
                        , onClick (CloneSet index)
                        ]
                        [ Html.span [ class "icon-copy" ] []
                        ]
                    , Html.button
                        [ style
                            [ ( "height", "48px" )
                            , ( "width", "48px" )
                            , ( "background-color", "transparent" )
                            ]
                        , class "no-focus"
                        , onClick (RemoveSet index)
                        ]
                        [ Html.span [ class "icon-remove" ] []
                        ]
                    ]
                ]
            ]
        ]


numberInput : String -> (String -> msg) -> Html msg
numberInput val handler =
    Html.input
        [ type_ "number"
        , class "number-input"
        , pattern "[0-9]*"
        , Html.Attributes.min "0"
        , Html.Attributes.placeholder "#"
        , value val
        , onInput handler
        ]
        []
