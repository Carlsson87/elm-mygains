port module Main exposing (..)

import Array exposing (Array)
import Colors
import Data.Exercise as Exercise exposing (Exercise)
import Data.History as History
import Data.Kind as Kind exposing (Kind)
import Date exposing (Date)
import Dict exposing (Dict)
import Editor.Editor as Editor exposing (Workout)
import Html exposing (Html, programWithFlags)
import Html.Attributes exposing (class, disabled, min, pattern, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Requests exposing (createWorkout, getExercises, getWorkouts, login, refreshToken)
import Task
import Time
import Utils exposing (..)
import View.Form as Form
import View.Modal as Modal


-- Ports


port remember : ( String, String ) -> Cmd msg


port forget : String -> Cmd msg



-- Main


main : Program Decode.Value Model Msg
main =
    programWithFlags { init = init, view = view, update = update, subscriptions = always Sub.none }



-- Model


type Model
    = LoggedOut LoggedOutState
    | Loading LoadingState
    | LoggedIn LoggedInState


type alias LoggedOutState =
    { email : String
    , password : String
    , logging_in : Bool
    , error : Maybe String
    , workout : Decode.Value
    }


type alias LoadingState =
    { token : String
    , exercises : Maybe (List Exercise)
    , workout : Decode.Value
    , date : Maybe Date
    }


type alias LoggedInState =
    { token : String
    , exercises : List Exercise
    , workout : Workout
    , date : Date
    , sidebarIsOpen : Bool
    , modal : Maybe (Modal.Modal ModalContent)
    , history : Dict Int History.History
    }



-- Msg


type Msg
    = Noop
    | LoggedInMsg LoggedInAction
    | LoadingMsg LoadingAction
    | LoggedOutMsg LoggedOutAction


type LoggedInAction
    = OpenModal ModalContent
    | UpdateModalState Modal.State
    | UpdateModal ModalContent
    | CloseModal
    | ToggleSidebar
    | SaveWorkout
    | WorkoutSaved
    | WorkoutMsg Editor.Msg
    | UpdateDate Date
    | SaveWorkoutFailed
    | SaveExercise String (Kind () ())
    | ExerciseSaved (Result Http.Error Exercise)


type LoadingAction
    = TokenRefreshSucceeded String
    | TokenRefreshFailed
    | LoadingData Date (List Exercise) Decode.Value
    | LoadingDataFailed


type LoggedOutAction
    = UpdateEmail String
    | UpdatePassword String
    | SendLoginRequest
    | LoginSucceeded String
    | LoginFailed


type ModalContent
    = WorkoutDone
    | CreateExercise { name : String, type_ : Kind () () }



-- Init


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        workout =
            Decode.decodeValue (Decode.field "workout" Decode.value) flags
                |> Result.withDefault (Encode.string "")
    in
    case Decode.decodeValue (Decode.field "api_token" Decode.string) flags of
        Ok token ->
            ( Loading (LoadingState token Nothing workout Nothing)
            , refreshToken token
                |> Http.toTask
                |> Task.attempt (Result.map TokenRefreshSucceeded >> Result.withDefault TokenRefreshFailed)
                |> Cmd.map LoadingMsg
            )

        Err _ ->
            ( LoggedOut (LoggedOutState "" "" False Nothing workout), Cmd.none )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LoggedInMsg msg, LoggedIn model ) ->
            loggedInUpdate msg model

        ( LoggedOutMsg msg, LoggedOut model ) ->
            loggedOutUpdate msg model

        ( LoadingMsg msg, Loading model ) ->
            loadingUpdate msg model

        ( _, _ ) ->
            let
                log =
                    Debug.log "Unrecognized msg model combo" ( msg, model )
            in
            ( model, Cmd.none )


loggedInUpdate : LoggedInAction -> LoggedInState -> ( Model, Cmd Msg )
loggedInUpdate msg model =
    case msg of
        -- WORKOUT
        WorkoutMsg msg ->
            let
                ( workout, cmd ) =
                    Editor.update msg model.workout
            in
            ( LoggedIn { model | workout = workout }
            , Cmd.batch
                [ remember ( "workout", Editor.toString workout )
                , Cmd.map (WorkoutMsg >> LoggedInMsg) cmd
                ]
            )

        -- MODALS
        OpenModal content ->
            ( LoggedIn { model | modal = Just (Modal.Modal Modal.IsOpening content) }
            , Process.sleep 17
                |> Task.perform (always (LoggedInMsg (UpdateModalState Modal.IsOpen)))
            )

        UpdateModalState state ->
            case state of
                Modal.IsClosed ->
                    ( LoggedIn { model | modal = Nothing }, Cmd.none )

                _ ->
                    ( LoggedIn { model | modal = Maybe.map (Modal.updateState state) model.modal }, Cmd.none )

        UpdateModal content ->
            ( LoggedIn { model | modal = Maybe.map (Modal.updateContent content) model.modal }, Cmd.none )

        CloseModal ->
            ( LoggedIn { model | modal = Maybe.map (Modal.updateState Modal.IsClosing) model.modal }, Cmd.none )

        ToggleSidebar ->
            ( LoggedIn { model | sidebarIsOpen = not model.sidebarIsOpen }, Cmd.none )

        SaveWorkout ->
            ( LoggedIn model
            , case Editor.encodeWorkout model.date model.workout of
                Just workout ->
                    createWorkout model.token workout
                        |> Http.toTask
                        |> Task.map (always WorkoutSaved)
                        |> Task.attempt (Result.withDefault SaveWorkoutFailed)
                        |> Cmd.map LoggedInMsg

                Nothing ->
                    Cmd.none
            )

        WorkoutSaved ->
            ( LoggedIn { model | workout = Editor.empty, modal = Nothing }, forget "workout" )

        SaveWorkoutFailed ->
            ( LoggedIn model, Cmd.none )

        UpdateDate date ->
            ( LoggedIn { model | date = date }, Cmd.none )

        SaveExercise name type_ ->
            ( LoggedIn model
            , Requests.createExercise model.token (Exercise.encode name type_)
                |> Http.toTask
                |> Task.attempt (ExerciseSaved >> LoggedInMsg)
            )

        ExerciseSaved (Ok exercise) ->
            ( LoggedIn { model | exercises = exercise :: model.exercises, modal = Nothing }, Cmd.none )

        ExerciseSaved (Err _) ->
            ( LoggedIn model, Cmd.none )


loadData : String -> Cmd Msg
loadData token =
    Task.map3 LoadingData
        Date.now
        (Http.toTask (getExercises token))
        (Http.toTask (getWorkouts token))
        |> Task.attempt (Result.withDefault LoadingDataFailed)
        |> Cmd.map LoadingMsg


loadingUpdate : LoadingAction -> LoadingState -> ( Model, Cmd Msg )
loadingUpdate msg model =
    case msg of
        TokenRefreshSucceeded token ->
            ( Loading { model | token = token }
            , loadData token
            )

        LoadingData date exercises workouts ->
            ( LoggedIn
                { token = model.token
                , exercises = exercises
                , workout =
                    Decode.decodeValue Decode.string model.workout
                        |> Result.andThen (Editor.fromString exercises)
                        |> Result.withDefault Editor.empty
                , date = date
                , sidebarIsOpen = False
                , modal = Nothing
                , history = Debug.log "History" (History.decoder exercises workouts)
                }
            , Cmd.none
            )

        LoadingDataFailed ->
            ( LoggedOut (LoggedOutState "" "" False (Just "Could not load your exercises, log in to try again.") model.workout), Cmd.none )

        TokenRefreshFailed ->
            ( LoggedOut (LoggedOutState "" "" False (Just "Your session has expired, you need to log back in.") model.workout), Cmd.none )


loggedOutUpdate : LoggedOutAction -> LoggedOutState -> ( Model, Cmd Msg )
loggedOutUpdate msg model =
    case msg of
        UpdateEmail str ->
            ( LoggedOut { model | email = str }, Cmd.none )

        UpdatePassword str ->
            ( LoggedOut { model | password = str }, Cmd.none )

        SendLoginRequest ->
            ( LoggedOut { model | logging_in = True }
            , login model.email model.password
                |> Http.toTask
                |> Task.map LoginSucceeded
                |> Task.attempt (Result.withDefault LoginFailed)
                |> Cmd.map LoggedOutMsg
            )

        LoginSucceeded token ->
            ( Loading (LoadingState token Nothing model.workout Nothing)
            , Cmd.batch
                [ remember ( "jwt", token )
                , loadData token
                ]
            )

        LoginFailed ->
            ( LoggedOut { model | error = Just "Cant log in" }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    case model of
        LoggedOut state ->
            Html.map LoggedOutMsg
                (loginForm state)

        Loading state ->
            Html.h1
                [ class "color-dark text-center mt-xl"
                ]
                [ Html.text "Loading..."
                ]

        LoggedIn state ->
            Html.map LoggedInMsg
                (layer 0
                    [ layer 0 [ content state ]
                    , layer 1 [ navbar state ]
                    , layer 2 [ sidebar state ]
                    , layer 3 (state.modal |> Maybe.map (modals state) |> Maybe.withDefault [])
                    ]
                )



---------------------


updateContent : ModalContent -> { a | content : ModalContent } -> { a | content : ModalContent }
updateContent content modal =
    { modal | content = content }


layer : Int -> List (Html msg) -> Html msg
layer z =
    Html.div
        [ class "relative"
        , style [ ( "z-index", Basics.toString z ) ]
        ]


modals : LoggedInState -> Modal.Modal ModalContent -> List (Html LoggedInAction)
modals model modal =
    case Modal.content modal of
        WorkoutDone ->
            [ Modal.view modal
                UpdateModalState
                [ Modal.title "Save workout"
                , Modal.body
                    [ Form.label "Date"
                    , Form.date model.date UpdateDate
                    ]
                , Modal.footer
                    [ Form.button "save" SaveWorkout
                    , Form.button "cancel" CloseModal
                    ]
                ]
            ]

        CreateExercise form ->
            let
                isReps =
                    Kind.fold
                        (always True)
                        (always False)
                        form.type_

                setName str =
                    UpdateModal (CreateExercise { form | name = str })
            in
            [ Modal.view modal
                UpdateModalState
                [ Modal.title "Create Exercise"
                , Modal.body
                    [ Form.label "Name"
                    , Form.input "text" form.name setName
                    , Form.label "Type"
                    , Form.radio isReps "Reps" (UpdateModal (CreateExercise { form | type_ = Kind.JustReps () }))
                    , Form.radio (not isReps) "Reps × Weight" (UpdateModal (CreateExercise { form | type_ = Kind.Weighted () }))
                    ]
                , Modal.footer
                    [ Form.button "save" (SaveExercise form.name form.type_)
                    , Form.button "cancel" CloseModal
                    ]
                ]
            ]


content : LoggedInState -> Html LoggedInAction
content model =
    Html.div
        [ style
            [ ( "padding", "58px 0" )
            ]
        ]
        [ Html.map WorkoutMsg (Editor.setList model.workout)
        ]


navbar : LoggedInState -> Html LoggedInAction
navbar model =
    Html.div
        [ class "fixed shadow-sm bg-white flex flex-align-center"
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
            [ saveButton (Editor.isValid model.workout)
            ]
        , Html.div
            [ class "flex-fill cf"
            ]
            [ Html.span
                [ class "float-right"
                ]
                [ Form.button "Add sets" ToggleSidebar
                ]
            ]
        ]


loginForm : LoggedOutState -> Html LoggedOutAction
loginForm { email, password, error } =
    Html.div
        [ class "bg-white shadow-sm m-md p-md"
        ]
        [ Modal.title "Sign in"
        , Modal.body
            [ Maybe.withDefault (Html.text "") (Maybe.map Form.info error)
            , Form.label "Email"
            , Form.email email UpdateEmail
            , Form.label "Password"
            , Form.password password UpdatePassword
            ]
        , Modal.footer
            [ Form.button "Sign in" SendLoginRequest
            ]
        ]


sidebar : LoggedInState -> Html LoggedInAction
sidebar { exercises, workout, sidebarIsOpen, history } =
    Html.div
        []
        [ Html.div
            [ style
                (if sidebarIsOpen then
                    [ ( "opacity", "0.24" )
                    ]
                 else
                    [ ( "pointer-events", "none" )
                    , ( "opacity", "0" )
                    ]
                )
            , class "fixed cover transition-opacity bg-black"
            , onClick ToggleSidebar
            ]
            []
        , Html.div
            [ class "bg-white fixed transition-transform shadow-sm overflow-scroll"
            , style
                [ ( "top", "0" )
                , ( "bottom", "0" )
                , ( "right", "-270px" )
                , ( "width", "270px" )
                , ( "transform"
                  , if sidebarIsOpen then
                        "translateX(-270px)"
                    else
                        ""
                  )
                ]
            ]
            [ Html.h2
                [ class "color-dark fx-lg p-md"
                ]
                [ Html.text "Exercises"
                ]
            , Html.map WorkoutMsg (Editor.exerciseList exercises history)
            , Form.blockButton "Create exercise" (OpenModal (CreateExercise { name = "", type_ = Kind.JustReps () }))
            ]
        ]


saveButton : Bool -> Html LoggedInAction
saveButton active =
    if active then
        Form.button "done" (OpenModal WorkoutDone)
    else
        Form.disabledButton "done"
