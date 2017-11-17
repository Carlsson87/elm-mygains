port module Main exposing (..)

import Array exposing (Array)
import Colors
import Data.Exercise as Exercise exposing (Exercise, ExerciseType(..))
import Date exposing (Date)
import Editor.Editor as Editor exposing (Workout)
import Html exposing (Html, programWithFlags)
import Html.Attributes exposing (class, disabled, min, pattern, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Icon
import Json.Decode as Decode
import Json.Encode as Encode
import Process
import Requests exposing (createWorkout, getExercises, login, refreshToken)
import Task
import Time
import Utils exposing (..)
import View.Form as Form
import View.Modal as Modal


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
        , subscriptions = always Sub.none
        }


type Model
    = LoggedIn LoggedInState
    | LoggedOut LoggedOutState


type alias LoggedInState =
    { token : String
    , exercises : List (Exercise {})
    , workout : Workout
    , date : Date
    , sidebarIsOpen : Bool
    , modal : Maybe (Modal.Modal { content : ModalContent })
    }


updateContent : ModalContent -> { a | content : ModalContent } -> { a | content : ModalContent }
updateContent content modal =
    { modal | content = content }


type ModalContent
    = WorkoutDone
    | CreateExercise { name : String, type_ : Exercise.ExerciseType }


makeLoggedInState : String -> Workout -> List (Exercise {}) -> Date -> LoggedInState
makeLoggedInState token editor exercises date =
    LoggedInState token exercises editor date False Nothing


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

        exercises : String -> Task.Task Http.Error (List (Exercise {}))
        exercises =
            getExercises >> Http.toTask

        workout : List (Exercise {}) -> Workout
        workout exs =
            flags
                |> Decode.decodeValue (Decode.field "workout" Decode.string)
                |> Result.andThen (Editor.fromString exs)
                |> Result.withDefault Editor.empty

        makeLogin : List (Exercise {}) -> Date -> String -> LoggedInState
        makeLogin exs date token =
            makeLoggedInState token (workout exs) exs date

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
    | SaveExercise String ExerciseType
    | ExerciseSaved (Result Http.Error (Exercise {}))


type LoggedOutAction
    = UpdateEmail String
    | UpdatePassword String
    | SendLoginRequest
    | LoginCompleted (Result Http.Error LoggedInState)


saveWorkout : String -> Date -> Workout -> Cmd Msg
saveWorkout token date workout =
    let
        encoded =
            Editor.encodeWorkout date workout
    in
    Maybe.map (createWorkout token >> Http.toTask) encoded
        |> Maybe.withDefault (Task.succeed 0)
        |> Task.attempt
            (Result.map
                (always (LoggedInMsg WorkoutSaved))
                >> Result.withDefault (LoggedInMsg SaveWorkoutFailed)
            )


rememberWorkout : Workout -> Cmd Msg
rememberWorkout workout =
    remember
        ( "workout", Editor.toString workout )


forgetWorkout : Cmd Msg
forgetWorkout =
    forget "workout"


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
                [ rememberWorkout workout
                , Cmd.map (WorkoutMsg >> LoggedInMsg) cmd
                ]
            )

        -- MODALS
        OpenModal content ->
            ( LoggedIn { model | modal = Just { state = Modal.IsOpening, content = content } }
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
            ( LoggedIn { model | modal = Maybe.map (updateContent content) model.modal }, Cmd.none )

        CloseModal ->
            ( LoggedIn { model | modal = Maybe.map (Modal.updateState Modal.IsClosing) model.modal }, Cmd.none )

        ToggleSidebar ->
            ( LoggedIn { model | sidebarIsOpen = not model.sidebarIsOpen }, Cmd.none )

        SaveWorkout ->
            ( LoggedIn model, saveWorkout model.token model.date model.workout )

        WorkoutSaved ->
            ( LoggedIn { model | workout = Editor.empty, modal = Nothing }, forgetWorkout )

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


loggedOutUpdate : LoggedOutAction -> LoggedOutState -> ( Model, Cmd Msg )
loggedOutUpdate msg model =
    case msg of
        UpdateEmail str ->
            ( LoggedOut { model | email = str }, Cmd.none )

        UpdatePassword str ->
            ( LoggedOut { model | password = str }, Cmd.none )

        SendLoginRequest ->
            let
                makeState token =
                    Task.map2 (makeLoggedInState token Editor.empty) (Http.toTask (getExercises token)) Date.now

                loginTask =
                    login model.email model.password
                        |> Http.toTask
                        |> Task.andThen makeState
            in
            ( LoggedOut { model | logging_in = True }, Task.attempt (LoginCompleted >> LoggedOutMsg) loginTask )

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
                (loginForm state)

        LoggedIn state ->
            Html.map LoggedInMsg
                (layer 0
                    [ layer 0 [ content state ]
                    , layer 1 [ navbar state ]
                    , layer 2 [ sidebar state ]
                    , layer 3 (state.modal |> Maybe.map (modals state) |> Maybe.withDefault [])
                    ]
                )


layer : Int -> List (Html msg) -> Html msg
layer z =
    Html.div
        [ class "relative"
        , style [ ( "z-index", Basics.toString z ) ]
        ]


modals : LoggedInState -> Modal.Modal { content : ModalContent } -> List (Html LoggedInAction)
modals model modal =
    case modal.content of
        WorkoutDone ->
            [ Modal.view modal.state
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
                    case form.type_ of
                        JustReps ->
                            True

                        Weighted ->
                            False

                setName str =
                    UpdateModal (CreateExercise { form | name = str })
            in
            [ Modal.view modal.state
                UpdateModalState
                [ Modal.title "Create Exercise"
                , Modal.body
                    [ Form.label "Name"
                    , Form.input "text" form.name setName
                    , Form.label "Type"
                    , Form.radio isReps "Reps" (UpdateModal (CreateExercise { form | type_ = JustReps }))
                    , Form.radio (not isReps) "Reps × Weight" (UpdateModal (CreateExercise { form | type_ = Weighted }))
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
            [ Form.label "Email"
            , Form.email email UpdateEmail
            , Form.label "Password"
            , Form.password password UpdatePassword
            ]
        , Modal.footer
            [ Form.button "Sign in" SendLoginRequest
            ]
        ]


sidebar : LoggedInState -> Html LoggedInAction
sidebar { exercises, workout, sidebarIsOpen } =
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
                , ( "right", "-210px" )
                , ( "width", "200px" )
                , ( "transform"
                  , if sidebarIsOpen then
                        "translateX(-210px)"
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
            , Html.map WorkoutMsg (Editor.exerciseList exercises)
            , Form.blockButton "Create exercise" (OpenModal (CreateExercise { name = "", type_ = JustReps }))
            ]
        ]


saveButton : Bool -> Html LoggedInAction
saveButton active =
    if active then
        Form.button "done" (OpenModal WorkoutDone)
    else
        Form.disabledButton "done"
