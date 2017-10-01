port module Main exposing (..)

import Array exposing (Array)
import Data exposing (..)
import Date
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation exposing (Location, newUrl, programWithFlags)
import Requests
    exposing
        ( createWorkout
        , getExercises
        , login
        , refreshToken
        )
import Route exposing (..)
import Task
import Utils exposing (..)


port remember : ( String, String ) -> Cmd msg


port forget : String -> Cmd msg


saveToken token =
    remember ( "jwt", token )


removeToken =
    forget "jwt"


main =
    programWithFlags
        (locationToRoute
            >> SetRoute
        )
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { sidebarIsOpen : Bool
    , exercises : List Exercise
    , workout : EditableWorkout
    , page : Route
    , api_token : Maybe String
    , login_form : LoginForm
    }


type alias LoginForm =
    { username : String
    , password : String
    , in_progress : Bool
    , error_message : Maybe String
    }


init : Decode.Value -> Location -> ( Model, Cmd Msg )
init flags loc =
    let
        handler =
            Result.map (TokenRefreshSucceeded (locationToRoute loc))
                >> Result.withDefault TokenRefreshFailed
    in
    flags
        |> Decode.decodeValue (Decode.field "api_token" Decode.string)
        |> Result.map (refreshToken >> Http.send handler)
        |> Result.map (\cmd -> ( model Loading Nothing, cmd ))
        |> Result.withDefault ( model Login Nothing, newUrl (routeToUrl Login) )


emptyLoginForm =
    LoginForm "" "" False Nothing


type Msg
    = Noop
    | TokenRefreshFailed
    | TokenRefreshSucceeded Route String
    | UpdateUsername String
    | UpdatePassword String
    | SendLoginRequest
    | UserLoggedIn String
    | LoginFailed String
    | ToggleSidebar
    | AddSet Exercise
    | UpdateReps Int String
    | UpdateWeight Int String
    | CloneSet Int
    | RemoveSet Int
    | SaveWorkout
    | SetRoute Route
    | SetExercises (List Exercise)
    | NavigateTo Route


model route token =
    Model
        True
        []
        (EditableWorkout Array.empty)
        route
        token
        emptyLoginForm


loadData token =
    let
        exerciseHandler exs =
            exs
                |> Result.map SetExercises
                |> Result.withDefault Noop
    in
    Cmd.batch
        [ Http.send exerciseHandler (getExercises token)
        ]


saveWorkout : Model -> Cmd Msg
saveWorkout { workout, api_token } =
    case api_token of
        Just token ->
            Task.attempt (always Noop)
                (Date.now
                    |> Task.map (toValidWorkout workout)
                    |> Task.map (Maybe.map (createWorkout token))
                    |> Task.map (Maybe.map Http.toTask)
                    |> Task.andThen (Maybe.withDefault (Task.succeed 0))
                )

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NavigateTo route ->
            ( model, newUrl (routeToUrl route) )

        SetExercises exs ->
            ( { model | exercises = exs }, Cmd.none )

        UserLoggedIn token ->
            ( { model | api_token = Just token }
            , Cmd.batch
                [ newUrl (routeToUrl Workout)
                , loadData token
                , saveToken token
                ]
            )

        TokenRefreshFailed ->
            ( { model | api_token = Nothing }
            , Cmd.batch
                [ newUrl (routeToUrl Login)
                , removeToken
                ]
            )

        TokenRefreshSucceeded route token ->
            ( { model | api_token = Just token, page = route }
            , Cmd.batch
                [ saveToken token
                , loadData token
                ]
            )

        LoginFailed msg ->
            let
                loginForm =
                    model.login_form
            in
            ( { model | login_form = { loginForm | error_message = Just msg, in_progress = False } }, Cmd.none )

        ToggleSidebar ->
            ( { model | sidebarIsOpen = not model.sidebarIsOpen }, Cmd.none )

        SaveWorkout ->
            ( model, saveWorkout model )

        AddSet ex ->
            ( { model | workout = addSet ex model.workout }, Cmd.none )

        UpdateReps index reps ->
            ( { model | workout = updateReps reps index model.workout }, Cmd.none )

        UpdateWeight index weight ->
            ( { model | workout = updateWeight weight index model.workout }, Cmd.none )

        CloneSet index ->
            ( { model | workout = cloneSet index model.workout }, Cmd.none )

        RemoveSet index ->
            ( { model | workout = removeSet index model.workout }, Cmd.none )

        SetRoute route ->
            ( { model | page = route }, Cmd.none )

        UpdateUsername str ->
            let
                loginForm =
                    model.login_form
            in
            ( { model | login_form = { loginForm | username = str } }, Cmd.none )

        UpdatePassword str ->
            let
                loginForm =
                    model.login_form
            in
            ( { model | login_form = { loginForm | password = str } }, Cmd.none )

        SendLoginRequest ->
            let
                loginForm =
                    model.login_form

                handler res =
                    case res of
                        Ok token ->
                            UserLoggedIn token

                        Err _ ->
                            LoginFailed ""
            in
            ( { model | login_form = { loginForm | in_progress = True } }
            , Http.send handler (login model.login_form.username model.login_form.password)
            )



-- View


view : Model -> Html Msg
view model =
    case model.page of
        Loading ->
            Html.text "Loading..."

        NotFound ->
            Html.text "404"

        Login ->
            case model.api_token of
                Just _ ->
                    alreadyLoggedInView

                Nothing ->
                    loginForm model

        Workout ->
            Html.div
                [ class "padding-1"
                , style [ ( "margin-bottom", "100px" ) ]
                ]
                [ addButton
                , sidebar model.exercises model.sidebarIsOpen
                , setList model.workout.sets
                , saveButton (not (Array.isEmpty model.workout.sets) && Array.foldl (\s p -> p && isValidSet s) True model.workout.sets)
                ]


alreadyLoggedInView : Html Msg
alreadyLoggedInView =
    Html.div
        [ class "padding-1"
        ]
        [ Html.h2
            [ class "margin-b-1"
            ]
            [ Html.text "Already logged in"
            ]
        , primaryButton "Log out" Noop
        , primaryButton "Go inside" (NavigateTo Workout)
        ]


loginForm : Model -> Html Msg
loginForm model =
    let
        button =
            if model.login_form.in_progress then
                primaryButton "Logging in..." Noop
            else
                primaryButton "Log in" SendLoginRequest

        keyDownHandler key =
            case key of
                13 ->
                    SendLoginRequest

                _ ->
                    Noop
    in
    Html.div
        [ class "padding-1"
        ]
        [ Html.input
            [ type_ "text"
            , class "block full-width padding-1 margin-b-1 border-gray border-radius-1"
            , value model.login_form.username
            , onInput UpdateUsername
            ]
            []
        , Html.input
            [ type_ "password"
            , class "block full-width padding-1 margin-b-1 border-gray border-radius-1"
            , value model.login_form.password
            , onInput UpdatePassword
            , onKeyDown keyDownHandler
            ]
            []
        , button
        , Html.text (Maybe.withDefault "" model.login_form.error_message)
        ]


addButton =
    Html.div
        [ class "text-center fixed padding-1 bg-success border-round color-white shadow-2"
        , style
            [ ( "height", "50px" )
            , ( "width", "50px" )
            , ( "bottom", "40px" )
            , ( "right", "40px" )
            , ( "font-size", "16px" )
            ]
        , onClick ToggleSidebar
        ]
        [ Html.text "+" ]


sidebar exercises isOpen =
    Html.div []
        [ Html.div
            [ class "fixed"
            , onClick ToggleSidebar
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
                ]
            ]
            []
        , Html.div
            [ class "padding-1 bg-white fixed transition shadow-1"
            , style
                [ ( "top", "0" )
                , ( "bottom", "0" )
                , ( "right", "-210px" )
                , ( "width", "200px" )
                , ( "transform"
                  , if isOpen then
                        "translateX(-200px)"
                    else
                        ""
                  )
                ]
            ]
            [ exerciseList exercises ]
        ]


saveButton : Bool -> Html Msg
saveButton active =
    let
        ( handler, classes ) =
            case active of
                True ->
                    ( SaveWorkout, "border-radius-1 block full-width padding-1 bg-success color-white" )

                False ->
                    ( Noop, "border-radius-1 block full-width padding-1 bg-gray-1 color-white cursor-disabled" )
    in
    Html.button
        [ class classes
        , disabled (not active)
        , onClick handler
        ]
        [ Html.text "Save" ]


exerciseList : List Exercise -> Html Msg
exerciseList exercises =
    Html.div []
        (List.map
            addExerciseButton
            exercises
        )


primaryButton : String -> Msg -> Html Msg
primaryButton text handler =
    Html.button
        [ onClick handler
        , class "margin-b-1 full-width block bg-primary color-white padding-1 border-radius-1"
        ]
        [ Html.text text ]


addExerciseButton : Exercise -> Html Msg
addExerciseButton ({ name } as ex) =
    primaryButton name (AddSet ex)


setList : Array Set -> Html Msg
setList sets =
    Html.div []
        (Array.toList (Array.indexedMap setForm sets))


setForm : Int -> Set -> Html Msg
setForm index ({ exercise, reps, reps_input, weight, weight_input } as set) =
    let
        star =
            if isValidSet set then
                ""
            else
                "*"
    in
    Html.div [ class "margin-b-1" ]
        [ Html.h5 [] [ Html.text (exercise.name ++ star) ]
        , Html.div [ class "flex" ]
            [ Html.div [ class "flex-fill" ] [ numberInput reps_input (UpdateReps index) ]
            , Html.div [ class "flex-fill" ] [ numberInput weight_input (UpdateWeight index) ]
            , Html.div [ class "flex-fit" ] [ primaryButton "Duplicera" (CloneSet index) ]
            , Html.div [ class "flex-fit" ] [ primaryButton "Radera" (RemoveSet index) ]
            ]
        ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput val handler =
    Html.input
        [ type_ "number"
        , class "full-width border-gray border-radius-1 padding-1 border-gray"
        , value val
        , onInput handler
        ]
        []
