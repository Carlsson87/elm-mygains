module Route exposing (..)

import Navigation exposing (Location)


type Route
    = Workout
    | Login
    | Loading
    | NotFound


locationToRoute : Location -> Route
locationToRoute { hash } =
    case hash of
        "#/workout" ->
            Workout

        "#/login" ->
            Login

        _ ->
            NotFound


routeToUrl : Route -> String
routeToUrl route =
    case route of
        Workout ->
            "#/workout"

        Login ->
            "#/login"

        _ ->
            "#/404"
