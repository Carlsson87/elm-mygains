module Requests exposing (..)

import Data
    exposing
        ( Exercise
        , ValidWorkout
        , exerciseDecoder
        , workoutEncoder
        )
import Http
import Json.Decode as Decode
import Json.Encode as Encode


endpoint =
    "http://api.mygains.se"


login : String -> String -> Http.Request String
login username password =
    let
        encodedValue =
            Encode.object
                [ ( "email", Encode.string username )
                , ( "password", Encode.string password )
                ]
    in
    post "/login" (Decode.field "token" Decode.string) encodedValue ""


getExercises : String -> Http.Request (List Exercise)
getExercises =
    get "/exercises" exerciseDecoder


refreshToken : String -> Http.Request String
refreshToken =
    get "/refresh" (Decode.field "token" Decode.string)


createWorkout : String -> ValidWorkout -> Http.Request Int
createWorkout token workout =
    post "/workouts" (Decode.field "id" Decode.int) (workoutEncoder workout) token


get : String -> Decode.Decoder a -> String -> Http.Request a
get url decoder token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = endpoint ++ url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


post : String -> Decode.Decoder a -> Encode.Value -> String -> Http.Request a
post url decoder body token =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = endpoint ++ url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }