module Utils exposing (..)

import Array exposing (Array)
import Date exposing (Date, day, month, year)
import Html exposing (Attribute)
import Html.Attributes
import Html.Events as Events
import Json.Decode as Decode
import Process
import Task
import Time exposing (Time)


-- delay : Time -> msg -> Cmd msg
-- delay ms tag =
--     Process.sleep ms
--         |> Task.perform (always tag)


apply : a -> (a -> b) -> b
apply x f =
    f x


updateAtIndex : Int -> (a -> a) -> List a -> List a
updateAtIndex index f xs =
    let
        update i x =
            if i == index then
                f x
            else
                x
    in
    List.indexedMap update xs


copyAtIndex : Int -> List a -> List a
copyAtIndex index xs =
    List.take (index + 1) xs ++ List.drop index xs


removeAtIndex : Int -> List a -> List a
removeAtIndex index xs =
    List.take index xs ++ List.drop (index + 1) xs


traverseList : (a -> Maybe b) -> List a -> Maybe (List b)
traverseList f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
    List.foldl step (Just [])


dateToString : Date -> String
dateToString date =
    let
        m =
            case month date of
                Date.Jan ->
                    "01"

                Date.Feb ->
                    "02"

                Date.Mar ->
                    "03"

                Date.Apr ->
                    "04"

                Date.May ->
                    "05"

                Date.Jun ->
                    "06"

                Date.Jul ->
                    "07"

                Date.Aug ->
                    "08"

                Date.Sep ->
                    "09"

                Date.Oct ->
                    "10"

                Date.Nov ->
                    "11"

                Date.Dec ->
                    "12"
    in
    String.join "-"
        [ toString (year date)
        , m
        , String.right 2 ("0" ++ toString (day date))
        ]


isJust : Maybe a -> Bool
isJust =
    Maybe.map (always True) >> Maybe.withDefault False


stringToInt : String -> Maybe Int
stringToInt =
    String.toInt >> Result.toMaybe


stringToFloat : String -> Maybe Float
stringToFloat =
    String.toFloat >> Result.toMaybe


onTransitionEnd : msg -> Attribute msg
onTransitionEnd tag =
    let
        options =
            { preventDefault = False
            , stopPropagation = True
            }
    in
    Events.onWithOptions "transitionend" options (Decode.succeed tag)


find : (a -> Bool) -> List a -> Maybe a
find p xs =
    List.head (List.filter p xs)
