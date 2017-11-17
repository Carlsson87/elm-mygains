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


removeFromArray : Int -> Array a -> Array a
removeFromArray i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2


copyInArray : Int -> Array a -> Array a
copyInArray index array =
    Array.get index array
        |> Maybe.map (insertInArray index array)
        |> Maybe.withDefault array


insertInArray : Int -> Array a -> a -> Array a
insertInArray i a x =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice i (Array.length a) a
    in
    Array.append (Array.push x a1) a2


updateInArray : Int -> (a -> a) -> Array a -> Array a
updateInArray index f arr =
    case Array.get index arr of
        Just item ->
            Array.set index (f item) arr

        Nothing ->
            arr


traverseArray : (a -> Maybe b) -> Array.Array a -> Maybe (Array.Array b)
traverseArray f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map (Array.push x) acc
    in
    Array.foldl step (Just Array.empty)


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



-- onKeyDown : (Int -> a) -> Html.Attribute a
-- onKeyDown tagger =
--     Events.on "keydown" (Decode.map tagger Events.keyCode)


groupByTransitive : (a -> a -> Bool) -> List a -> List (List a)
groupByTransitive cmp xss =
    case xss of
        [] ->
            []

        [ x ] ->
            [ [ x ] ]

        x :: ((xx :: _) as xs) ->
            case groupByTransitive cmp xs of
                (y :: ys) as r ->
                    if cmp x xx then
                        (x :: y) :: ys
                    else
                        [ x ] :: r

                [] ->
                    []


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
