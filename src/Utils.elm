module Utils exposing (..)

import Array exposing (Array)
import Date exposing (Date, day, month, year)
import Html
import Html.Events
import Json.Decode as Decode


removeFromArray : Int -> Array a -> Array a
removeFromArray i a =
    let
        a1 =
            Array.slice 0 i a

        a2 =
            Array.slice (i + 1) (Array.length a) a
    in
    Array.append a1 a2


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


onKeyDown : (Int -> a) -> Html.Attribute a
onKeyDown tagger =
    Html.Events.on "keydown" (Decode.map tagger Html.Events.keyCode)
