module Icon exposing (calendar, check, check_circle, circle, clone, plus, times)

import Color exposing (Color, toRgb)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (d, fill, height, viewBox, width)


calendar : Color -> Int -> Html msg
calendar =
    icon "M192 1664h288v-288h-288v288zm352 0h320v-288h-320v288zm-352-352h288v-320h-288v320zm352 0h320v-320h-320v320zm-352-384h288v-288h-288v288zm736 736h320v-288h-320v288zm-384-736h320v-288h-320v288zm768 736h288v-288h-288v288zm-384-352h320v-320h-320v320zm-352-864v-288q0-13-9.5-22.5t-22.5-9.5h-64q-13 0-22.5 9.5t-9.5 22.5v288q0 13 9.5 22.5t22.5 9.5h64q13 0 22.5-9.5t9.5-22.5zm736 864h288v-320h-288v320zm-384-384h320v-288h-320v288zm384 0h288v-288h-288v288zm32-480v-288q0-13-9.5-22.5t-22.5-9.5h-64q-13 0-22.5 9.5t-9.5 22.5v288q0 13 9.5 22.5t22.5 9.5h64q13 0 22.5-9.5t9.5-22.5zm384-64v1280q0 52-38 90t-90 38h-1408q-52 0-90-38t-38-90v-1280q0-52 38-90t90-38h128v-96q0-66 47-113t113-47h64q66 0 113 47t47 113v96h384v-96q0-66 47-113t113-47h64q66 0 113 47t47 113v96h128q52 0 90 38t38 90z"


plus : Color -> Int -> Html msg
plus =
    icon "M1600 736v192q0 40-28 68t-68 28h-416v416q0 40-28 68t-68 28h-192q-40 0-68-28t-28-68v-416h-416q-40 0-68-28t-28-68v-192q0-40 28-68t68-28h416v-416q0-40 28-68t68-28h192q40 0 68 28t28 68v416h416q40 0 68 28t28 68z"


clone : Color -> Int -> Html msg
clone =
    icon "M1664 1632v-1088q0-13-9.5-22.5t-22.5-9.5h-1088q-13 0-22.5 9.5t-9.5 22.5v1088q0 13 9.5 22.5t22.5 9.5h1088q13 0 22.5-9.5t9.5-22.5zm128-1088v1088q0 66-47 113t-113 47h-1088q-66 0-113-47t-47-113v-1088q0-66 47-113t113-47h1088q66 0 113 47t47 113zm-384-384v160h-128v-160q0-13-9.5-22.5t-22.5-9.5h-1088q-13 0-22.5 9.5t-9.5 22.5v1088q0 13 9.5 22.5t22.5 9.5h160v128h-160q-66 0-113-47t-47-113v-1088q0-66 47-113t113-47h1088q66 0 113 47t47 113z"


times : Color -> Int -> Html msg
times =
    icon "M1490 1322q0 40-28 68l-136 136q-28 28-68 28t-68-28l-294-294-294 294q-28 28-68 28t-68-28l-136-136q-28-28-28-68t28-68l294-294-294-294q-28-28-28-68t28-68l136-136q28-28 68-28t68 28l294 294 294-294q28-28 68-28t68 28l136 136q28 28 28 68t-28 68l-294 294 294 294q28 28 28 68z"


check_circle : Color -> Int -> Html msg
check_circle =
    icon "M1412 734q0-28-18-46l-91-90q-19-19-45-19t-45 19l-408 407-226-226q-19-19-45-19t-45 19l-91 90q-18 18-18 46 0 27 18 45l362 362q19 19 45 19 27 0 46-19l543-543q18-18 18-45zm252 162q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"


circle : Color -> Int -> Html msg
circle =
    icon "M1664 896q0 209-103 385.5t-279.5 279.5-385.5 103-385.5-103-279.5-279.5-103-385.5 103-385.5 279.5-279.5 385.5-103 385.5 103 279.5 279.5 103 385.5z"


check : Color -> Int -> Html msg
check =
    icon "M1671 566q0 40-28 68l-724 724-136 136q-28 28-68 28t-68-28l-136-136-362-362q-28-28-28-68t28-68l136-136q28-28 68-28t68 28l294 295 656-657q28-28 68-28t68 28l136 136q28 28 28 68z"


type alias Size =
    Int


type alias Path =
    String


type alias ViewBox =
    ( Int, Int, Int, Int )


icon : Path -> Color -> Size -> Html msg
icon path color size =
    fromPath color size size ( 0, 0, 1792, 1792 ) path


fromPath : Color -> Int -> Int -> ViewBox -> Path -> Html msg
fromPath color w h ( x0, y0, x1, y1 ) pathStr =
    let
        widthStr =
            toString w

        heightStr =
            toString h

        colorStr =
            fromColor color

        viewBoxStr =
            toString x0
                ++ " "
                ++ toString y0
                ++ " "
                ++ toString x1
                ++ " "
                ++ toString y1
    in
    svg
        [ width widthStr
        , height heightStr
        , viewBox viewBoxStr
        ]
        [ path [ fill colorStr, d pathStr ] [] ]


fromColor : Color -> String
fromColor color =
    let
        c =
            toRgb color

        r =
            toString c.red

        g =
            toString c.green

        b =
            toString c.blue

        a =
            toString c.alpha
    in
    "rgba("
        ++ r
        ++ ","
        ++ g
        ++ ","
        ++ b
        ++ ","
        ++ a
        ++ ")"
