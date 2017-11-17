module View.Form
    exposing
        ( blockButton
        , button
        , date
        , disabledButton
        , email
        , iconButton
        , input
        , label
        , numberInput
        , password
        , radio
        , text
        )

import Colors
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Event
import Icon
import Utils exposing (dateToString)


input : String -> String -> (String -> msg) -> Html msg
input typ value tag =
    Html.input
        [ Attr.type_ typ
        , Attr.class "shadow-sm-inset full-width lh-xl fz-sm ls-sm rounded border-silver pl-md pr-md mb-md focus-border-blue"
        , Attr.value value
        , Event.onInput tag
        ]
        []


date : Date -> (Date -> msg) -> Html msg
date d tag =
    Html.div
        [ Attr.class "relative"
        ]
        [ Html.span
            [ Attr.class "absolute"
            , Attr.style
                [ ( "top", "1rem" )
                , ( "left", "1rem" )
                ]
            ]
            [ Icon.calendar Colors.dark 16
            ]
        , Html.input
            [ Attr.type_ "date"
            , Attr.class "shadow-sm-inset full-width lh-xl fz-sm ls-sm rounded border-silver pl-xl pr-md mb-md focus-border-blue"
            , Attr.value (dateToString d)
            , Event.onInput (Date.fromString >> Result.withDefault d >> tag)
            ]
            []
        ]


label : String -> Html msg
label str =
    Html.label
        [ Attr.class "fz-sm ls-sm mb-sm inline-block bold"
        ]
        [ Html.text str
        ]


email : String -> (String -> msg) -> Html msg
email =
    input "email"


password : String -> (String -> msg) -> Html msg
password =
    input "password"


text : String -> (String -> msg) -> Html msg
text =
    input "text"


blockButton : String -> msg -> Html msg
blockButton text tag =
    button_ (btnClass ++ " color-blue full-width") (Html.text text) tag


button : String -> msg -> Html msg
button text tag =
    button_ (btnClass ++ " color-blue") (Html.text text) tag


disabledButton : String -> Html msg
disabledButton text =
    Html.button
        [ Attr.class (btnClass ++ " color-silver")
        , Attr.disabled True
        ]
        [ Html.text text
        ]


iconButton : Html msg -> msg -> Html msg
iconButton icon tag =
    Html.button
        [ Attr.class "bg-white focus-bg-clouds no-outline"
        , Attr.style
            [ ( "height", "48px" )
            , ( "width", "48px" )
            ]
        , Event.onClick tag
        ]
        [ icon
        ]


button_ : String -> Html msg -> msg -> Html msg
button_ classes content tag =
    Html.button
        [ Attr.class classes
        , Event.onClick tag
        ]
        [ content
        ]


btnClass : String
btnClass =
    "lh-xl rounded bold uc ls-sm bg-white fz-sm pl-md pr-md no-outline active-bg-clouds transition-bg"


radio : Bool -> String -> msg -> Html msg
radio chk label tag =
    Html.label
        [ Attr.class "full-width fz-sm ls-sm lh-xl pl-xl relative"
        , Attr.checked chk
        ]
        [ Html.input
            [ Attr.type_ "radio"
            , Attr.style [ ( "display", "none" ) ]
            , Event.onClick tag
            ]
            []
        , Html.span
            [ Attr.class "absolute inline-block lh-none"
            , Attr.style
                [ ( "top", "0.75rem" )
                , ( "left", "0.75rem" )
                , ( "width", "24px" )
                , ( "height", "24px" )
                ]
            ]
            [ if chk then
                Icon.check_circle Colors.blue 24
              else
                Icon.circle Colors.clouds 24
            ]
        , Html.text label
        ]


numberInput : String -> (String -> msg) -> Html msg
numberInput val handler =
    Html.input
        [ Attr.type_ "number"
        , Attr.class "bg-white color-dark border-none rounded bold ls-sm lh-xl fz-lg inline-block text-center focus-bg-clouds"
        , Attr.style
            [ ( "width", "72px" )
            ]
        , Attr.pattern "[0-9]*"
        , Attr.min "0"
        , Attr.placeholder "#"
        , Attr.value val
        , Event.onInput handler
        ]
        []
