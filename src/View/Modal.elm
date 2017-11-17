module View.Modal exposing (Modal, State(..), body, footer, title, updateState, view)

import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events as Event
import Json.Decode as Decode


type State
    = IsOpening
    | IsOpen
    | IsClosing
    | IsClosed


type alias Modal a =
    { a | state : State }


updateState : State -> Modal a -> Modal a
updateState state modal =
    { modal | state = state }


onTransitionEnd : msg -> Html.Attribute msg
onTransitionEnd =
    Event.on "transitionend" << Decode.succeed


noop : Html.Attribute msg
noop =
    Event.on "n" (Decode.fail "noop")


title : String -> Html msg
title str =
    Html.h2
        [ class "fz-lg mb-md ls-sm color-dark"
        ]
        [ Html.text str ]


wrap : Html msg -> Html msg
wrap node =
    Html.span
        [ class "float-right"
        ]
        [ node
        ]


footer : List (Html msg) -> Html msg
footer children =
    Html.div
        [ class "cf" ]
        (List.map wrap children)


view : State -> (State -> msg) -> List (Html msg) -> Html msg
view state tag content =
    let
        ( coverOpacity, modalY, event ) =
            case state of
                IsOpening ->
                    ( "0", "translateY(calc(-100% - 32px))", onTransitionEnd (tag IsOpen) )

                IsOpen ->
                    ( "0.24", "translateY(0)", noop )

                IsClosing ->
                    ( "0", "translateY(calc(-100% - 32px))", onTransitionEnd (tag IsClosed) )

                IsClosed ->
                    ( "0", "translateY(calc(-100% - 32px))", noop )
    in
    Html.div
        []
        [ Html.div
            [ class "fixed cover bg-black transition-opacity"
            , style
                [ ( "opacity", coverOpacity )
                ]
            ]
            []
        , Html.div
            [ class "fixed bg-white shadow-sm p-md transition-transform"
            , style
                [ ( "z-index", "2" )
                , ( "top", "16px" )
                , ( "left", "16px" )
                , ( "right", "16px" )
                , ( "transform", modalY )
                ]
            , event
            ]
            content
        ]


body : List (Html msg) -> Html msg
body =
    Html.div
        [ class "mb-lg"
        ]
