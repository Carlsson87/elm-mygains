module Data.Kind exposing (..)


type Kind a b
    = JustReps a
    | Weighted b


map : (a1 -> a2) -> (b1 -> b2) -> Kind a1 b1 -> Kind a2 b2
map fa fb t =
    case t of
        JustReps a ->
            JustReps (fa a)

        Weighted b ->
            Weighted (fb b)


mapJustReps : (a1 -> a2) -> Kind a1 b -> Kind a2 b
mapJustReps f =
    map f identity


mapWeighted : (b1 -> b2) -> Kind a b1 -> Kind a b2
mapWeighted f =
    map identity f


fold : (a -> x) -> (b -> x) -> Kind a b -> x
fold fa fb kind =
    case kind of
        JustReps a ->
            fa a

        Weighted b ->
            fb b


cata : { justReps : a -> x, weighted : b -> x } -> Kind a b -> x
cata { justReps, weighted } =
    fold justReps weighted


replace : x -> y -> Kind a b -> Kind x y
replace x y =
    map (always x) (always y)
