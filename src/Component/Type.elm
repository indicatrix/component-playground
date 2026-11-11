module Component.Type exposing (Type(..), customValue, floatValue, intValue, stringValue)

import Component.Ref exposing (Ref)


{-| A stored control value. `Copy` and `Empty` are never stored: as an update
they rewrite the target ref's whole subtree. `Copy source` replaces it with a
copy of the source's subtree; `Empty` removes it. Updates apply in order, so a
later update sees an earlier copy.
-}
type Type t
    = StringValue String
    | FloatValue Float
    | IntValue Int
    | CustomValue t
    | Copy Ref
    | Empty


stringValue : Type t -> Maybe String
stringValue t =
    case t of
        StringValue s ->
            Just s

        _ ->
            Nothing


floatValue : Type t -> Maybe Float
floatValue t =
    case t of
        FloatValue f ->
            Just f

        _ ->
            Nothing


intValue : Type t -> Maybe Int
intValue t =
    case t of
        IntValue i ->
            Just i

        _ ->
            Nothing


customValue : Type t -> Maybe t
customValue t =
    case t of
        CustomValue c ->
            Just c

        _ ->
            Nothing
