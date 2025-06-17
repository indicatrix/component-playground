module Component.Type exposing (Type(..), customValue, floatValue, intValue, stringValue)


type Type t
    = StringValue String
    | FloatValue Float
    | IntValue Int
    | CustomValue t


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
