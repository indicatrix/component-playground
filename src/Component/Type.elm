module Component.Type exposing (Type(..), boolValue, customValue, floatValue, intValue, stringValue)


type Type t
    = StringValue String
    | FloatValue Float
    | IntValue Int
    | BoolValue Bool
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


boolValue : Type t -> Maybe Bool
boolValue t =
    case t of
        BoolValue b ->
            Just b

        _ ->
            Nothing


customValue : Type t -> Maybe t
customValue t =
    case t of
        CustomValue c ->
            Just c

        _ ->
            Nothing
