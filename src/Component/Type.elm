module Component.Type exposing (Type(..), boolValue, customValue, intValue, stringValue)


type Type t
    = StringValue String
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
