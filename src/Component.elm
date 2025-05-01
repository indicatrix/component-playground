module Component exposing (bool, list, preview, string, withControl, withMsg, withState)

import Component.Preview as Preview exposing (Block(..), Identifier, Lookup, Preview)
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Html exposing (Html)
import Maybe.Extra as Maybe
import State exposing (State)


preview : String -> { name : String } -> a -> Preview t m a
preview =
    Preview.preview


withControl : String -> Block t a -> Preview t m (a -> b) -> Preview t m b
withControl =
    Preview.withControl


withState :
    String
    -> Block t a
    -> Preview t (Preview.Msg t m) (a -> (a -> Preview.Msg t m) -> c)
    -> Preview t (Preview.Msg t m) c
withState =
    Preview.withState


withMsg : Preview t (Preview.Msg t m) ((m -> Preview.Msg t m) -> a) -> Preview t (Preview.Msg t m) a
withMsg =
    Preview.withMsg


{-| Provide required functions to make a custom block type.
-}
string : Block t String
string =
    let
        toType s =
            [ ( "value", Type.StringValue s ) ]

        fromType lookup =
            lookup "value" |> Maybe.andThen Type.stringValue

        default =
            "Value"

        control id label lookup =
            UI.textField { msg = toType, id = id "value", label = label, value = fromType lookup |> Maybe.withDefault default }

        display label lookup =
            UI.text [] [ Html.text <| label ++ ": " ++ (fromType lookup |> Maybe.withDefault default) ]
    in
    Block
        { fromType = fromType
        , toType = toType
        , control = control
        , display = display
        , default = default
        }


type Block2 t x a
    = Block2 (State Ref (Block2_ t x a))


type alias Block2_ t x a =
    { fromType : Lookup2 t -> Maybe a
    , toType : x -> List ( Ref, Type t )
    , control : List (Lookup2 t -> Html (List ( Ref, Type t )))
    , display : List (Lookup2 t -> Html ())
    , default : a
    }


type alias Lookup2 t =
    Ref -> Maybe (Type t)


someRecords : Block2 t x { label : String, active : String }
someRecords =
    -- pure (\a b -> { label = a, active = b }) |> andMap "Label" string2 |> andMap "Active" string2
    Debug.todo ""


pure : a -> Block2 t x a
pure a =
    (\s ->
        ( { fromType = \_ -> Just a, toType = \_ -> [], control = [], display = [], default = a }
        , s
        )
    )
        |> State.advance
        |> Block2


andMap : Block2 t x1 a -> Block2 t x (a -> b) -> Block2 t x b
andMap (Block2 state1) (Block2 stateF) =
    let
        func : Block2_ t x (a -> b) -> Block2_ t x1 a -> Block2_ t x b
        func bF b1 =
            let
                fromType : Lookup2 t -> Maybe b
                fromType lookup =
                    bF.fromType lookup |> Maybe.andMap (b1.fromType lookup)

                toType : x -> List ( Ref, Type t )
                toType x =
                    bF.toType x

                control : List (Lookup2 t -> Html (List ( Ref, Type t )))
                control =
                    Debug.todo ""

                display : List (Lookup2 t -> Html ())
                display =
                    Debug.todo ""

                default : b
                default =
                    bF.default b1.default
            in
            { fromType = fromType
            , toType = toType
            , control = control
            , display = display
            , default = default
            }
    in
    stateF |> State.andThen (\bF -> state1 |> State.map (func bF)) |> Block2


string2 : String -> Block2 t String String
string2 label =
    Ref.take
        |> State.map
            (\ref ->
                let
                    toType s =
                        [ ( ref, Type.StringValue s ) ]

                    fromType lookup =
                        lookup ref |> Maybe.andThen Type.stringValue

                    default =
                        "Value"

                    control lookup =
                        UI.textField { msg = toType, id = Ref.toString ref, label = label, value = fromType lookup |> Maybe.withDefault default }

                    display lookup =
                        UI.text [] [ Html.text <| label ++ ": " ++ (fromType lookup |> Maybe.withDefault default) ]
                in
                { fromType = fromType
                , toType = toType
                , control = [ control ]
                , display = [ display ]
                , default = default
                }
            )
        |> Block2


{-| Get a bool type.
-}
bool : Block t Bool
bool =
    Debug.todo ""


{-| Create a block making a list of other blocks.
-}
list : Block t a -> Block t (List a)
list (Block b) =
    let
        fromType : Lookup t -> Maybe (List a)
        fromType lookup =
            lookup "len"
                |> Maybe.andThen Type.intValue
                |> Maybe.map
                    (\len ->
                        List.range 0 (len - 1)
                            |> List.map
                                (\i ->
                                    b.fromType (\s -> lookup (String.fromInt i ++ "." ++ s)) |> Maybe.withDefault b.default
                                )
                    )

        toType : List a -> List ( String, Type t )
        toType values =
            ( "len", Type.IntValue <| List.length values )
                :: List.concat
                    (List.indexedMap
                        (\i value ->
                            List.map
                                (Tuple.mapFirst
                                    (\s -> String.fromInt i ++ "." ++ s)
                                )
                                (b.toType value)
                        )
                        values
                    )

        default =
            [ b.default, b.default, b.default ]

        control : Identifier -> String -> Lookup t -> Html (List ( String, Type t ))
        control identifier label lookup =
            let
                len =
                    lookup "len" |> Maybe.andThen Type.intValue |> Maybe.withDefault (List.length default)
            in
            UI.vStack [ UI.style "gap" "8px" ]
                [ UI.text [] [ Html.text label ]
                , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                    (List.map
                        (\i ->
                            let
                                index =
                                    String.fromInt i

                                id s =
                                    index ++ "." ++ s
                            in
                            Html.map (\ll -> ( "len", Type.IntValue len ) :: List.map (Tuple.mapFirst id) ll) <|
                                b.control (id >> identifier) index (id >> lookup)
                        )
                        (List.range 0 (len - 1))
                        ++ [ UI.hStack [ UI.style "gap" "8px" ]
                                [ UI.button [ UI.onClick [ ( "len", Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , UI.button [ UI.onClick [ ( "len", Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]
                           ]
                    )
                ]

        display : String -> Lookup t -> Html ()
        display label lookup =
            let
                len =
                    lookup "len" |> Maybe.andThen Type.intValue |> Maybe.withDefault (List.length default)
            in
            UI.vStack []
                (UI.hStack [] [ UI.text [] [ Html.text label ], UI.text [] [ Html.text "List:" ] ]
                    :: List.map
                        (\i ->
                            let
                                index =
                                    String.fromInt i

                                id s =
                                    index ++ "." ++ s
                            in
                            b.display index (id >> lookup)
                        )
                        (List.range 0 (len - 1))
                )
    in
    Block
        { fromType = fromType
        , toType = toType
        , control = control
        , display = display
        , default = default
        }
