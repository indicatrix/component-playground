module Component exposing (bool, list, preview, string, withControl, withMsg, withState)

import Component.Preview as Preview exposing (Block(..), Identifier, Lookup, Preview)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Html exposing (Html)


preview : String -> { name : String } -> a -> Preview t m a
preview =
    Preview.preview


withControl : String -> Block t a -> Preview t m (a -> b) -> Preview t m b
withControl =
    Preview.withControl


withState : String -> Block t a -> Preview t m (a -> (a -> Preview.Msg t m) -> c) -> Preview t m c
withState =
    Preview.withState


withMsg : Preview t m ((m -> Preview.Msg t m) -> a) -> Preview t m a
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
