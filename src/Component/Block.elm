module Component.Block exposing (..)

import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Html exposing (Html)
import Maybe.Extra as Maybe
import State exposing (State)


type alias Lookup t =
    Ref -> Maybe (Type t)


type Block t x a
    = Block (State Ref (Block_ t x a))


type alias Block_ t x a =
    { fromType : Lookup t -> Maybe a
    , toType : x -> List ( Ref, Type t )
    , controls : List (Lookup t -> Html (List ( Ref, Type t )))
    , displays : List (Lookup t -> Html ())
    , default : a
    }


unwrap : Block t x a -> State Ref (Block_ t x a)
unwrap (Block b) =
    b


pure : a -> Block t x a
pure a =
    Block <|
        State.state
            { fromType = \_ -> Just a
            , toType = \_ -> []
            , controls = []
            , displays = []
            , default = a
            }


andMap : Block t x1 a -> Block t x (a -> b) -> Block t x b
andMap (Block state1) (Block stateF) =
    let
        inner : Block_ t x (a -> b) -> Block_ t x1 a -> Block_ t x b
        inner bF b1 =
            let
                fromType : Lookup t -> Maybe b
                fromType lookup =
                    bF.fromType lookup |> Maybe.andMap (b1.fromType lookup)

                toType : x -> List ( Ref, Type t )
                toType x =
                    bF.toType x

                controls : List (Lookup t -> Html (List ( Ref, Type t )))
                controls =
                    Debug.todo ""

                displays : List (Lookup t -> Html ())
                displays =
                    Debug.todo ""

                default : b
                default =
                    bF.default b1.default
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , displays = displays
            , default = default
            }
    in
    stateF
        |> State.andThen (\bF -> state1 |> State.map (inner bF))
        |> Block


string : String -> Block t String String
string label =
    let
        inner ref =
            let
                toType s =
                    [ ( ref, Type.StringValue s ) ]

                fromType lookup =
                    lookup ref |> Maybe.andThen Type.stringValue

                default =
                    "Value"

                controls lookup =
                    UI.textField
                        { msg = toType
                        , id = Ref.toString ref
                        , label = label
                        , value = fromType lookup |> Maybe.withDefault default
                        }

                displays lookup =
                    UI.text []
                        [ Html.text <|
                            label
                                ++ ": "
                                ++ (fromType lookup |> Maybe.withDefault default)
                        ]
            in
            { fromType = fromType
            , toType = toType
            , controls = [ controls ]
            , displays = [ displays ]
            , default = default
            }
    in
    Block <| State.map inner Ref.take


identifier : Block t x String
identifier =
    Ref.take
        |> State.map
            (\ref ->
                { fromType = \_ -> Nothing
                , toType = \_ -> []
                , controls = []
                , displays = []
                , default = Ref.toString ref
                }
            )
        |> Block


list : (String -> Block t a a) -> String -> Block t (List a) (List a)
list labelledBlock listLabel =
    listHelper (\label -> unwrap (labelledBlock label)) listLabel


list2 : (b -> String -> Block t a a) -> b -> String -> Block t (List a) (List a)
list2 labelledBlock dep listLabel =
    listHelper (\label -> unwrap (labelledBlock dep label)) listLabel


listHelper : (String -> State Ref (Block_ t a a)) -> String -> Block t (List a) (List a)
listHelper block listLabel =
    let
        inner : Ref -> Block_ t (List a) (List a)
        inner ref =
            let
                fromType : Lookup t -> Maybe (List a)
                fromType lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.map
                            (\len ->
                                State.traverse
                                    (\i ->
                                        State.map
                                            (\b ->
                                                b.fromType lookup
                                                    |> Maybe.withDefault b.default
                                            )
                                            (block (String.fromInt i))
                                    )
                                    (List.range 0 (len - 1))
                                    |> Ref.from ref
                            )

                toType : List a -> List ( Ref, Type t )
                toType values =
                    ( ref, Type.IntValue <| List.length values )
                        :: List.concat
                            (Ref.from ref
                                (State.traverse
                                    (\( i, value ) ->
                                        State.map (\b -> b.toType value)
                                            (block (String.fromInt i))
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                default =
                    State.traverse
                        (\i -> State.map .default (block (String.fromInt i)))
                        (List.range 0 2)
                        |> Ref.from ref

                control : Lookup t -> Html (List ( Ref, Type t ))
                control lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)
                    in
                    UI.vStack [ UI.style "gap" "8px" ]
                        [ UI.text [] [ Html.text listLabel ]
                        , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                            (UI.hStack [ UI.style "gap" "8px" ]
                                [ UI.button [ UI.onClick [ ( ref, Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , UI.button [ UI.onClick [ ( ref, Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]
                                :: List.concat
                                    (State.traverse
                                        (\i ->
                                            State.map
                                                (\b ->
                                                    List.map (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup) b.controls
                                                )
                                                (block (String.fromInt i))
                                        )
                                        (List.range 0 (len - 1))
                                        |> Ref.from ref
                                    )
                            )
                        ]

                display : Lookup t -> Html ()
                display lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)
                    in
                    UI.vStack []
                        (UI.hStack [] [ UI.text [] [ Html.text listLabel ], UI.text [] [ Html.text "List:" ] ]
                            :: List.concat
                                (State.traverse
                                    (\i ->
                                        State.map
                                            (\b -> List.map (\f -> f lookup) b.displays)
                                            (block (String.fromInt i))
                                    )
                                    (List.range 0 (len - 1))
                                    |> Ref.from ref
                                )
                        )
            in
            { fromType = fromType
            , toType = toType
            , controls = [ control ]
            , displays = [ display ]
            , default = default
            }
    in
    Block <| State.map inner Ref.take
