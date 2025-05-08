module Component.Block exposing
    ( Block
    , BlockI(..)
    , BlockI_
    , Builder
    , Lookup
    , addVia
    , build
    , finishI
    , identifier
    , list
    , list2
    , oneOf
    , string
    , unwrap
    , withDefault
    )

import Array
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)


type alias Lookup t =
    Ref -> Maybe (Type t)


type alias Block t a =
    BlockI t a a


type BlockI t i a
    = Block (State Ref (BlockI_ t i i a))


type alias BlockI_ t i r a =
    { fromType : Lookup t -> Maybe i
    , toType : r -> List ( Ref, Type t )
    , controls : r -> List (Lookup t -> Html (List ( Ref, Type t )))
    , default : i
    , map : Lookup t -> i -> a
    }


unwrap : BlockI t i a -> State Ref (BlockI_ t i i a)
unwrap (Block bState) =
    bState


withDefault : i -> BlockI t i a -> BlockI t i a
withDefault i (Block state) =
    Block <| State.map (\b -> { b | default = i }) state


type Builder t i r a
    = Builder (State Ref (BlockI_ t i r a))


build : i -> Builder t i r i
build i =
    Builder <|
        State.state
            { fromType = \_ -> Nothing
            , toType = \_ -> []
            , controls = \_ -> []
            , default = i
            , map = always identity
            }


addVia :
    (r -> a)
    -> String
    -> (String -> BlockI t a a)
    -> Builder t (a -> b) r (a -> b)
    -> Builder t b r b
addVia fa label blockF (Builder stateF) =
    let
        inner : BlockI_ t (a -> b) r (a -> b) -> BlockI_ t a a a -> BlockI_ t b r b
        inner bF b1 =
            let
                fromType : Lookup t -> Maybe b
                fromType lookup =
                    bF.fromType lookup |> Maybe.andMap (b1.fromType lookup)

                toType : r -> List ( Ref, Type t )
                toType r =
                    b1.toType (fa r) ++ bF.toType r

                controls : r -> List (Lookup t -> Html (List ( Ref, Type t )))
                controls default =
                    bF.controls default ++ b1.controls (fa default)
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default b1.default
            , map = always identity
            }
    in
    stateF
        |> State.andThen
            (\bF -> blockF label |> unwrap |> State.map (inner bF))
        |> Builder


finishI : (i -> a) -> Builder t i i i -> String -> BlockI t i a
finishI f (Builder bState) label =
    let
        controls b default =
            [ \lookup ->
                UI.vStack [ UI.style "gap" "8px" ]
                    [ UI.text [] [ Html.text label ]
                    , UI.vStack
                        [ UI.style "gap" "8px"
                        , UI.style "padding-left" "16px"
                        ]
                        (List.map (\c -> c lookup) (b.controls default))
                    ]
            ]
    in
    State.map
        (\b ->
            { fromType = b.fromType
            , toType = b.toType
            , controls = controls b
            , default = b.default
            , map = always f
            }
        )
        bState
        |> Block


string : String -> Block t String
string label =
    let
        inner ref =
            let
                toType s =
                    [ ( ref, Type.StringValue s ) ]

                fromType lookup =
                    lookup ref |> Maybe.andThen Type.stringValue

                controls default =
                    [ \lookup ->
                        UI.textField
                            { msg = toType
                            , id = Ref.toString ref
                            , label = label
                            , value = fromType lookup |> Maybe.withDefault default
                            }
                    ]
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = "Value"
            , map = always identity
            }
    in
    Block <| State.map inner Ref.take


identifier : BlockI t Ref String
identifier =
    Ref.take
        |> State.map
            (\ref ->
                { fromType = \_ -> Nothing
                , toType = \_ -> []
                , controls = \_ -> []
                , default = ref
                , map = always Ref.toString
                }
            )
        |> Block


list : (String -> BlockI t i a) -> String -> BlockI t (List i) (List a)
list labelledBlock listLabel =
    listHelper (\label -> unwrap (labelledBlock label)) listLabel


list2 : (g -> String -> BlockI t i a) -> g -> String -> BlockI t (List i) (List a)
list2 labelledBlock dep listLabel =
    listHelper (\label -> unwrap (labelledBlock dep label)) listLabel


listHelper : (String -> State Ref (BlockI_ t i i a)) -> String -> BlockI t (List i) (List a)
listHelper blockF listLabel =
    let
        inner : Ref -> BlockI_ t (List i) (List i) (List a)
        inner ref =
            let
                fromType : Lookup t -> Maybe (List i)
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
                                            (blockF (String.fromInt i))
                                    )
                                    (List.range 0 (len - 1))
                                    |> Ref.from ref
                            )

                toType : List i -> List ( Ref, Type t )
                toType values =
                    ( ref, Type.IntValue <| List.length values )
                        :: List.concat
                            (Ref.from ref
                                (State.traverse
                                    (\( i, value ) ->
                                        State.map (\b -> b.toType value)
                                            (blockF (String.fromInt i))
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                control : List i -> Lookup t -> Html (List ( Ref, Type t ))
                control default lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)

                        entryControl ( i, default_ ) =
                            State.map
                                (\b ->
                                    List.map
                                        (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup)
                                        (b.controls default_)
                                )
                                (blockF (String.fromInt i))
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
                                        entryControl
                                        (List.indexedMap Tuple.pair default)
                                        |> Ref.from ref
                                    )
                            )
                        ]

                map : Lookup t -> List i -> List a
                map lookup l =
                    State.traverse
                        (\( index, i ) ->
                            State.map
                                (\b -> b.map lookup i)
                                (blockF (String.fromInt index))
                        )
                        (List.indexedMap Tuple.pair l)
                        |> Ref.from ref
            in
            { fromType = fromType
            , toType = toType
            , controls = \default -> [ control default ]
            , default =
                State.traverse
                    (\i -> State.map .default (blockF (String.fromInt i)))
                    (List.range 0 2)
                    |> Ref.from ref
            , map = map
            }
    in
    State.map inner Ref.take |> Block


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a
oneOf first rest label =
    let
        inner : Ref -> BlockI_ t a a a
        inner ref =
            let
                valuesList =
                    first :: rest

                findIndex a =
                    List.findIndex (\( x, _ ) -> x == a) valuesList

                values =
                    Array.fromList <| List.map Tuple.first valuesList

                fromIndex : Int -> Maybe a
                fromIndex i =
                    Array.get i values

                toType s =
                    Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                        (findIndex s)
                        |> Maybe.withDefault []

                fromType lookup =
                    lookup ref |> Maybe.andThen Type.intValue |> Maybe.andThen fromIndex

                controls default lookup =
                    UI.select
                        { msg =
                            String.toInt
                                >> Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                                >> Maybe.withDefault []
                        , id = Ref.toString ref
                        , label = label
                        , value =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.map String.fromInt
                                |> Maybe.orElseLazy
                                    (\() ->
                                        findIndex default
                                            |> Maybe.map String.fromInt
                                    )
                                |> Maybe.withDefault "0"
                        , options =
                            List.indexedMap
                                (\i ( _, s ) -> { label = s, value = String.fromInt i })
                                valuesList
                        }
            in
            { fromType = fromType
            , toType = toType
            , controls = \default -> [ controls default ]
            , default = Tuple.first first
            , map = always identity
            }
    in
    State.map inner Ref.take |> Block
