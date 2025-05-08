module Component.Block exposing
    ( Block
    , BlockDef
    , BlockI(..)
    , Builder
    , Lookup
    , addVia
    , build
    , finish
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
    = Block (BlockI_ t i a)


type alias BlockI_ t i a =
    { finish : i -> a, block_ : State Ref (BlockDef t i i) }


block : (i -> a) -> State Ref (BlockDef t i i) -> BlockI t i a
block f block_ =
    Block { finish = f, block_ = block_ }


type alias BlockDef t r i =
    { fromType : Lookup t -> Maybe i
    , toType : r -> List ( Ref, Type t )
    , controls : r -> List (Lookup t -> Html (List ( Ref, Type t )))
    , default : i
    }


unwrap : BlockI t i a -> BlockI_ t i a
unwrap (Block b) =
    b


withDefault : a -> Block t a -> Block t a
withDefault a (Block b) =
    Block <| { b | block_ = State.map (\b_ -> { b_ | default = a }) b.block_ }


type Builder t r a
    = Builder (State Ref (BlockDef t r a))


build : a -> Builder t r a
build a =
    Builder <|
        State.state
            { fromType = \_ -> Just a
            , toType = \_ -> []
            , controls = \_ -> []
            , default = a
            }


addVia :
    (r -> a)
    -> String
    -> (String -> Block t a)
    -> Builder t r (a -> b)
    -> Builder t r b
addVia fa label blockF (Builder stateF) =
    let
        inner : BlockDef t r (a -> b) -> BlockDef t a a -> BlockDef t r b
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
            }
    in
    stateF
        |> State.andThen
            (\bF -> blockF label |> unwrap |> .block_ |> State.map (inner bF))
        |> Builder


finish : (i -> a) -> Builder t i i -> String -> BlockI t i a
finish f (Builder bState) label =
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
    State.map (\b -> { b | controls = controls b }) bState |> block f


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
            }
    in
    block identity <| State.map inner Ref.take


identifier : BlockI t Ref String
identifier =
    Ref.take
        |> State.map
            (\ref ->
                { fromType = \_ -> Nothing
                , toType = \_ -> []
                , controls = \_ -> []
                , default = ref
                }
            )
        |> block Ref.toString


list : (String -> BlockI t i a) -> String -> BlockI t (List i) (List a)
list labelledBlock listLabel =
    listHelper (\label -> unwrap (labelledBlock label)) listLabel


list2 : (g -> String -> BlockI t i a) -> g -> String -> BlockI t (List i) (List a)
list2 labelledBlock dep listLabel =
    listHelper (\label -> unwrap (labelledBlock dep label)) listLabel


listHelper : (String -> BlockI_ t i a) -> String -> BlockI t (List i) (List a)
listHelper blockF listLabel =
    let
        inner : Ref -> BlockDef t (List i) (List i)
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
                                            (blockF (String.fromInt i)).block_
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
                                            (blockF (String.fromInt i)).block_
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

                        something ( i, default_ ) =
                            State.map
                                (\b ->
                                    List.map
                                        (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup)
                                        (b.controls default_)
                                )
                                (blockF (String.fromInt i)).block_
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
                                        something
                                        (List.indexedMap Tuple.pair default)
                                        |> Ref.from ref
                                    )
                            )
                        ]
            in
            { fromType = fromType
            , toType = toType
            , controls = \default -> [ control default ]
            , default =
                State.traverse
                    (\i -> State.map .default (blockF (String.fromInt i)).block_)
                    (List.range 0 2)
                    |> Ref.from ref
            }
    in
    State.map inner Ref.take |> block


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a
oneOf first rest label =
    let
        inner : Ref -> BlockDef t a a
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
            }
    in
    Block <| State.map inner Ref.take
