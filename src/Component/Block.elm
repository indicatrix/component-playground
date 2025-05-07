module Component.Block exposing
    ( Block(..)
    , Block_
    , Builder
    , Lookup
    , add
    , build
    , finish
    , identifier
    , list
    , list2
    , oneOf
    , string
    , unwrap
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


type Block t x a
    = Block (State Ref (Block_ t x a))


type alias Block_ t x a =
    { fromType : Lookup t -> Maybe a
    , toType : x -> List ( Ref, Type t )
    , controls : List (Lookup t -> Html (List ( Ref, Type t )))
    , default : a
    }


unwrap : Block t x a -> State Ref (Block_ t x a)
unwrap (Block b) =
    b


type Builder a
    = Builder a


finish : Builder (Block t a a) -> String -> Block t a a
finish (Builder (Block bState)) label =
    let
        controls b =
            [ \lookup ->
                UI.vStack [ UI.style "gap" "8px" ]
                    [ UI.text [] [ Html.text label ]
                    , UI.vStack
                        [ UI.style "gap" "8px"
                        , UI.style "padding-left" "16px"
                        ]
                        (List.map (\c -> c lookup) b.controls)
                    ]
            ]
    in
    State.map (\b -> { b | controls = controls b }) bState |> Block


build : a -> Builder (Block t x a)
build a =
    Builder <|
        Block <|
            State.state
                { fromType = \_ -> Just a
                , toType = \_ -> []
                , controls = []
                , default = a
                }


add :
    String
    -> (String -> Block t a a)
    -> (x -> a)
    -> Builder (Block t x (a -> b))
    -> Builder (Block t x b)
add label block fa (Builder (Block stateF)) =
    let
        inner : Block_ t x (a -> b) -> Block_ t a a -> Block_ t x b
        inner bF b1 =
            let
                fromType : Lookup t -> Maybe b
                fromType lookup =
                    bF.fromType lookup |> Maybe.andMap (b1.fromType lookup)

                toType : x -> List ( Ref, Type t )
                toType x =
                    b1.toType (fa x) ++ bF.toType x

                controls : List (Lookup t -> Html (List ( Ref, Type t )))
                controls =
                    bF.controls ++ b1.controls

                default : b
                default =
                    bF.default b1.default
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = default
            }
    in
    stateF
        |> State.andThen (\bF -> block label |> unwrap |> State.map (inner bF))
        |> Block
        |> Builder


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
            in
            { fromType = fromType
            , toType = toType
            , controls = [ controls ]
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
                , default = Ref.toString ref
                }
            )
        |> Block


list : (String -> Block t b a) -> String -> Block t (List b) (List a)
list labelledBlock listLabel =
    listHelper (\label -> unwrap (labelledBlock label)) listLabel


list2 : (g -> String -> Block t b a) -> g -> String -> Block t (List b) (List a)
list2 labelledBlock dep listLabel =
    listHelper (\label -> unwrap (labelledBlock dep label)) listLabel


listHelper : (String -> State Ref (Block_ t b a)) -> String -> Block t (List b) (List a)
listHelper block listLabel =
    let
        inner : Ref -> Block_ t (List b) (List a)
        inner ref =
            let
                fromType : Lookup t -> Maybe (List a)
                fromType lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)
                    in
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
                        |> Just

                toType : List b -> List ( Ref, Type t )
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
            in
            { fromType = fromType
            , toType = toType
            , controls = [ control ]
            , default = default
            }
    in
    State.map inner Ref.take |> Block


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a a
oneOf first rest label =
    let
        inner : Ref -> Block_ t a a
        inner ref =
            let
                valuesList =
                    first :: rest

                values =
                    Array.fromList <| List.map Tuple.first valuesList

                toInt : a -> Maybe Int
                toInt a =
                    List.findIndex (\( x, _ ) -> x == a) valuesList

                fromInt : Int -> Maybe a
                fromInt i =
                    Array.get i values

                toType s =
                    Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                        (toInt s)
                        |> Maybe.withDefault []

                fromType lookup =
                    lookup ref |> Maybe.andThen Type.intValue |> Maybe.andThen fromInt

                controls lookup =
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
                                |> Maybe.withDefault "0"
                        , options =
                            List.indexedMap
                                (\i ( _, s ) -> { label = s, value = String.fromInt i })
                                valuesList
                        }
            in
            { fromType = fromType
            , toType = toType
            , controls = [ controls ]
            , default = Tuple.first first
            }
    in
    Block <| State.map inner Ref.take
