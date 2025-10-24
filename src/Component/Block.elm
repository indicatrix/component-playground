module Component.Block exposing
    ( Block
    , BlockI(..)
    , BlockI_
    , Builder
    , Lookup
    , addVia
    , build
    , custom
    , finishI
    , float
    , identifier
    , int
    , list
    , list2
    , oneOf
    , string
    , stringEntryBlock
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


{-| Internal type for representing types that can be used in a Component
Playground.

Type definitions:

  - `a` is the end type. Using blocks in previews applies the `a` type.
  - `t` is the library-consumer's custom type for storing their own types.
  - `i` is the internal representation. This allows for an internal
    representation. BlockI makes this explicit, while Block assumes i == a.
    `.map` provides a mapping from i to a.
  - `r` is the ultimate type when used inside a Builder. We need this to store
    types and get defaults at each step while building the type.

-}
type alias BlockI_ t i r a =
    --| Create a type from the lookup, using a default. The ultimate type, `r`,
    -- is also provided for use in Builders.
    { fromType : r -> i -> Lookup t -> i

    --| Convert a type for later use in Lookup t.
    , toType : r -> List ( Ref, Type t )

    --| A list of controls to use. Again uses the ultimate type, `r` for use in
    -- builders. Each control can get and set Lookup t.
    , controls : r -> List (Lookup t -> Html (List ( Ref, Type t )))

    --| The default value for some type. Note this is passed into fromType so
    -- it can be overridden.
    , default : i

    --| Map the internal representation to the end type.
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
            { fromType = \_ default _ -> default
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
                fromType : r -> b -> Lookup t -> b
                fromType end _ lookup =
                    -- need a way to see if we used the default or not
                    -- default.
                    bF.fromType end bF.default lookup (b1.fromType (fa end) (fa end) lookup)

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

                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.withDefault default

                controls default =
                    [ \lookup ->
                        UI.textField
                            { msg = toType
                            , id = Ref.toString ref
                            , label = label
                            , value = fromType default default lookup
                            , error = Nothing
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


float : String -> Block t Float
float =
    stringEntryBlock
        { toString = String.fromFloat
        , fromString = String.toFloat
        , toType = Type.FloatValue
        , fromType = Type.floatValue
        , default = 1.0
        , onError = \s -> "`" ++ s ++ "` is not a Float."
        }


int : String -> Block t Int
int =
    stringEntryBlock
        { toString = String.fromInt
        , fromString = String.toInt
        , toType = Type.IntValue
        , fromType = Type.intValue
        , default = 1
        , onError = \s -> "`" ++ s ++ "` is not an Int."
        }


stringEntryBlock :
    { toString : a -> String
    , toType : a -> Type t
    , fromString : String -> Maybe a
    , fromType : Type t -> Maybe a
    , default : a
    , onError : String -> String
    }
    -> String
    -> Block t a
stringEntryBlock c label =
    let
        inner ( stringRef, valueRef ) =
            let
                toType t =
                    [ ( valueRef, c.toType t ) ]

                fromType _ default lookup =
                    lookup valueRef
                        |> Maybe.andThen c.fromType
                        |> Maybe.withDefault default

                controls default =
                    [ \lookup ->
                        let
                            value =
                                fromType default default lookup

                            stringValue =
                                lookup stringRef
                                    |> Maybe.andThen Type.stringValue

                            onUpdate : String -> List ( Ref, Type t )
                            onUpdate s =
                                let
                                    update =
                                        [ ( stringRef, Type.StringValue s ) ]
                                in
                                case c.fromString s of
                                    Nothing ->
                                        update

                                    Just t ->
                                        toType t ++ update

                            error input =
                                case c.fromString input of
                                    Just _ ->
                                        Nothing

                                    Nothing ->
                                        Just (c.onError input)
                        in
                        UI.textField
                            { msg = onUpdate
                            , id = Ref.toString stringRef
                            , label = label
                            , value = stringValue |> Maybe.withDefault (c.toString value)
                            , error = stringValue |> Maybe.andThen error
                            }
                    ]
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = c.default
            , map = always identity
            }
    in
    Block <| State.map inner (Ref.nested (State.map2 Tuple.pair Ref.take Ref.take))


identifier : BlockI t String String
identifier =
    Ref.take
        |> State.map
            (\ref ->
                { fromType = \_ default _ -> default
                , toType = \_ -> []
                , controls = \_ -> []
                , default = Ref.toString ref
                , map = always identity
                }
            )
        |> Block


custom : (t -> Maybe a) -> (a -> t) -> a -> BlockI t a a
custom fromType toType default =
    let
        inner : Ref -> BlockI_ t a a a
        inner ref =
            { fromType =
                \_ def lookup ->
                    lookup ref
                        |> Maybe.andThen Type.customValue
                        |> Maybe.andThen fromType
                        |> Maybe.withDefault def
            , toType =
                \t ->
                    [ ( ref, Type.CustomValue (toType t) ) ]
            , controls = \_ -> []
            , default = default
            , map = always identity
            }
    in
    Block <| State.map inner Ref.take


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
                defaultList :
                    Lookup t
                    -> List i
                    -> Int
                    -> (BlockI_ t i i a -> ( Int, i ) -> x)
                    -> List x
                defaultList lookup default len body =
                    let
                        defaultLen =
                            List.length default
                    in
                    State.traverse
                        (\( index, i ) ->
                            State.map
                                (\b ->
                                    body b ( index, b.fromType i i lookup )
                                )
                                (blockF (String.fromInt index))
                        )
                        (List.indexedMap Tuple.pair <| List.take len default)
                        |> State.andThen
                            (\viaListDefault ->
                                State.traverse
                                    (\index ->
                                        State.map
                                            (\b ->
                                                body b ( index, b.fromType b.default b.default lookup )
                                            )
                                            (defaultLen
                                                + index
                                                |> String.fromInt
                                                |> blockF
                                            )
                                    )
                                    (let
                                        tail =
                                            len - defaultLen
                                     in
                                     if tail > 0 then
                                        List.range 0 (tail - 1)

                                     else
                                        []
                                    )
                                    |> State.map (\viaIDefault -> viaListDefault ++ viaIDefault)
                            )
                        |> Ref.from ref

                fromType : x -> List i -> Lookup t -> List i
                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.withDefaultLazy (\() -> List.length default)
                        |> (\len ->
                                defaultList lookup default len (\_ -> Tuple.second)
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
                                |> Maybe.withDefaultLazy (\() -> List.length default)

                        entryControl b ( _, default_ ) =
                            List.map
                                (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup)
                                (b.controls default_)
                    in
                    UI.vStack [ UI.style "gap" "8px" ]
                        [ UI.text [] [ Html.text listLabel ]
                        , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                            (UI.hStack [ UI.style "gap" "8px" ]
                                [ UI.button [ UI.onClick [ ( ref, Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , UI.button [ UI.onClick [ ( ref, Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]
                                :: List.concat
                                    (defaultList lookup default len entryControl)
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

                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.andThen fromIndex
                        |> Maybe.withDefault default

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
