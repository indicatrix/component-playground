module Controls exposing
    ( Controls, ControlsBuilder
    , builder, add, toControls
    , string, int, float, bool
    , identifier, withPresets, fromLookup, custom, list
    , withUpdate
    , stringEntryBlock
    )

{-| Controls describe how a value of type `m` is stored, retrieved, and
rendered as interactive controls in the playground.


# Types

@docs Controls, ControlsBuilder


# Record Composition

Build controls for record types using a constructor function. Field order must
match constructor argument order.

    Controls.builder (\label value -> { label = label, value = value })
        |> Controls.add "Label" .label Controls.string
        |> Controls.add "Value" .value Controls.string
        |> Controls.toControls

@docs builder, add, toControls


# Primitives

@docs string, int, float, bool


# Other Combinators

@docs identifier, withPresets, fromLookup, custom, list


# Modifiers

@docs withUpdate


# Lower-level

@docs stringEntryBlock

-}

import Array
import Component.Internal as Internal
    exposing
        ( BlockI(..)
        , Builder(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)


{-| Describes how a value of type `m` is stored, retrieved, and rendered as
interactive controls. Compose using `builder`/`add`/`toControls` or use a
primitive directly.
-}
type alias Controls e t m =
    Internal.BlockI e t m m


{-| Intermediate type during record composition. You rarely need to annotate
this explicitly; it appears only in intermediate pipeline steps.
-}
type alias ControlsBuilder e t i m =
    Internal.Builder e t i m i



-- RECORD COMPOSITION


{-| Start building controls for a record type by supplying the constructor
function. Follow with `add` calls (one per field, in constructor argument
order) and finish with `toControls`.
-}
builder : i -> ControlsBuilder e t i m
builder i =
    Builder <|
        \_ ->
            State.state
                { fromType = \_ default _ -> default
                , toType = \_ -> []
                , controls = \_ _ -> []
                , default = i
                , map = always identity
                , update = \_ x -> ( x, [] )
                }


{-| Add a field to a controls builder. The getter extracts the field value from
the final record type `m`; the inner controls describe how to store and render
that field. Field order must match constructor argument order.
-}
add :
    String
    -> (m -> a)
    -> Controls e t a
    -> ControlsBuilder e t (a -> b) m
    -> ControlsBuilder e t b m
add label getter (Block blockF) (Builder stateF) =
    let
        inner :
            Internal.BlockI_ e t (a -> b) m (a -> b)
            -> Internal.BlockI_ e t a a a
            -> Internal.BlockI_ e t b m b
        inner bF b1 =
            let
                fromType : m -> b -> Internal.Lookup t -> b
                fromType end _ lookup =
                    bF.fromType end bF.default lookup (b1.fromType (getter end) (getter end) lookup)

                toType : m -> List ( Ref, Type t )
                toType r =
                    b1.toType (getter r) ++ bF.toType r

                controls : String -> m -> List (Internal.Lookup t -> Html (List ( Ref, Type t )))
                controls outerLabel default =
                    bF.controls outerLabel default ++ b1.controls label (getter default)
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default b1.default
            , map = always identity
            , update = \_ x -> ( x, [] )
            }
    in
    Builder <|
        \lib ->
            stateF lib
                |> State.andThen
                    (\bF ->
                        blockF lib
                            |> State.map (inner bF)
                    )


{-| Finalise a builder into `Controls`. Wraps all field controls in a labelled
group in the UI.
-}
toControls : ControlsBuilder e t m m -> Controls e t m
toControls (Builder bState) =
    let
        wrapControls b outerLabel default =
            [ \lookup ->
                UI.vStack [ UI.style "gap" "8px" ]
                    [ UI.text [] [ Html.text outerLabel ]
                    , UI.vStack
                        [ UI.style "gap" "8px"
                        , UI.style "padding-left" "16px"
                        ]
                        (List.map (\c -> c lookup) (b.controls outerLabel default))
                    ]
            ]
    in
    Block <|
        \lib ->
            State.map
                (\b ->
                    { fromType = b.fromType
                    , toType = b.toType
                    , controls = wrapControls b
                    , default = b.default
                    , map = always identity
                    , update = \_ x -> ( x, [] )
                    }
                )
                (bState lib)



-- PRIMITIVES


{-| Controls for a `String` value. Renders as a text field.
-}
string : Controls e t String
string =
    let
        inner ref =
            let
                toType s =
                    [ ( ref, Type.StringValue s ) ]

                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.withDefault default

                controls label default =
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
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


{-| Controls for a `Float` value. Renders as a text field with float
validation.
-}
float : Controls e t Float
float =
    stringEntryBlock
        { toString = String.fromFloat
        , fromString = String.toFloat
        , toType = Type.FloatValue
        , fromType = Type.floatValue
        , default = 1.0
        , onError = \s -> "`" ++ s ++ "` is not a Float."
        }


{-| Controls for an `Int` value. Renders as a text field with int validation.
-}
int : Controls e t Int
int =
    stringEntryBlock
        { toString = String.fromInt
        , fromString = String.toInt
        , toType = Type.IntValue
        , fromType = Type.intValue
        , default = 1
        , onError = \s -> "`" ++ s ++ "` is not an Int."
        }


{-| Controls for a `Bool` value. Renders as a True/False dropdown.
-}
bool : Controls e t Bool
bool =
    withPresets ( True, "True" ) [ ( False, "False" ) ]


{-| Controls that produce a stable unique string identifier. Has no UI control;
the value is a stable ref-derived string. Useful for `id` attributes.
-}
identifier : Controls e t String
identifier =
    Block <|
        \_ ->
            Ref.take
                |> State.map
                    (\ref ->
                        { fromType = \_ default _ -> default
                        , toType = \_ -> []
                        , controls = \_ _ -> []
                        , default = Ref.toString ref
                        , map = always identity
                        , update = \_ i -> ( i, [] )
                        }
                    )


{-| Controls offering a list of preset values in a dropdown. When the current
value is not in the preset list, the dropdown shows "Custom".

Uses `(==)` internally — not suitable for function values. Use `fromLookup`
instead when your type contains functions.

-}
withPresets : ( a, String ) -> List ( a, String ) -> Controls e t a
withPresets first rest =
    let
        presets =
            first :: rest

        inner : Ref -> Internal.BlockI_ e t a a a
        inner ref =
            let
                values =
                    Array.fromList (List.map Tuple.first presets)

                findIndex a =
                    List.findIndex (\( x, _ ) -> x == a) presets

                fromIndex : Int -> Maybe a
                fromIndex i =
                    Array.get i values

                toType a =
                    Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                        (findIndex a)
                        |> Maybe.withDefault []

                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.andThen fromIndex
                        |> Maybe.withDefault default

                currentIndex default lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.orElseLazy (\() -> findIndex default)

                controls label default lookup =
                    UI.select
                        { msg =
                            String.toInt
                                >> Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                                >> Maybe.withDefault []
                        , id = Ref.toString ref
                        , label = label
                        , value =
                            currentIndex default lookup
                                |> Maybe.map String.fromInt
                                |> Maybe.withDefault ""
                        , options =
                            List.indexedMap
                                (\i ( _, s ) -> { label = s, value = String.fromInt i })
                                presets
                                ++ (case currentIndex default lookup of
                                        Just _ ->
                                            []

                                        Nothing ->
                                            [ { label = "Custom", value = "" } ]
                                   )
                        }
            in
            { fromType = fromType
            , toType = toType
            , controls = \label default -> [ controls label default ]
            , default = Tuple.first first
            , map = always identity
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


{-| Controls backed by a named lookup list. The stored value is the key string;
the rendered value is the associated `a`. Suitable when your type contains
functions (unlike `withPresets` which uses `(==)`).
-}
fromLookup : ( String, a ) -> List ( String, a ) -> Internal.BlockI e t String a
fromLookup first rest =
    let
        inner : Ref -> Internal.BlockI_ e t String String a
        inner ref =
            let
                pairs =
                    first :: rest

                dict =
                    Dict.fromList pairs

                keys =
                    List.map Tuple.first pairs

                toType key =
                    [ ( ref, Type.StringValue key ) ]

                fromType _ default lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.filter (\k -> Dict.member k dict)
                        |> Maybe.withDefault default

                controls label default lookup =
                    UI.select
                        { msg = \k -> [ ( ref, Type.StringValue k ) ]
                        , id = Ref.toString ref
                        , label = label
                        , value =
                            lookup ref
                                |> Maybe.andThen Type.stringValue
                                |> Maybe.withDefault default
                        , options =
                            List.map (\k -> { label = k, value = k }) keys
                        }
            in
            { fromType = fromType
            , toType = toType
            , controls = \label default -> [ controls label default ]
            , default = Tuple.first first
            , map = \_ key -> Dict.get key dict |> Maybe.withDefault (Tuple.second first)
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


{-| Controls backed by custom serialisation functions. Has no UI control;
useful for values that participate in state serialisation but have no editor.
-}
custom : (t -> Maybe a) -> (a -> t) -> a -> Controls e t a
custom fromType_ toType_ default =
    let
        inner : Ref -> Internal.BlockI_ e t a a a
        inner ref =
            { fromType =
                \_ def lookup ->
                    lookup ref
                        |> Maybe.andThen Type.customValue
                        |> Maybe.andThen fromType_
                        |> Maybe.withDefault def
            , toType =
                \val ->
                    [ ( ref, Type.CustomValue (toType_ val) ) ]
            , controls = \_ _ -> []
            , default = default
            , map = always identity
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


{-| Controls for a `List m`, with Add/Remove buttons and per-item controls.
-}
list : Controls e t m -> Controls e t (List m)
list ctrl =
    Block <| \lib -> unwrap lib (listHelper (unwrap lib ctrl))


{-| Attach an update function to controls. The function receives the old model
(before the user interaction) and the new model (after), and returns the final
model plus any side effects.

Use this to implement components with internal behaviour — toggles, accordions,
validated fields — without needing a separate `msg` type variable.

    Controls.withUpdate
        (\old new ->
            -- clamp a value on change
            ( { new | count = clamp 0 100 new.count }, [] )
        )
        myControls

-}
withUpdate : (m -> m -> ( m, List e )) -> Controls e t m -> Controls e t m
withUpdate f (Block blockF) =
    Block <|
        \lib ->
            blockF lib
                |> State.map (\b -> { b | update = f })



-- LOWER-LEVEL


{-| Build controls from explicit string serialisation functions. Used
internally by `int` and `float`; exposed for custom numeric types.
-}
stringEntryBlock :
    { toString : a -> String
    , toType : a -> Type t
    , fromString : String -> Maybe a
    , fromType : Type t -> Maybe a
    , default : a
    , onError : String -> String
    }
    -> Controls e t a
stringEntryBlock c =
    let
        inner ( stringRef, valueRef ) =
            let
                toType t =
                    [ ( valueRef, c.toType t ) ]

                fromType _ default lookup =
                    lookup valueRef
                        |> Maybe.andThen c.fromType
                        |> Maybe.withDefault default

                controls label default =
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
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner (Ref.nested (State.map2 Tuple.pair Ref.take Ref.take))



-- INTERNAL HELPERS


unwrap : Internal.Library e t -> Controls e t a -> State Ref (Internal.BlockI_ e t a a a)
unwrap lib (Block f) =
    f lib


listHelper : State Ref (Internal.BlockI_ e t i i a) -> Internal.BlockI e t (List i) (List a)
listHelper blockState =
    let
        inner : Ref -> Internal.BlockI_ e t (List i) (List i) (List a)
        inner ref =
            let
                defaultList :
                    Internal.Lookup t
                    -> List i
                    -> Int
                    -> (Internal.BlockI_ e t i i a -> ( Int, i ) -> x)
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
                                blockState
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
                                            blockState
                                    )
                                    (let
                                        tail =
                                            len - defaultLen
                                     in
                                     if tail > 0 then
                                        List.range defaultLen (len - 1)

                                     else
                                        []
                                    )
                                    |> State.map (\viaIDefault -> viaListDefault ++ viaIDefault)
                            )
                        |> Ref.from ref

                fromType : x -> List i -> Internal.Lookup t -> List i
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
                                    (\( _, value ) ->
                                        State.map (\b -> b.toType value) blockState
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                control : String -> List i -> Internal.Lookup t -> Html (List ( Ref, Type t ))
                control outerLabel default lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefaultLazy (\() -> List.length default)

                        entryControl b ( index, default_ ) =
                            List.map
                                (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup)
                                (b.controls (String.fromInt index) default_)
                    in
                    UI.vStack [ UI.style "gap" "8px" ]
                        [ UI.text [] [ Html.text outerLabel ]
                        , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                            (UI.hStack [ UI.style "gap" "8px" ]
                                [ UI.button [ UI.onClick [ ( ref, Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , UI.button [ UI.onClick [ ( ref, Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]
                                :: List.concat
                                    (defaultList lookup default len entryControl)
                            )
                        ]

                listMap : Internal.Lookup t -> List i -> List a
                listMap lookup l =
                    State.traverse
                        (\( _, i ) ->
                            State.map
                                (\b -> b.map lookup i)
                                blockState
                        )
                        (List.indexedMap Tuple.pair l)
                        |> Ref.from ref
            in
            { fromType = fromType
            , toType = toType
            , controls = \outerLabel default -> [ control outerLabel default ]
            , default =
                State.traverse
                    (\_ -> State.map .default blockState)
                    (List.range 0 2)
                    |> Ref.from ref
            , map = listMap
            , update = \_ i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take
