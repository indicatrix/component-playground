module Component.Control exposing
    ( Control, Control_, Builder, ComponentInstance, Type
    , string, int, float, bool
    , identifier, fromOptions, fromLookup, componentRef, stringEntry, custom
    , withUpdate, hidden, withDefault, withDescription
    , builder, add, add_, addWhen, addWhen_, toControl, toControl_
    , list, maybe
    )

{-| Define how a value of type `m` is stored, retrieved, and rendered as
interactive controls in the playground.


# Types

@docs Control, Control_, Builder, ComponentInstance, Type


# Primitives

@docs string, int, float, bool
@docs identifier, fromOptions, fromLookup, componentRef, stringEntry, custom


# Modifiers

@docs withUpdate, hidden, withDefault, withDescription


# Composing Types

Build controls for record types using a constructor function. Field order must
match constructor argument order.

    Control.builder (\label value -> { label = label, value = value })
        |> Control.add "Label" .label Control.string
        |> Control.add "Value" .value Control.string
        |> Control.toControl

@docs builder, add, add_, addWhen, addWhen_, toControl, toControl_
@docs list, maybe

-}

import Array
import Component.Application.Theme exposing (Theme)
import Component.Internal as Internal
    exposing
        ( Builder(..)
        , Control(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.Ui as Ui
import Dict
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)


{-| Describes how a value of type `state` is stored, retrieved, and rendered as
interactive controls. Compose using `builder`/`add`/`toControl` or use a
primitive directly.
-}
type alias Control e t state =
    Internal.Control e t state state


{-| Control where the storage type `state` differs from the output type `value`.
Used by `componentRef`, `fromLookup`, `maybe`, and `toControl_`.
-}
type alias Control_ e t state value =
    Internal.Control e t state value


{-| Intermediate type during record composition. `state` is the full record
being built; `value` is the remaining constructor being partially applied.
-}
type alias Builder e t state value =
    Internal.Builder e t value state value


{-| Re-export of `Component.Type.Type` so users of `stringEntry` can annotate
without importing `Component.Type` directly.
-}
type alias Type t =
    Type.Type t


{-| Re-export of `Component.ComponentInstance`. Provided to `withUpdate`
callbacks to identify the owning component instance.
-}
type alias ComponentInstance =
    Internal.ComponentInstance



-- RECORD COMPOSITION


{-| Start building controls for a record type by supplying the constructor
function. Follow with `add` calls (one per field, in constructor argument
order) and finish with `toControl`.
-}
builder : value -> Builder e t state value
builder i =
    Builder <|
        \_ ->
            State.state
                { fromType = \_ default _ -> default
                , toType = \_ -> []
                , controls = \_ _ _ -> []
                , default = i
                , map = always identity
                , update = \_ _ _ x -> ( x, [] )
                , description = Nothing
                }


{-| Add a field to a controls builder. The getter extracts the field value from
the full record type `state`; the inner controls describe how to store and
render that field. Field order must match constructor argument order.
-}
add :
    String
    -> (state -> a)
    -> Control e t a
    -> Builder e t state (a -> value)
    -> Builder e t state value
add label getter (Control controlsF) (Builder stateF) =
    let
        inner :
            Internal.ControlI_ e t (a -> value) state (a -> value)
            -> Internal.ControlI_ e t a a a
            -> Internal.ControlI_ e t value state value
        inner bF b1 =
            let
                fromType : state -> value -> Internal.Lookup t -> value
                fromType end _ lookup =
                    bF.fromType end bF.default lookup (b1.fromType (getter end) (getter end) lookup)

                toType : state -> List ( Ref, Type t )
                toType r =
                    b1.toType (getter r) ++ bF.toType r

                controls : Theme -> Maybe String -> state -> List (Internal.Lookup t -> Html (List ( Ref, Type t )))
                controls theme outerLabel default =
                    bF.controls theme outerLabel default ++ b1.controls theme (Just label) (getter default)
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default b1.default
            , map = always identity
            , update = \_ _ _ x -> ( x, [] )
            , description = Nothing
            }
    in
    Builder <|
        \lib ->
            stateF lib
                |> State.andThen
                    (\bF ->
                        controlsF lib
                            |> State.map (inner bF)
                    )


{-| Finalise a builder into `Control`. Wraps all field controls in a labelled
group in the Ui.
-}
toControl : Builder e t state state -> Control e t state
toControl (Builder bState) =
    let
        wrapControls b theme outerLabel default =
            case outerLabel of
                Nothing ->
                    b.controls theme Nothing default

                Just label ->
                    [ \lookup ->
                        Ui.vStack [ Ui.style "gap" "8px" ]
                            [ Html.div (Ui.labelStyles theme) [ Html.text label ]
                            , Ui.vStack
                                [ Ui.style "gap" "8px"
                                , Ui.style "padding-left" "16px"
                                , Ui.style "border-left" ("1px solid " ++ theme.dividerColor)
                                ]
                                (List.map (\c -> c lookup) (b.controls theme (Just label) default))
                            ]
                    ]
    in
    Control <|
        \lib ->
            State.map
                (\b ->
                    { fromType = b.fromType
                    , toType = b.toType
                    , controls = wrapControls b
                    , default = b.default
                    , map = always identity
                    , update = \_ _ _ x -> ( x, [] )
                    , description = Nothing
                    }
                )
                (bState lib)


{-| Finalise a mapped builder into `Control_`. The builder's constructor must
produce a `{ state, toValue }` record: the storage record and a mapping
function from storage to output.

    Control.builder
        (\branch strVal ->
            { state = { branch = branch, strVal = strVal }
            , toValue = \s ->
                case s.branch of
                    "string" -> Just s.strVal
                    _ -> Nothing
            }
        )
        |> Control.add "Type" .branch (Control.fromOptions ...)
        |> Control.add "Value" .strVal Control.string
        |> Control.toControl_

-}
toControl_ : Builder e t state { state : state, toValue : state -> value } -> Control_ e t state value
toControl_ (Builder bState) =
    Control <|
        \lib ->
            State.map
                (\b ->
                    let
                        wrapControls theme outerLabel default =
                            case outerLabel of
                                Nothing ->
                                    b.controls theme Nothing default

                                Just label_ ->
                                    [ \lookup ->
                                        Ui.vStack [ Ui.style "gap" "8px" ]
                                            [ Html.div (Ui.labelStyles theme) [ Html.text label_ ]
                                            , Ui.vStack
                                                [ Ui.style "gap" "8px"
                                                , Ui.style "padding-left" "16px"
                                                , Ui.style "border-left" ("1px solid " ++ theme.dividerColor)
                                                ]
                                                (List.map (\c -> c lookup)
                                                    (b.controls theme (Just label_) default)
                                                )
                                            ]
                                    ]
                    in
                    { fromType =
                        \r _ lookup ->
                            (b.fromType r b.default lookup).state
                    , toType =
                        \r -> b.toType r
                    , controls = wrapControls
                    , default = b.default.state
                    , map =
                        \lookup state ->
                            -- Rebuild toValue using the current lookup so
                            -- embedded componentRef fields see current state,
                            -- not the defaults captured at init time.
                            (b.fromType state b.default lookup).toValue state
                    , update = \_ _ _ x -> ( x, [] )
                    , description = Nothing
                    }
                )
                (bState lib)


{-| Add a mapped field to a builder. The constructor receives a record with
`state` (the storage value) and `toValue` (the mapping function). Use this
for controls where storage differs from output, like `componentRef`.

    Control.builder
        (\title element ->
            { state = { title = title, refId = element.state }
            , toValue =
                \lookup s ->
                    { title = s.title, element = element.toValue s.refId }
            }
        )
        |> Control.add "Title" .title Control.string
        |> Control.add_ "Element" .refId Control.componentRef
        |> Control.toControl_

-}
add_ :
    String
    -> (state -> innerState)
    -> Control_ e t innerState innerValue
    -> Builder e t state ({ state : innerState, toValue : innerState -> innerValue } -> value)
    -> Builder e t state value
add_ label getter ctrl bldr =
    addWhen_ (always True) label getter ctrl bldr


{-| Like `add`, but the field's controls are only shown when the predicate
returns `True` for the current storage record. The field still participates in
state — it is only hidden from the Ui.

    Control.addWhen (\s -> s.branch == "string") "Value" .strVal Control.string

-}
addWhen :
    (state -> Bool)
    -> String
    -> (state -> a)
    -> Control e t a
    -> Builder e t state (a -> value)
    -> Builder e t state value
addWhen predicate label getter (Control controlsF) (Builder stateF) =
    let
        inner :
            Internal.ControlI_ e t (a -> value) state (a -> value)
            -> Internal.ControlI_ e t a a a
            -> Internal.ControlI_ e t value state value
        inner bF b1 =
            let
                fromType : state -> value -> Internal.Lookup t -> value
                fromType end _ lookup =
                    bF.fromType end bF.default lookup (b1.fromType (getter end) (getter end) lookup)

                toType : state -> List ( Ref, Type t )
                toType r =
                    b1.toType (getter r) ++ bF.toType r

                controls : Theme -> Maybe String -> state -> List (Internal.Lookup t -> Html (List ( Ref, Type t )))
                controls theme outerLabel default =
                    if predicate default then
                        bF.controls theme outerLabel default ++ b1.controls theme (Just label) (getter default)

                    else
                        bF.controls theme outerLabel default
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default b1.default
            , map = always identity
            , update = \_ _ _ x -> ( x, [] )
            , description = Nothing
            }
    in
    Builder <|
        \lib ->
            stateF lib
                |> State.andThen
                    (\bF ->
                        controlsF lib
                            |> State.map (inner bF)
                    )


{-| Like `add_`, but the field's controls are only shown when the predicate
returns `True` for the current storage record. This is the core implementation
that all other `add` variants are built on.
-}
addWhen_ :
    (state -> Bool)
    -> String
    -> (state -> innerState)
    -> Control_ e t innerState innerValue
    -> Builder e t state ({ state : innerState, toValue : innerState -> innerValue } -> value)
    -> Builder e t state value
addWhen_ predicate label getter (Control controlsF) (Builder stateF) =
    let
        inner :
            Internal.ControlI_ e t ({ state : innerState, toValue : innerState -> innerValue } -> value) state ({ state : innerState, toValue : innerState -> innerValue } -> value)
            -> Internal.ControlI_ e t innerState innerState innerValue
            -> Internal.ControlI_ e t value state value
        inner bF b1 =
            let
                fromType : state -> value -> Internal.Lookup t -> value
                fromType end _ lookup =
                    bF.fromType end
                        bF.default
                        lookup
                        { state = b1.fromType (getter end) (getter end) lookup
                        , toValue = b1.map lookup
                        }

                toType : state -> List ( Ref, Type t )
                toType r =
                    b1.toType (getter r) ++ bF.toType r

                controls : Theme -> Maybe String -> state -> List (Internal.Lookup t -> Html (List ( Ref, Type t )))
                controls theme outerLabel default =
                    if predicate default then
                        bF.controls theme outerLabel default ++ b1.controls theme (Just label) (getter default)

                    else
                        bF.controls theme outerLabel default
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default { state = b1.default, toValue = b1.map (always Nothing) }
            , map = always identity
            , update = \_ _ _ x -> ( x, [] )
            , description = Nothing
            }
    in
    Builder <|
        \lib ->
            stateF lib
                |> State.andThen
                    (\bF ->
                        controlsF lib
                            |> State.map (inner bF)
                    )



-- PRIMITIVES


{-| Control for a `String` value. Renders as a text field.
-}
string : Control e t String
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

                controls theme label default =
                    [ \lookup ->
                        Ui.textField theme
                            { msg = toType
                            , id = Ref.toString ref
                            , label = Maybe.withDefault "" label
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
            , update = \_ _ _ i -> ( i, [] )
            , description = Just "String"
            }
    in
    Control <| \_ -> State.map inner Ref.take


{-| Control for a `Float` value. Renders as a text field with float
validation.
-}
float : Control e t Float
float =
    stringEntry
        { toString = String.fromFloat
        , fromString = String.toFloat
        , toType = Type.FloatValue
        , fromType = Type.floatValue
        , default = 1.0
        , onError = \s -> "`" ++ s ++ "` is not a Float."
        , description = "Float"
        }


{-| Control for an `Int` value. Renders as a text field with int validation.
-}
int : Control e t Int
int =
    stringEntry
        { toString = String.fromInt
        , fromString = String.toInt
        , toType = Type.IntValue
        , fromType = Type.intValue
        , default = 1
        , onError = \s -> "`" ++ s ++ "` is not an Int."
        , description = "Integer"
        }


{-| Control for a `Bool` value. Renders as a True/False dropdown.
-}
bool : Control e t Bool
bool =
    fromOptions "Boolean" ( True, "True" ) [ ( False, "False" ) ]


{-| Control that produces a stable unique string identifier. Has no Ui control,
and overriding the default has no effect; the value is a stable ref-derived
string. Useful for `id` attributes.
-}
identifier : Control e t String
identifier =
    Control <|
        \_ ->
            Ref.take
                |> State.map
                    (\ref ->
                        { fromType = \_ _ _ -> Ref.toString ref
                        , toType = \_ -> []
                        , controls = \_ _ _ -> []
                        , default = "pending"
                        , map = always identity
                        , update = \_ _ _ i -> ( i, [] )
                        , description = Nothing
                        }
                    )


{-| Control offering a list of values in a dropdown. When the current
value is not in the option list, the dropdown shows "Custom".

Uses `(==)` internally — not suitable for function values. Use `fromLookup`
instead when your type contains functions.

-}
fromOptions : String -> ( a, String ) -> List ( a, String ) -> Control e t a
fromOptions desc first rest =
    let
        presets =
            first :: rest

        inner : Ref -> Internal.ControlI_ e t a a a
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

                controls theme label default lookup =
                    Ui.select theme
                        { msg =
                            String.toInt
                                >> Maybe.map (\i -> [ ( ref, Type.IntValue i ) ])
                                >> Maybe.withDefault []
                        , id = Ref.toString ref
                        , label = Maybe.withDefault "" label
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
            , controls = \theme label default -> [ controls theme label default ]
            , default = Tuple.first first
            , map = always identity
            , update = \_ _ _ i -> ( i, [] )
            , description = Just desc
            }
    in
    Control <| \_ -> State.map inner Ref.take


{-| Control backed by a named lookup list. The stored value is the key string;
the rendered value is the associated `a`. Suitable when your type contains
functions (unlike `fromOptions` which uses `(==)`).
-}
fromLookup : String -> ( String, a ) -> List ( String, a ) -> Control_ e t String a
fromLookup desc first rest =
    let
        inner : Ref -> Internal.ControlI_ e t String String a
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

                controls theme label default lookup =
                    Ui.select theme
                        { msg = \k -> [ ( ref, Type.StringValue k ) ]
                        , id = Ref.toString ref
                        , label = Maybe.withDefault "" label
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
            , controls = \theme label default -> [ controls theme label default ]
            , default = Tuple.first first
            , map = \_ key -> Dict.get key dict |> Maybe.withDefault (Tuple.second first)
            , update = \_ _ _ i -> ( i, [] )
            , description = Just desc
            }
    in
    Control <| \_ -> State.map inner Ref.take


{-| Control backed by custom serialisation functions. Has no Ui control;
useful for values that participate in state serialisation but have no editor.
-}
custom : (t -> Maybe a) -> (a -> t) -> a -> Control e t a
custom fromType_ toType_ default =
    let
        inner : Ref -> Internal.ControlI_ e t a a a
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
            , controls = \_ _ _ -> []
            , default = default
            , map = always identity
            , update = \_ _ _ i -> ( i, [] )
            , description = Nothing
            }
    in
    Control <| \_ -> State.map inner Ref.take


{-| Control for a list, with Add/Remove buttons and per-item controls.
Works for both simple controls (`Control e t m`) and mapped controls
(`Control_ e t i a`).
-}
list : Control_ e t i a -> Control_ e t (List i) (List a)
list ctrl =
    Control <| \lib -> unwrapMapped lib (listHelper (unwrapMapped lib ctrl))


{-| Control for a `Maybe a` value. Shows an "Enabled" toggle and conditionally
displays the inner control. Stores `{ has : Bool, value : a }` internally.

    Control.maybe Control.string
    -- produces Control_ e t { has : Bool, value : String } (Maybe String)

-}
maybe : Control_ e t i a -> Control_ e t { has : Bool, value : i } (Maybe a)
maybe inner =
    builder
        (\has value ->
            { state = { has = has, value = value.state }
            , toValue =
                \s ->
                    if s.has then
                        Just (value.toValue s.value)

                    else
                        Nothing
            }
        )
        |> add "Enabled" .has bool
        |> addWhen_ .has "Value" .value inner
        |> toControl_


{-| Control that embeds another component by reference. Stores an opaque
`ComponentRef` and renders the referenced component via the library lookup.
The Ui shows a dropdown of all available components (excluding the current
page to prevent recursion).

Use with `Component.toRef` to set default values:

    Control.componentRef
        |> Control.withDefault (Component.toRef myComponent)

-}
componentRef : Control_ e t Internal.ComponentRef (Html (Internal.Update t))
componentRef =
    Control <|
        \((Internal.Library currentPageId lib) as library) ->
            Ref.take
                |> State.map
                    (\slotRef ->
                        let
                            availableComponents =
                                List.filter (\item -> item.id /= currentPageId) lib.index

                            unwrapRef (Internal.ComponentRef id) =
                                id

                            fromType _ default lookup =
                                lookup slotRef
                                    |> Maybe.andThen Type.stringValue
                                    |> Maybe.withDefault (unwrapRef default)
                                    |> Internal.ComponentRef

                            toType ref =
                                [ ( slotRef, Type.StringValue (unwrapRef ref) ) ]

                            renderComponent : Internal.Lookup t -> Internal.ComponentRef -> Html (Internal.Update t)
                            renderComponent lookup ref =
                                let
                                    id =
                                        unwrapRef ref
                                in
                                case lib.lookupDef id of
                                    Just def ->
                                        let
                                            componentE =
                                                Ref.from slotRef (def library)
                                        in
                                        componentE.render lookup
                                            |> Tuple.first

                                    Nothing ->
                                        Html.div [] [ Html.text ("Component not found: " ++ id) ]

                            controls theme label default =
                                [ \lookup ->
                                    let
                                        currentRef =
                                            fromType default default lookup

                                        currentId =
                                            unwrapRef currentRef

                                        unwrapUpdate msg =
                                            let
                                                (Internal.Update _ refs) =
                                                    msg
                                            in
                                            ( slotRef, Type.StringValue currentId ) :: refs

                                        embeddedControls =
                                            case lib.lookupDef currentId of
                                                Just def ->
                                                    Ref.from slotRef (def library)
                                                        |> .controls
                                                        |> (\c -> c theme lookup)
                                                        |> List.map (Html.map unwrapUpdate)

                                                Nothing ->
                                                    []

                                        picker =
                                            Ui.select theme
                                                { msg = \id -> [ ( slotRef, Type.StringValue id ) ]
                                                , id = Ref.toString slotRef
                                                , label = Maybe.withDefault "" label
                                                , value = currentId
                                                , options =
                                                    List.map
                                                        (\item -> { label = item.name, value = item.id })
                                                        availableComponents
                                                }

                                        embeddedBlock =
                                            if List.isEmpty embeddedControls then
                                                []

                                            else
                                                let
                                                    embeddedName =
                                                        List.find (\item -> item.id == currentId) lib.index
                                                            |> Maybe.map .name
                                                            |> Maybe.withDefault currentId
                                                in
                                                [ Html.div (Ui.labelStyles theme) [ Html.text embeddedName ]
                                                , Ui.vStack
                                                    [ Ui.style "gap" "8px"
                                                    , Ui.style "padding-left" "16px"
                                                    , Ui.style "border-left" ("1px solid " ++ theme.dividerColor)
                                                    ]
                                                    embeddedControls
                                                ]
                                    in
                                    Ui.vStack [ Ui.style "gap" "8px" ]
                                        (picker :: embeddedBlock)
                                ]
                        in
                        { fromType = fromType
                        , toType = toType
                        , controls = controls
                        , default =
                            List.head availableComponents
                                |> Maybe.map .id
                                |> Maybe.withDefault ""
                                |> Internal.ComponentRef
                        , map = renderComponent
                        , update = \_ _ _ i -> ( i, [] )
                        , description = Nothing
                        }
                    )


{-| Attach an update function to controls. The function receives the
`ComponentInstance`, a setter that produces Update messages from state,
old state (before the user interaction) and new state (after), and returns
the final state plus any side effects.

Use this to implement components with internal behaviour — toggles, accordions,
validated fields — without needing a separate `msg` type variable. The
`ComponentInstance` parameter provides portal access via
`Component.Application.renderPortal`. The setter lets effect callbacks
(e.g. `Popover.dropdown`'s `onClick`) produce `Update` messages that
dispatch state changes back through the library.

    Control.withUpdate
        (\_ _ old new ->
            -- clamp a value on change
            ( { new | count = clamp 0 100 new.count }, [] )
        )
        myControls

Works on both `Control` and `Control_` — the update function operates on the
storage type in either case.

-}
withUpdate : (ComponentInstance -> (state -> Internal.Update t) -> state -> state -> ( state, List e )) -> Internal.Control e t state value -> Internal.Control e t state value
withUpdate f (Control controlsF) =
    Control <|
        \lib ->
            controlsF lib
                |> State.map (\b -> { b | update = f })


{-| Remove the controls Ui for a value, while keeping it in state
serialisation. The value is always read back as its default.

Use this for values that participate in state (e.g. stable IDs, internal
flags) but should not be editable in the controls panel.

    Control.hidden Control.identifier

-}
hidden : Control_ e t i a -> Control_ e t i a
hidden (Control controlsF) =
    Control <|
        \lib ->
            controlsF lib
                |> State.map (\b -> { b | controls = \_ _ _ -> [] })


{-| Override the default value used when the controls are first rendered or
when a new list item is added. For mapped controls (`Control_`), this sets
the default storage value.
-}
withDefault : i -> Control_ e t i a -> Control_ e t i a
withDefault i (Control f) =
    Control <| \lib -> State.map (\b -> { b | default = i }) (f lib)


{-| Set the label shown for this control when it is used directly as a
component's `controls` (i.e. not nested inside a `builder` group). Overrides
the type-specific default set by primitives such as `int` ("Integer") or
`string` ("Text").

    controls =
        Control.int |> Control.withDescription "Count"

-}
withDescription : String -> Control_ e t i a -> Control_ e t i a
withDescription desc (Control f) =
    Control <| \lib -> State.map (\b -> { b | description = Just desc }) (f lib)



-- LOWER-LEVEL


{-| Build controls from explicit string serialisation functions. Used
internally by `int` and `float`; exposed for custom numeric types.
-}
stringEntry :
    { toString : a -> String
    , toType : a -> Type t
    , fromString : String -> Maybe a
    , fromType : Type t -> Maybe a
    , default : a
    , onError : String -> String
    , description : String
    }
    -> Control e t a
stringEntry c =
    let
        inner ( stringRef, valueRef ) =
            let
                toType t =
                    [ ( valueRef, c.toType t ) ]

                fromType _ default lookup =
                    lookup valueRef
                        |> Maybe.andThen c.fromType
                        |> Maybe.withDefault default

                controls theme label default =
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
                        Ui.textField theme
                            { msg = onUpdate
                            , id = Ref.toString stringRef
                            , label = Maybe.withDefault "" label
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
            , update = \_ _ _ i -> ( i, [] )
            , description = Just c.description
            }
    in
    Control <| \_ -> State.map inner (Ref.nested (State.map2 Tuple.pair Ref.take Ref.take))



-- INTERNAL HELPERS


unwrapMapped : Internal.Library e t -> Control_ e t i a -> State Ref (Internal.ControlI_ e t i i a)
unwrapMapped lib (Control f) =
    f lib


listHelper : State Ref (Internal.ControlI_ e t i i a) -> Control_ e t (List i) (List a)
listHelper controlsState =
    let
        inner : Ref -> Internal.ControlI_ e t (List i) (List i) (List a)
        inner ref =
            let
                defaultList :
                    Internal.Lookup t
                    -> List i
                    -> Int
                    -> (Internal.ControlI_ e t i i a -> ( Int, i ) -> x)
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
                                controlsState
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
                                            controlsState
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
                                        State.map (\b -> b.toType value) controlsState
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                control : Theme -> Maybe String -> List i -> Internal.Lookup t -> Html (List ( Ref, Type t ))
                control theme outerLabel default lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefaultLazy (\() -> List.length default)

                        entryControl b ( index, default_ ) =
                            List.map
                                (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup)
                                (b.controls theme (Just (String.fromInt index)) default_)

                        buttons =
                            Ui.hStack [ Ui.style "gap" "8px" ]
                                [ Ui.button theme [ Ui.onClick [ ( ref, Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , Ui.button theme [ Ui.onClick [ ( ref, Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]

                        items =
                            buttons :: List.concat (defaultList lookup default len entryControl)
                    in
                    case outerLabel of
                        Nothing ->
                            Ui.vStack [ Ui.style "gap" "8px" ] items

                        Just label ->
                            Ui.vStack [ Ui.style "gap" "8px" ]
                                [ Html.div (Ui.labelStyles theme) [ Html.text label ]
                                , Ui.vStack
                                    [ Ui.style "gap" "8px"
                                    , Ui.style "padding-left" "16px"
                                    , Ui.style "border-left" ("1px solid " ++ theme.dividerColor)
                                    ]
                                    items
                                ]

                listMap : Internal.Lookup t -> List i -> List a
                listMap lookup l =
                    State.traverse
                        (\( _, i ) ->
                            State.map
                                (\b -> b.map lookup i)
                                controlsState
                        )
                        (List.indexedMap Tuple.pair l)
                        |> Ref.from ref
            in
            { fromType = fromType
            , toType = toType
            , controls = \theme outerLabel default -> [ control theme outerLabel default ]
            , default =
                State.traverse
                    (\_ -> State.map .default controlsState)
                    (List.range 0 2)
                    |> Ref.from ref
            , map = listMap
            , update = \_ _ _ i -> ( i, [] )
            , description = Nothing
            }
    in
    Control <| \_ -> State.map inner Ref.take
