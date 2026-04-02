module Component exposing
    ( Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Update, Preview, PreviewGroup, Ref, Type, View
    , group
    , new, withControl, withControl_, withState, withState_, withStateF, withStateF_, withUnlabelledState, withUnlabelledState_, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelled, withUnlabelled_, withComponent, withComponent_, withMsg, withMsg2, withMsg3, withMsgF, withUpdateF, map, fromPreview
    , string, int, float, bool, withPresets, fromLookup, identifier, list, list2, custom, previewBlock, stringEntryBlock
    , build, addVia, finish, finish_, withDefault
    , toPreview, toPortalPreview
    , toComponentUpdate
    )

{-| Component Playground - an interactive component testing library for Elm.

Build interactive previews of your UI components with configurable controls.


# Types

Core types for building components and previews. Type definitions live in
Component.Internal to preserve invariants.

@docs Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Update, Preview, PreviewGroup, Ref, Type, View


# Groups

Organize previews into named groups.

@docs group


# Constructing Components

Build components by composing controls and state. Functions ending in `_` don't
set a default value (use `withDefault` separately for stable defaults).

@docs new, withControl, withControl_, withState, withState_, withStateF, withStateF_, withUnlabelledState, withUnlabelledState_, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelled, withUnlabelled_, withComponent, withComponent_, withMsg, withMsg2, withMsg3, withMsgF, withUpdateF, map, fromPreview


# Blocks

Blocks define how values are stored, retrieved, and displayed as controls.
Primitive blocks for common types, plus combinators for building complex ones.

@docs string, int, float, bool, withPresets, fromLookup, identifier, list, list2, custom, previewBlock, stringEntryBlock


# Building Composite Blocks

Build blocks for record types by composing field blocks.

@docs build, addVia, finish, finish_, withDefault


# Constructing Previews

Convert components to previews with metadata for display in the playground.

@docs toPreview, toPortalPreview


# Updates

Wrap application updates for use with stateful components.

@docs toComponentUpdate

-}

import Array
import Component.Internal as Internal
    exposing
        ( BlockI(..)
        , Builder(..)
        , Component(..)
        , ComponentRef(..)
        , Library(..)
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)



-- TYPE RE-EXPORTS


type alias Library e t =
    Internal.Library e t


type alias Component e t a =
    Internal.Component e t a


type alias Preview e t =
    Internal.Preview e t


type alias PreviewGroup e t =
    Internal.PreviewGroup e t


type alias Block e t a =
    Internal.Block e t a


type alias BlockI e t i a =
    Internal.BlockI e t i a


type alias Lookup t =
    Internal.Lookup t


type alias Update t e =
    Internal.Update t e


type alias View msg =
    Internal.View msg


type alias Builder e t i r a =
    Internal.Builder e t i r a


type alias ComponentRef =
    Internal.ComponentRef


type alias Ref =
    Ref.Ref


type alias Type t =
    Type.Type t



-- COMPONENT FUNCTIONS


{-| Create a new component
-}
new : a -> Component e t a
new value =
    Component <|
        { value = \_ _ -> State.state value
        , controls = \_ -> State.state []

        -- This is set here ONLY and passed through all with* calls to ensure
        -- this is a stable reference regardless of how many with* calls have
        -- been made.
        , reference = State.get
        }


group : String -> List (Preview e t) -> PreviewGroup e t
group name previews =
    { name = name, previews = previews }


map : (a -> b) -> Component e t a -> Component e t b
map f (Component p) =
    Component <|
        { value = \lib l -> State.map f (p.value lib l)
        , controls = p.controls
        , reference = p.reference
        }


toPreview : { id : String, name : String } -> Component e t (Html (Update t e)) -> Preview e t
toPreview meta component =
    ( meta, map (\html -> ( html, Dict.empty )) component )


toPortalPreview : { id : String, name : String } -> Component e t (View (Update t e)) -> Preview e t
toPortalPreview meta component =
    ( meta, component )


toComponentUpdate : e -> Update t e
toComponentUpdate effect =
    WithEffect [] [ effect ]


fromPreview : Preview e t -> ComponentRef
fromPreview ( meta, _ ) =
    ComponentRef meta.id


withControl : String -> BlockI e t i a -> i -> Component e t (a -> b) -> Component e t b
withControl label block default =
    withControl_ label (withDefault default block)


withControl_ : String -> BlockI e t i a -> Component e t (a -> b) -> Component e t b
withControl_ label block =
    withHelper label block <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withMsg :
    (a -> e)
    -> Component e t ((a -> Update t e) -> r)
    -> Component e t r
withMsg toEffect (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a -> WithEffect [] [ toEffect a ]))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg2 :
    (a -> b -> e)
    -> Component e t ((a -> b -> Update t e) -> r)
    -> Component e t r
withMsg2 toEffect (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a b -> WithEffect [] [ toEffect a b ]))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg3 :
    (a -> b -> c -> e)
    -> Component e t ((a -> b -> c -> Update t e) -> r)
    -> Component e t r
withMsg3 toEffect (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a b c -> WithEffect [] [ toEffect a b c ]))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withState : String -> BlockI e t i a -> i -> Component e t (a -> (i -> Update t e) -> y) -> Component e t y
withState label block default =
    withState_ label (withDefault default block)


withState_ : String -> BlockI e t i a -> Component e t (a -> (i -> Update t e) -> y) -> Component e t y
withState_ label block =
    withStateInternal label block (\get set f -> f get set)


withStateInternal :
    String
    -> BlockI e t i a
    -> (a -> (i -> Update t e) -> x -> y)
    -> Component e t x
    -> Component e t y
withStateInternal label block f =
    withHelper label block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (\i -> Update (b.toType i) [])
                x


withStateF : String -> BlockI e t i a -> i -> (Ref -> a -> (i -> e -> Update t e) -> x -> y) -> Component e t x -> Component e t y
withStateF label block default =
    withStateF_ label (withDefault default block)


withStateF_ : String -> BlockI e t i a -> (Ref -> a -> (i -> e -> Update t e) -> x -> y) -> Component e t x -> Component e t y
withStateF_ label block =
    withStateFInternal label block


withStateFInternal :
    String
    -> BlockI e t i a
    -> (Ref -> a -> (i -> e -> Update t e) -> x -> y)
    -> Component e t x
    -> Component e t y
withStateFInternal label block f =
    withHelper label block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\i effect -> WithEffect (b.toType i) [ effect ])
                x


withUnlabelledState : BlockI e t i a -> i -> Component e t (a -> (i -> Update t e) -> b) -> Component e t b
withUnlabelledState block default =
    withUnlabelledState_ (withDefault default block)


withUnlabelledState_ : BlockI e t i a -> Component e t (a -> (i -> Update t e) -> b) -> Component e t b
withUnlabelledState_ block =
    withUnlabelledStateInternal block (\get set f -> f get set)


withUnlabelledStateInternal :
    BlockI e t i a
    -> (a -> (i -> Update t e) -> x -> y)
    -> Component e t x
    -> Component e t y
withUnlabelledStateInternal block f =
    withHelperUnlabelled block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (\i -> Update (b.toType i) [])
                x


withUnlabelledStateF : BlockI e t i a -> i -> (Ref -> a -> (i -> e -> Update t e) -> x -> y) -> Component e t x -> Component e t y
withUnlabelledStateF block default =
    withUnlabelledStateF_ (withDefault default block)


withUnlabelledStateF_ : BlockI e t i a -> (Ref -> a -> (i -> e -> Update t e) -> x -> y) -> Component e t x -> Component e t y
withUnlabelledStateF_ block f =
    withHelperUnlabelled block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\i effect -> WithEffect (b.toType i) [ effect ])
                x


withUpdateF :
    String
    -> BlockI e t i a
    -> (Ref -> a -> ((a -> ( i, e )) -> Update t e) -> x -> y)
    -> Component e t x
    -> Component e t y
withUpdateF label block f =
    withHelper label block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\body ->
                    Computed
                        (\l ->
                            let
                                a =
                                    b.fromType b.default b.default l |> b.map l

                                ( i, effect ) =
                                    body a
                            in
                            ( b.toType i, [ effect ] )
                        )
                )
                x


withMsgF :
    ((e -> Update t e) -> x -> y)
    -> Component e t x
    -> Component e t y
withMsgF f (Component p) =
    Component <|
        { value =
            \lib lookup ->
                let
                    helper =
                        f (\effect -> Computed (always ( [], [ effect ] )))
                in
                State.map helper (p.value lib lookup)
        , controls = p.controls
        , reference = p.reference
        }


withUnlabelled : BlockI e t i a -> i -> Component e t (a -> b) -> Component e t b
withUnlabelled block default =
    withUnlabelled_ (withDefault default block)


withUnlabelled_ : BlockI e t i a -> Component e t (a -> b) -> Component e t b
withUnlabelled_ block =
    withHelperUnlabelled block <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withComponent : String -> (Library e t -> BlockI e t i b) -> i -> Component e t (b -> a) -> Component e t a
withComponent label block default =
    withComponent_ label (\lib -> withDefault default (block lib))


withComponent_ : String -> (Library e t -> BlockI e t i b) -> Component e t (b -> a) -> Component e t a
withComponent_ label blockF (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map2
                    (\f b ->
                        f (b.fromType b.default b.default lookup |> b.map lookup)
                    )
                    (p.value lib lookup)
                    (unwrap lib (blockF lib))
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ List.map (wrapControl b) (b.controls label b.default))
                    (p.controls lib)
                    (unwrap lib (blockF lib))
        , reference = p.reference
        }


withHelper :
    String
    -> BlockI e t i a
    -> (Library e t -> Lookup t -> Ref -> b -> Internal.BlockI_ e t i i a -> c)
    -> Component e t b
    -> Component e t c
withHelper label (Block bF) body (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map3 (body lib lookup) p.reference (p.value lib lookup) (bF lib)
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ List.map (wrapControl b) (b.controls label b.default))
                    (p.controls lib)
                    (bF lib)
        , reference = p.reference
        }


withHelperUnlabelled :
    BlockI e t i a
    -> (Library e t -> Lookup t -> Ref -> b -> Internal.BlockI_ e t i i a -> c)
    -> Component e t b
    -> Component e t c
withHelperUnlabelled (Block bF) body (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map3 (body lib lookup) p.reference (p.value lib lookup) (bF lib)
        , controls = \lib -> State.map2 always (p.controls lib) (bF lib)
        , reference = p.reference
        }


{-| Wrap a block control to call the update function after state changes.
-}
wrapControl :
    Internal.BlockI_ e t i i a
    -> (Lookup t -> Html (List ( Ref, Type t )))
    -> (Lookup t -> Html ( List ( Ref, Type t ), List e ))
wrapControl b ctrl lookup =
    ctrl lookup
        |> Html.map
            (\rawChanges ->
                let
                    patchedLookup ref =
                        List.find (\( r, _ ) -> r == ref) rawChanges
                            |> Maybe.map Tuple.second
                            |> Maybe.orElseLazy (\() -> lookup ref)

                    i =
                        b.fromType b.default b.default patchedLookup

                    ( i2, effects ) =
                        b.update i

                    ownedChanges =
                        b.toType i2

                    ownedRefs =
                        List.map Tuple.first ownedChanges

                    foreignChanges =
                        List.filter (\( r, _ ) -> not (List.member r ownedRefs)) rawChanges
                in
                ( ownedChanges ++ foreignChanges, effects )
            )


previewBlock : BlockI e t ComponentRef (Html (Update t e))
previewBlock =
    Block <|
        \((Library currentComponentId lib_) as lib) ->
            let
                inner : Ref -> Internal.BlockI_ e t ComponentRef ComponentRef (Html (Update t e))
                inner ref =
                    let
                        controlUI : String -> String -> List (Html ( List ( Ref, Type t ), List e )) -> Html (List ( Ref, Type t ))
                        controlUI label previewId componentControls =
                            UI.vStack [ UI.style "gap" "8px" ]
                                [ UI.text [] [ Html.text label ]
                                , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                                    (UI.select
                                        { id = Ref.toString ref
                                        , label = "Component"
                                        , options =
                                            List.filterMap
                                                (\i ->
                                                    if i.id == currentComponentId then
                                                        Nothing

                                                    else
                                                        Just { value = i.id, label = i.name }
                                                )
                                                lib_.index
                                        , value = previewId
                                        , msg =
                                            \selected ->
                                                [ ( ref, selected |> Type.StringValue ) ]
                                        }
                                        :: List.map
                                            (Html.map (\( state, _ ) -> ( ref, Type.StringValue previewId ) :: state))
                                            componentControls
                                    )
                                ]

                        control : String -> ComponentRef -> Lookup t -> Html (List ( Ref, Type t ))
                        control label (ComponentRef default) lookup =
                            lookup ref
                                |> Maybe.andThen Type.stringValue
                                |> Maybe.withDefault default
                                |> (\id ->
                                        lib_.lookup id
                                            |> Maybe.map
                                                (\( pId, Component p ) ->
                                                    let
                                                        controls =
                                                            Ref.from ref (p.controls lib)
                                                    in
                                                    controlUI label pId <|
                                                        List.map (\c -> c lookup) controls
                                                )
                                            |> Maybe.withDefault (controlUI label id [])
                                   )

                        mapF : Lookup t -> ComponentRef -> Html (Update t e)
                        mapF lookup (ComponentRef id) =
                            lib_.lookup id
                                |> Maybe.map (\( _, Component p ) -> Tuple.first <| Ref.from ref (p.value lib lookup))
                                |> Maybe.withDefault
                                    (Html.div []
                                        [ Html.text "Component not found"
                                        ]
                                    )
                    in
                    { fromType =
                        \_ default lookup ->
                            lookup ref
                                |> Maybe.andThen Type.stringValue
                                |> Maybe.map ComponentRef
                                |> Maybe.withDefault default
                    , toType = \(ComponentRef s) -> [ ( ref, Type.StringValue s ) ]
                    , controls = \label default -> [ control label default ]
                    , default =
                        List.head lib_.index
                            |> Maybe.map (.id >> ComponentRef)
                            |> Maybe.withDefault (ComponentRef "not-found")
                    , map = mapF
                    , update = \i -> ( i, [] )
                    }
            in
            Ref.withNestedRef inner



-- BLOCK FUNCTIONS


unwrap : Library e t -> BlockI e t i a -> State Ref (Internal.BlockI_ e t i i a)
unwrap lib (Block f) =
    f lib


{-| Override the default value for a block. Use this to get a stable default
to be referenced in multiple places. Use this when using
Component.Application.updateAt.

withDefault is used to set the initial value when building Components with
withControl, withState etc., but not when using withControl\_, withState\_, etc.

-}
withDefault : i -> BlockI e t i a -> BlockI e t i a
withDefault i (Block f) =
    Block <| \lib -> State.map (\b -> { b | default = i }) (f lib)


build : i -> Builder e t i r i
build i =
    Builder <|
        \_ ->
            State.state
                { fromType = \_ default _ -> default
                , toType = \_ -> []
                , controls = \_ _ -> []
                , default = i
                , map = always identity
                , update = \x -> ( x, [] )
                }


addVia :
    (r -> a)
    -> String
    -> BlockI e t a a
    -> Builder e t (a -> b) r (a -> b)
    -> Builder e t b r b
addVia fa label block (Builder stateF) =
    let
        inner : Internal.BlockI_ e t (a -> b) r (a -> b) -> Internal.BlockI_ e t a a a -> Internal.BlockI_ e t b r b
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

                controls : String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))
                controls outerLabel default =
                    bF.controls outerLabel default ++ b1.controls label (fa default)
            in
            { fromType = fromType
            , toType = toType
            , controls = controls
            , default = bF.default b1.default
            , map = always identity
            , update = \x -> ( x, [] )
            }
    in
    Builder <|
        \lib ->
            stateF lib
                |> State.andThen
                    (\bF -> unwrap lib block |> State.map (inner bF))


finish : (i -> a) -> Builder e t i i i -> BlockI e t i a
finish f =
    finishI f


finish_ : Builder e t a a a -> BlockI e t a a
finish_ =
    finishI identity


finishI : (i -> a) -> Builder e t i i i -> BlockI e t i a
finishI f (Builder bState) =
    let
        controls b outerLabel default =
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
                    , controls = controls b
                    , default = b.default
                    , map = always f
                    , update = \x -> ( x, [] )
                    }
                )
                (bState lib)


string : Block e t String
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
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


float : Block e t Float
float =
    stringEntryBlock
        { toString = String.fromFloat
        , fromString = String.toFloat
        , toType = Type.FloatValue
        , fromType = Type.floatValue
        , default = 1.0
        , onError = \s -> "`" ++ s ++ "` is not a Float."
        }


int : Block e t Int
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
    -> Block e t a
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
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner (Ref.nested (State.map2 Tuple.pair Ref.take Ref.take))


identifier : BlockI e t String String
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
                        , update = \i -> ( i, [] )
                        }
                    )


custom : (t -> Maybe a) -> (a -> t) -> a -> BlockI e t a a
custom fromType toType default =
    let
        inner : Ref -> Internal.BlockI_ e t a a a
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
            , controls = \_ _ -> []
            , default = default
            , map = always identity
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


list : BlockI e t i a -> BlockI e t (List i) (List a)
list block =
    Block <| \lib -> unwrap lib (listHelper (unwrap lib block))


list2 : (g -> BlockI e t i a) -> g -> BlockI e t (List i) (List a)
list2 blockF dep =
    Block <| \lib -> unwrap lib (listHelper (unwrap lib (blockF dep)))


listHelper : State Ref (Internal.BlockI_ e t i i a) -> BlockI e t (List i) (List a)
listHelper blockState =
    let
        inner : Ref -> Internal.BlockI_ e t (List i) (List i) (List a)
        inner ref =
            let
                defaultList :
                    Lookup t
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
                                    (\( _, value ) ->
                                        State.map (\b -> b.toType value) blockState
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                control : String -> List i -> Lookup t -> Html (List ( Ref, Type t ))
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

                listMap : Lookup t -> List i -> List a
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
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


{-| A block that offers a list of preset values in a dropdown. When the current
value is not in the preset list, the dropdown shows "Custom".

Uses (==) internally — not suitable for function values. Use `fromLookup` instead
when your type contains functions (e.g. sum types represented as constructor functions).

-}
withPresets : ( a, String ) -> List ( a, String ) -> Block e t a
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
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


fromLookup : ( String, a ) -> List ( String, a ) -> BlockI e t String a
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
            , update = \i -> ( i, [] )
            }
    in
    Block <| \_ -> State.map inner Ref.take


bool : Block e t Bool
bool =
    withPresets ( True, "True" ) [ ( False, "False" ) ]
