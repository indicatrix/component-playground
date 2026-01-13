module Component exposing
    ( Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Msg, Preview, PreviewGroup, Ref, Type, View
    , group
    , new, withComponent, withComponent_, withControl, withControl_, withMsg, withMsg2, withMsg3, withState, withStateF, withStateF_, withState_, withUnlabelled, withUnlabelledState, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelledState_, withUnlabelled_, withUpdateF, withMsgF, fromPreview, map
    , previewBlock, identifier, list, list2, bool, int, float, string, oneOf, stringEntryBlock, custom
    , addVia, build, finish, finish_
    , toPortalPreview, toPreview
    , toComponentMsg, withDefault
    )

{-| TODO: write a description of the module, and write descriptions for each section of the docs


# Re-exported Aliases

These opaque types are defined and exported from submodules. They are aliased
and exported here so that it is possible to write explicit type signatures.

@docs Block, BlockI, Builder, Component, ComponentRef, Library, Lookup, Msg, Preview, PreviewGroup, Ref, Type, View


# Groups

@docs group


# Constructing Components

@docs new, withComponent, withComponent_, withControl, withControl_, withMsg, withMsg2, withMsg3, withState, withStateF, withStateF_, withState_, withUnlabelled, withUnlabelledState, withUnlabelledStateF, withUnlabelledStateF_, withUnlabelledState_, withUnlabelled_, withUpdateF, withMsgF, fromPreview, map


# Blocks

@docs previewBlock, identifier, list, list2, bool, int, float, string, oneOf, stringEntryBlock, custom


# Building Blocks

@docs addVia, build, finish, finish\_


# Constructing Previews

@docs toPortalPreview, toPreview


# Messages

@docs toComponentMsg

-}

import Array
import Component.Internal as Internal
    exposing
        ( BlockI(..)
        , Builder(..)
        , Component(..)
        , ComponentRef(..)
        , Library(..)
        , Msg(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)



-- TYPE RE-EXPORTS


type alias Library t msg =
    Internal.Library t msg


type alias Component t msg a =
    Internal.Component t msg a


type alias Preview t msg =
    Internal.Preview t msg


type alias PreviewGroup t msg =
    Internal.PreviewGroup t msg


type alias Block t a =
    Internal.Block t a


type alias BlockI t i a =
    Internal.BlockI t i a


type alias Lookup t =
    Internal.Lookup t


type alias Msg t msg =
    Internal.Msg t msg


type alias View msg =
    Internal.View msg


type alias Builder t i r a =
    Internal.Builder t i r a


type alias ComponentRef =
    Internal.ComponentRef


type alias Ref =
    Ref.Ref


type alias Type t =
    Type.Type t



-- COMPONENT FUNCTIONS


{-| Create a new component
-}
new : a -> Component t msg a
new value =
    Component <|
        { value = \_ _ -> State.state value
        , controls = \_ -> State.state []

        -- This is set here ONLY and passed through all with* calls to ensure
        -- this is a stable reference regardless of how many with* calls have
        -- been made.
        , reference = State.get
        }


group : String -> List (Preview t msg) -> PreviewGroup t msg
group name previews =
    { name = name, previews = previews }


map : (a -> b) -> Component t msg a -> Component t msg b
map f (Component p) =
    Component <|
        { value = \lib l -> State.map f (p.value lib l)
        , controls = p.controls
        , reference = p.reference
        }


toPreview : { id : String, name : String } -> Component t msg (Html msg) -> Preview t msg
toPreview meta component =
    ( meta, map (\html -> ( html, Dict.empty )) component )


toPortalPreview : { id : String, name : String } -> Component t msg (View msg) -> Preview t msg
toPortalPreview meta component =
    ( meta, component )


toComponentMsg : msg -> Msg t msg
toComponentMsg msg =
    Msg [] msg


fromPreview : Preview t msg -> ComponentRef
fromPreview ( meta, _ ) =
    ComponentRef meta.id


withControl : String -> (String -> Block t a) -> a -> Component t msg (a -> b) -> Component t msg b
withControl label block default =
    withControl_ label (\l -> withDefault default (block l))


withControl_ : String -> (String -> Block t a) -> Component t msg (a -> b) -> Component t msg b
withControl_ label blockF =
    withHelper (blockF label) <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withMsg :
    (a -> msg)
    -> Component t (Msg t msg) ((a -> Msg t msg) -> r)
    -> Component t (Msg t msg) r
withMsg msg (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a -> Msg [] (msg a)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg2 :
    (a -> b -> msg)
    -> Component t (Msg t msg) ((a -> b -> Msg t msg) -> r)
    -> Component t (Msg t msg) r
withMsg2 msg (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a b -> Msg [] (msg a b)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg3 :
    (a -> b -> c -> msg)
    -> Component t (Msg t msg) ((a -> b -> c -> Msg t msg) -> r)
    -> Component t (Msg t msg) r
withMsg3 msg (Component p) =
    Component <|
        { value =
            \pl l ->
                State.map
                    (\f -> f (\a b c -> Msg [] (msg a b c)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withState : String -> (String -> BlockI t i a) -> i -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Component t (Msg t msg) y
withState label blockF default =
    withState_ label (\l -> withDefault default (blockF l))


withState_ : String -> (String -> BlockI t i a) -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> y) -> Component t (Msg t msg) y
withState_ label blockF =
    withStateInternal (blockF label) (\get set f -> f get set)


withStateInternal :
    BlockI t i a
    -> (a -> (i -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withStateInternal block f =
    withHelper block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (b.toType >> SetState)
                x


withStateF : String -> (String -> BlockI t i a) -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withStateF label blockF default =
    withStateF_ label (\l -> withDefault default (blockF l))


withStateF_ : String -> (String -> BlockI t i a) -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withStateF_ label blockF =
    withStateFInternal (blockF label)


withStateFInternal :
    BlockI t i a
    -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withStateFInternal block f =
    withHelper block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\i msg -> Msg (b.toType i) msg)
                x


withUnlabelledState : BlockI t i a -> i -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Component t (Msg t msg) b
withUnlabelledState block default =
    withUnlabelledState_ (withDefault default block)


withUnlabelledState_ : BlockI t i a -> Component t (Msg t msg) (a -> (i -> Msg t msg) -> b) -> Component t (Msg t msg) b
withUnlabelledState_ block =
    withUnlabelledStateInternal block (\get set f -> f get set)


withUnlabelledStateInternal :
    BlockI t i a
    -> (a -> (i -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withUnlabelledStateInternal block f =
    withHelperUnlabelled block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (b.toType >> SetState)
                x


withUnlabelledStateF : BlockI t i a -> i -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withUnlabelledStateF block default =
    withUnlabelledStateF_ (withDefault default block)


withUnlabelledStateF_ : BlockI t i a -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y) -> Component t (Msg t msg) x -> Component t (Msg t msg) y
withUnlabelledStateF_ =
    withStateFInternal


withUpdateF :
    BlockI t i a
    -> (Ref -> a -> ((a -> ( i, msg )) -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withUpdateF block f =
    withHelper block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\body ->
                    Update
                        (\l ->
                            let
                                a =
                                    b.fromType b.default b.default l |> b.map l

                                ( i, msg ) =
                                    body a
                            in
                            ( b.toType i, msg )
                        )
                )
                x


withMsgF :
    ((msg -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withMsgF f (Component p) =
    Component <|
        { value =
            \lib lookup ->
                let
                    helper =
                        f (\msg -> Update (always ( [], msg )))
                in
                State.map helper (p.value lib lookup)
        , controls = p.controls
        , reference = p.reference
        }


withUnlabelled : BlockI t i a -> i -> Component t msg (a -> b) -> Component t msg b
withUnlabelled block default =
    withUnlabelled_ (withDefault default block)


withUnlabelled_ : BlockI t i a -> Component t msg (a -> b) -> Component t msg b
withUnlabelled_ block =
    withHelperUnlabelled block <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withComponent : String -> (Library t msg -> String -> BlockI t i b) -> i -> Component t msg (b -> a) -> Component t msg a
withComponent label block default =
    withComponent_ label (\lib l -> withDefault default (block lib l))


withComponent_ : String -> (Library t msg -> String -> BlockI t i b) -> Component t msg (b -> a) -> Component t msg a
withComponent_ label blockF (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map2
                    (\f b ->
                        f (b.fromType b.default b.default lookup |> b.map lookup)
                    )
                    (p.value lib lookup)
                    (unwrap <| blockF lib label)
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ b.controls b.default)
                    (p.controls lib)
                    (unwrap <| blockF lib label)
        , reference = p.reference
        }


withHelper :
    BlockI t i a
    -> (Library t msg -> Lookup t -> Ref -> b -> Internal.BlockI_ t i i a -> c)
    -> Component t msg b
    -> Component t msg c
withHelper (Block bState) body (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map3 (body lib lookup) p.reference (p.value lib lookup) bState
        , controls =
            \lib ->
                State.map2 (\c b -> c ++ b.controls b.default) (p.controls lib) bState
        , reference = p.reference
        }


withHelperUnlabelled :
    BlockI t i a
    -> (Library t msg -> Lookup t -> Ref -> b -> Internal.BlockI_ t i i a -> c)
    -> Component t msg b
    -> Component t msg c
withHelperUnlabelled (Block bState) body (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map3 (body lib lookup) p.reference (p.value lib lookup) bState
        , controls = \lib -> State.map2 always (p.controls lib) bState
        , reference = p.reference
        }


previewBlock : Library t msg -> String -> BlockI t ComponentRef (Html msg)
previewBlock ((Library currentComponentId lib_) as lib) label =
    let
        inner : Ref -> Internal.BlockI_ t ComponentRef ComponentRef (Html msg)
        inner ref =
            let
                controlUI : String -> List (Html (List ( Ref, Type t ))) -> Html (List ( Ref, Type t ))
                controlUI previewId componentControls =
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
                                    (Html.map ((::) ( ref, Type.StringValue previewId )))
                                    componentControls
                            )
                        ]

                control : ComponentRef -> Lookup t -> Html (List ( Ref, Type t ))
                control (ComponentRef default) lookup =
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
                                            controlUI pId <|
                                                List.map (\c -> c lookup) controls
                                        )
                                    |> Maybe.withDefault (controlUI id [])
                           )

                mapF : Lookup t -> ComponentRef -> Html msg
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
            , controls = \default -> [ control default ]
            , default =
                List.head lib_.index
                    |> Maybe.map (.id >> ComponentRef)
                    |> Maybe.withDefault (ComponentRef "not-found")
            , map = mapF
            }
    in
    Ref.withNestedRef inner |> Block



-- BLOCK FUNCTIONS


unwrap : BlockI t i a -> State Ref (Internal.BlockI_ t i i a)
unwrap (Block bState) =
    bState


{-| Override the default value for a block. Use this to get a stable default
to be referenced in multiple places. Use this when using
Component.Application.updateAt.

withDefault is used to set the initial value when building Components with
withControl, withState etc., but not when using withControl\_, withState\_, etc.

-}
withDefault : i -> BlockI t i a -> BlockI t i a
withDefault i (Block state) =
    Block <| State.map (\b -> { b | default = i }) state


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
        inner : Internal.BlockI_ t (a -> b) r (a -> b) -> Internal.BlockI_ t a a a -> Internal.BlockI_ t b r b
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


finish : (i -> a) -> Builder t i i i -> String -> BlockI t i a
finish f =
    finishI f


finish_ : Builder t a a a -> String -> BlockI t a a
finish_ =
    finishI identity


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
        inner : Ref -> Internal.BlockI_ t a a a
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


listHelper : (String -> State Ref (Internal.BlockI_ t i i a)) -> String -> BlockI t (List i) (List a)
listHelper blockF listLabel =
    let
        inner : Ref -> Internal.BlockI_ t (List i) (List i) (List a)
        inner ref =
            let
                defaultList :
                    Lookup t
                    -> List i
                    -> Int
                    -> (Internal.BlockI_ t i i a -> ( Int, i ) -> x)
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

                listMap : Lookup t -> List i -> List a
                listMap lookup l =
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
            , map = listMap
            }
    in
    State.map inner Ref.take |> Block


oneOf : ( a, String ) -> List ( a, String ) -> String -> Block t a
oneOf first rest label =
    let
        inner : Ref -> Internal.BlockI_ t a a a
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


bool : String -> Block t Bool
bool =
    oneOf ( True, "True" ) [ ( False, "False" ) ]
