module Component.Component exposing
    ( Component(..)
    , ComponentRef(..)
    , Component_
    , Library(..)
    , Library_
    , Lookup
    , Meta
    , Msg(..)
    , Preview
    , View
    , library_
    , map
    , new
    , previewBlock
    , withComponent
    , withControl
    , withMsg
    , withMsg2
    , withMsg3
    , withState
    , withStateF
    , withUnlabelled
    , withUpdateF
    )

import Component.Block as Block exposing (BlockI(..), BlockI_)
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import State exposing (State)


type Msg t msg
    = SetState (List ( Ref, Type t ))
    | Msg (List ( Ref, Type t )) msg
    | Update (Lookup t -> ( List ( Ref, Type t ), msg ))


type Component t msg a
    = Component (Component_ t msg a)


type alias Component_ t msg a =
    { value : Library t msg -> Lookup t -> State Ref a
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
    , reference : State Ref Ref
    }


type alias Meta =
    { id : String, name : String }


type alias Lookup t =
    Block.Lookup t


type alias Preview t msg =
    ( Meta, Component t msg (View msg) )


type alias View msg =
    ( Html msg, Dict String (Html msg) )


type Library t msg
    = Library
        -- Current componet id (used in previewBlock)
        String
        (Library_ t msg)


type alias Library_ t msg =
    { index : List { name : String, id : String }
    , lookup : String -> Maybe ( String, Component t msg (View msg) )
    , lookup_ : String -> Maybe ( String, Ref, Component_ t msg (View msg) )
    }


library_ : List (Preview t msg) -> Library_ t msg
library_ previews =
    let
        withRef ( meta, Component p ) =
            Ref.take |> State.map (\ref -> ( meta.id, ( ref, p ) ))

        lib =
            Dict.fromList <| Ref.fromTop <| State.traverse withRef previews
    in
    { index = List.map Tuple.first previews
    , lookup = \s -> Dict.get s lib |> Maybe.map (\( _, p ) -> ( s, Component p ))
    , lookup_ = \s -> Dict.get s lib |> Maybe.map (\( r, p ) -> ( s, r, p ))
    }


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


withState :
    BlockI t i a
    -> (a -> (i -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withState block f =
    withHelper block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (b.toType >> SetState)
                x


withStateF :
    BlockI t i a
    -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y)
    -> Component t (Msg t msg) x
    -> Component t (Msg t msg) y
withStateF block f =
    withHelper block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\i msg -> Msg (b.toType i) msg)
                x


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


withControl : String -> (String -> BlockI t i a) -> Component t msg (a -> b) -> Component t msg b
withControl label blockF =
    withUnlabelled (blockF label)


withUnlabelled : BlockI t i a -> Component t msg (a -> b) -> Component t msg b
withUnlabelled block =
    withHelper block <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withHelper :
    BlockI t i a
    -> (Library t msg -> Lookup t -> Ref -> b -> BlockI_ t i i a -> c)
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


withComponent : String -> (Library t msg -> String -> BlockI t i b) -> Component t msg (b -> a) -> Component t msg a
withComponent label blockF (Component p) =
    Component <|
        { value =
            \lib lookup ->
                State.map2
                    (\f b ->
                        f (b.fromType b.default b.default lookup |> b.map lookup)
                    )
                    (p.value lib lookup)
                    (Block.unwrap <| blockF lib label)
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ b.controls b.default)
                    (p.controls lib)
                    (Block.unwrap <| blockF lib label)
        , reference = p.reference
        }


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


type ComponentRef
    = ComponentRef String


previewBlock : Library t msg -> String -> BlockI t ComponentRef (Html msg)
previewBlock ((Library currentComponentId lib_) as lib) label =
    let
        inner : Ref -> BlockI_ t ComponentRef ComponentRef (Html msg)
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


map : (a -> b) -> Component t msg a -> Component t msg b
map f (Component p) =
    Component <|
        { value = \lib l -> State.map f (p.value lib l)
        , controls = p.controls
        , reference = p.reference
        }
