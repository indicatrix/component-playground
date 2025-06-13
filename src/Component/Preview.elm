module Component.Preview exposing
    ( Library(..)
    , Library_
    , Msg(..)
    , Preview(..)
    , PreviewRef(..)
    , Preview_
    , fromPreview
    , library_
    , map
    , preview
    , previewBlock
    , withControl
    , withMsg
    , withMsg2
    , withMsg3
    , withPreview
    , withState
    , withStateF
    , withUnlabelled, View
    )

import Component.Block as Block exposing (Block, BlockI(..), BlockI_)
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import Maybe.Extra as Maybe
import State exposing (State)


type Msg t msg
    = SetState (List ( Ref, Type t ))
    | Msg (List ( Ref, Type t )) msg


type Preview t msg a
    = Preview (Preview_ t msg a)


type alias Preview_ t msg a =
    { meta : Meta
    , value : Library t msg -> Lookup t -> State Ref a
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
    , reference : State Ref Ref
    }


type alias Meta =
    { id : String, name : String }


type alias Lookup t =
    Block.Lookup t

type alias View msg = (Html msg, Dict String (Html msg))

type Library t msg
    = Library
        -- Current componet id (used in previewBlock)
        String
        (Library_ t msg)

type alias Library_ t msg =
    { index : List { name : String, id : String }
    , lookup : String -> Maybe (Preview t msg (View msg))
    , lookup_ : String -> Maybe ( Ref, Preview_ t msg (View msg) )
    }


library_ : List (Preview t msg (View msg)) -> Library_ t msg
library_ previews =
    let
        withRef (Preview p) =
            Ref.take |> State.map (\ref -> ( p.meta.id, ( ref, p ) ))

        lib =
            Dict.fromList <| Ref.fromTop <| State.traverse withRef previews
    in
    { index = List.map (\(Preview p) -> p.meta) previews
    , lookup = \s -> Dict.get s lib |> Maybe.map (Tuple.second >> Preview)
    , lookup_ = \s -> Dict.get s lib
    }



{-| Create a preview with the name, id and definition.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    Preview <|
        { meta = { id = id, name = meta.name }
        , value = \_ _ -> State.state value
        , controls = \_ -> State.state []

        -- This is set here ONLY and passed through all with* calls to ensure
        -- this is a stable reference regardless of how many with* calls have
        -- been made.
        , reference = State.get
        }


withState :
    BlockI t i a
    -> (a -> (i -> Msg t msg) -> x -> y)
    -> Preview t (Msg t msg) x
    -> Preview t (Msg t msg) y
withState block f =
    withHelper block <|
        \_ lookup _ x b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)
                (b.toType >> SetState)
                x


withStateF :
    BlockI t i a
    -> (Ref -> a -> (i -> msg -> Msg t msg) -> x -> y)
    -> Preview t (Msg t msg) x
    -> Preview t (Msg t msg) y
withStateF block f =
    withHelper block <|
        \_ lookup ref x b ->
            f ref
                (b.fromType b.default b.default lookup |> b.map lookup)
                (\i msg -> Msg (b.toType i) msg)
                x


withControl : String -> (String -> BlockI t i a) -> Preview t msg (a -> b) -> Preview t msg b
withControl label blockF =
    withUnlabelled (blockF label)


withUnlabelled : BlockI t i a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled block =
    withHelper block <|
        \_ lookup _ f b ->
            f (b.fromType b.default b.default lookup |> b.map lookup)


withHelper :
    BlockI t i a
    -> (Library t msg -> Lookup t -> Ref -> b -> BlockI_ t i i a -> c)
    -> Preview t msg b
    -> Preview t msg c
withHelper (Block bState) body (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \lib lookup ->
                State.map3 (body lib lookup) p.reference (p.value lib lookup) bState
        , controls =
            \lib ->
                State.map2 (\c b -> c ++ b.controls b.default) (p.controls lib) bState
        , reference = p.reference
        }


withPreview : String -> (Library t msg -> String -> BlockI t i b) -> Preview t msg (b -> a) -> Preview t msg a
withPreview label componentBlock (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \lib lookup ->
                State.map2
                    (\f b ->
                        f (b.fromType b.default b.default lookup |> b.map lookup)
                    )
                    (p.value lib lookup)
                    (Block.unwrap <| componentBlock lib label)
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ b.controls b.default)
                    (p.controls lib)
                    (Block.unwrap <| componentBlock lib label)
        , reference = p.reference
        }


withMsg :
    (a -> msg)
    -> Preview t (Msg t msg) ((a -> Msg t msg) -> r)
    -> Preview t (Msg t msg) r
withMsg msg (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \pl l ->
                State.map
                    (\f -> f (\a -> Msg [] (msg a)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg2 :
    (a -> b -> msg)
    -> Preview t (Msg t msg) ((a -> b -> Msg t msg) -> r)
    -> Preview t (Msg t msg) r
withMsg2 msg (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \pl l ->
                State.map
                    (\f -> f (\a b -> Msg [] (msg a b)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


withMsg3 :
    (a -> b -> c -> msg)
    -> Preview t (Msg t msg) ((a -> b -> c -> Msg t msg) -> r)
    -> Preview t (Msg t msg) r
withMsg3 msg (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \pl l ->
                State.map
                    (\f -> f (\a b c -> Msg [] (msg a b c)))
                    (p.value pl l)
        , controls = p.controls
        , reference = p.reference
        }


type PreviewRef
    = PreviewRef String


fromPreview : Preview t msg a -> PreviewRef
fromPreview (Preview p) =
    PreviewRef p.meta.id


previewBlock : Library t msg -> String -> BlockI t PreviewRef (Html msg)
previewBlock ((Library currentComponentId lib_) as lib) label =
    let
        inner : Ref -> BlockI_ t PreviewRef PreviewRef (Html msg)
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

                control : PreviewRef -> Lookup t -> Html (List ( Ref, Type t ))
                control (PreviewRef default) lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.withDefault default
                        |> (\id ->
                                lib_.lookup id
                                    |> Maybe.map
                                        (\(Preview p) ->
                                            let
                                                controls =
                                                    Ref.from ref (p.controls lib)
                                            in
                                            controlUI p.meta.id <|
                                                List.map (\c -> c lookup) controls
                                        )
                                    |> Maybe.withDefault (controlUI id [])
                           )

                mapF : Lookup t -> PreviewRef -> Html msg
                mapF lookup (PreviewRef id) =
                    lib_.lookup id
                        |> Maybe.map (\(Preview p) -> Tuple.first <| Ref.from ref (p.value lib lookup))
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
                        |> Maybe.map PreviewRef
                        |> Maybe.withDefault default
            , toType = \(PreviewRef s) -> [ ( ref, Type.StringValue s ) ]
            , controls = \default -> [ control default ]
            , default =
                List.head lib_.index
                    |> Maybe.map (.id >> PreviewRef)
                    |> Maybe.withDefault (PreviewRef "not-found")
            , map = mapF
            }
    in
    Ref.withNestedRef inner |> Block


map : (a -> b) -> Preview t msg a -> Preview t msg b
map f (Preview p) =
    Preview <|
        { meta = p.meta
        , value = \lib l -> State.map f (p.value lib l)
        , controls = p.controls
        , reference = p.reference
        }
