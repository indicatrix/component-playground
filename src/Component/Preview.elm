module Component.Preview exposing (..)

import Component.Block as Block exposing (Block(..), Block_)
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Html exposing (Html)
import Maybe.Extra as Maybe
import State exposing (State)


type Msg t msg
    = SetState (List ( Ref, Type t ))
    | Msg msg


type Preview t msg a
    = Preview (State Ref (Preview_ t msg a))


type alias Preview_ t msg a =
    { meta : Meta
    , value : PreviewLookup t msg -> Lookup t -> a
    , controls : PreviewLookup t msg -> List (Lookup t -> Html (List ( Ref, Type t )))
    , state : List (Lookup t -> Html ())
    }


type alias Meta =
    { id : String, name : String }


type alias Lookup t =
    Block.Lookup t


type alias PreviewLookup t msg =
    String -> Maybe (Preview t msg (Html msg))


{-| Create a preview with the name, id and definition.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    fromPreview_
        { meta = { id = id, name = meta.name }
        , value = \_ _ -> value
        , controls = \_ -> []
        , state = []
        }


withState :
    String
    -> (String -> Block t a a)
    -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c)
    -> Preview t (Msg t msg) c
withState label blockF (Preview pState) =
    let
        inner :
            Preview_ t (Msg t msg) (a -> (a -> Msg t msg) -> c)
            -> Block_ t a a
            -> Preview_ t (Msg t msg) c
        inner p b =
            { meta = p.meta
            , value =
                \pl lookup ->
                    p.value pl
                        lookup
                        (b.fromType lookup |> Maybe.withDefault b.default)
                        (b.toType >> SetState)
            , controls = p.controls
            , state = p.state ++ b.displays
            }
    in
    pState
        |> State.andThen (\p -> Ref.nested (Block.unwrap (blockF label)) |> State.map (inner p))
        |> Preview


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
withMsg (Preview pState) =
    let
        inner : Preview_ t (Msg t m) ((m -> Msg t m) -> a) -> Preview_ t (Msg t m) a
        inner p =
            { meta = p.meta
            , value = \pl lookup -> p.value pl lookup Msg
            , controls = p.controls
            , state = p.state
            }
    in
    pState |> State.map inner |> Preview


withControl : String -> (String -> Block t x a) -> Preview t msg (a -> b) -> Preview t msg b
withControl label blockF (Preview pState) =
    let
        inner : Preview_ t msg (a -> b) -> Block_ t x a -> Preview_ t msg b
        inner p b =
            { meta = p.meta
            , value =
                \pl lookup ->
                    p.value pl
                        lookup
                        (b.fromType lookup |> Maybe.withDefault b.default)
            , controls = \pl -> p.controls pl ++ b.controls
            , state = p.state
            }
    in
    pState
        |> State.andThen (\p -> Ref.nested (Block.unwrap (blockF label)) |> State.map (inner p))
        |> Preview


withAnonymous : Block t x a -> Preview t msg (a -> b) -> Preview t msg b
withAnonymous (Block block) (Preview pState) =
    let
        inner : Preview_ t msg (a -> b) -> Block_ t x a -> Preview_ t msg b
        inner p b =
            { meta = p.meta
            , value =
                \pl lookup ->
                    p.value pl
                        lookup
                        (b.fromType lookup |> Maybe.withDefault b.default)
            , controls = \pl -> p.controls pl ++ b.controls
            , state = p.state
            }
    in
    pState
        |> State.andThen (\p -> Ref.nested block |> State.map (inner p))
        |> Preview


withSubcomponent : String -> (PreviewLookup t msg -> String -> Block t x (Html msg)) -> Preview t msg (Html msg -> a) -> Preview t msg a
withSubcomponent label componentBlock (Preview pState) =
    let
        inner : Preview_ t msg (Html msg -> a) -> Ref -> Preview_ t msg a
        inner p ref =
            { meta = p.meta
            , value =
                \pl lookup ->
                    let
                        b =
                            State.finalValue ref (Block.unwrap (componentBlock pl label))
                    in
                    p.value pl
                        lookup
                        (b.fromType lookup |> Maybe.withDefault b.default)
            , controls =
                \pl ->
                    let
                        b =
                            State.finalValue ref (Block.unwrap (componentBlock pl label))
                    in
                    p.controls pl ++ b.controls
            , state = p.state
            }
    in
    pState |> State.andThen (\p -> Ref.take |> State.map (inner p)) |> Preview


subcomponent : PreviewLookup t msg -> String -> Block t x (Html msg)
subcomponent previewLookup label =
    let
        inner : Ref -> Block_ t x (Html msg)
        inner ref =
            let
                fromType : Lookup t -> Maybe (Html msg)
                fromType lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.andThen previewLookup
                        |> Maybe.map
                            (\(Preview pState) ->
                                let
                                    p =
                                        State.finalValue ref pState
                                in
                                p.value previewLookup lookup
                            )

                default : Html msg
                default =
                    Html.text "Component not found"

                controlUI : String -> List (Html (List ( Ref, Type t ))) -> Html (List ( Ref, Type t ))
                controlUI previewId componentControls =
                    UI.vStack [ UI.style "gap" "8px" ]
                        [ UI.text [] [ Html.text label ]
                        , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                            (UI.select
                                { id = Ref.toString ref
                                , label = "Component"
                                , options = [ "something" ]
                                , defaultText = "Select component"
                                , value = Just ""
                                , msg =
                                    \selected ->
                                        [ ( ref, selected |> Maybe.withDefault previewId |> Type.StringValue ) ]
                                }
                                :: List.map
                                    (Html.map ((::) ( ref, Type.StringValue previewId )))
                                    componentControls
                            )
                        ]

                control : Lookup t -> Html (List ( Ref, Type t ))
                control lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.andThen previewLookup
                        |> Maybe.map
                            (\(Preview pState) ->
                                let
                                    p =
                                        State.finalValue ref pState
                                in
                                controlUI p.meta.id <|
                                    List.map (\c -> c lookup) (p.controls previewLookup)
                            )
                        |> Maybe.withDefault (Html.text "Some default")
            in
            -- Need a string selector to get the identifier to look up
            -- Controls is that PLUS nested controls for the preview. (See list for examples)
            { fromType = fromType
            , toType = \_ -> []
            , controls = [ control ]
            , displays = []
            , default = default
            }
    in
    Ref.take |> State.map inner |> Block


map : (a -> b) -> Preview t msg a -> Preview t msg b
map f (Preview pState) =
    State.map
        (\p ->
            { meta = p.meta
            , value = \scl lookup -> f (p.value scl lookup)
            , controls = p.controls
            , state = p.state
            }
        )
        pState
        |> Preview


unwrap : Preview t msg a -> State Ref (Preview_ t msg a)
unwrap (Preview p) =
    p


fromPreview_ : Preview_ t msg a -> Preview t msg a
fromPreview_ =
    State.state >> Preview
