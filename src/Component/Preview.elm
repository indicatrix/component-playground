module Component.Preview exposing (..)

import Component.Block as Block exposing (Block(..), Block_)
import Component.Ref as Ref exposing (Ref)
import Component.Type exposing (Type)
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
    , controls : List (PreviewLookup t msg -> Lookup t -> Html (List ( Ref, Type t )))
    , state : List (Lookup t -> Html ())
    }


type alias Lookup t =
    Block.Lookup t


type alias PreviewLookup t msg =
    String -> Preview t msg (Html msg)


{-| Create a preview with the name, id and definition.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    fromPreview_
        { meta = { id = id, name = meta.name }
        , value = \_ _ -> value
        , controls = []
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
            , state = p.state ++ b.display
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
            , controls = p.controls ++ List.map always b.control
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
            , controls = p.controls ++ List.map always b.control
            , state = p.state
            }
    in
    pState
        |> State.andThen (\p -> Ref.nested block |> State.map (inner p))
        |> Preview


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


type alias Meta =
    { id : String, name : String }



-- START OF BLOCK FILE
