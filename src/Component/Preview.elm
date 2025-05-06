module Component.Preview exposing (..)

import Component.Block as Block exposing (Block(..), Block_)
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.UI as UI
import Dict
import Html exposing (Html)
import Maybe.Extra as Maybe
import State exposing (State)


type Msg t msg
    = SetState (List ( Ref, Type t ))
    | Msg msg


type Preview t msg a
    = Preview (State Ref (Preview_ t msg a))



{- Sub-preview bug is that previews themselves don't have state inside.

   To look at a state, we need to make value and controls stateful (so it can
   be re-entrant).

   Currently only Block is that (as it's built!)

   Preview needs to have inside only (no state around it)
-}


type Preview2 t msg a
    = Preview2 (Preview2_ t msg a)


type alias Preview2_ t msg a =
    { meta : Meta
    , value : Library t msg -> Lookup t -> State Ref a

    -- not sure if State needs to be here, but it's the pattern.
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
    }


type alias Preview_ t msg a =
    { meta : Meta
    , value : Library t msg -> Lookup t -> a
    , controls : Library t msg -> List (Lookup t -> Html (List ( Ref, Type t )))
    }


type alias Meta =
    { id : String, name : String }


type alias Lookup t =
    Block.Lookup t


type Library t msg
    = Library String (Library_ t msg)


library_ : List (Preview t msg (Html msg)) -> Library_ t msg
library_ previews =
    let
        unwrap (Preview pState) =
            Ref.nested pState

        previews_ =
            Ref.fromTop (State.traverse unwrap previews)

        lib =
            Dict.fromList <| List.map (\p -> ( p.meta.id, p )) previews_
    in
    { index = List.map (\p -> p.meta) previews_
    , lookup = \s -> Dict.get s lib |> Maybe.map (State.state >> Preview)
    , lookup_ = \s -> Dict.get s lib
    }


type alias Library_ t msg =
    { index : List { name : String, id : String }
    , lookup : String -> Maybe (Preview t msg (Html msg))
    , lookup_ : String -> Maybe (Preview_ t msg (Html msg))
    }


{-| Create a preview with the name, id and definition.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    Preview <|
        State.state
            { meta = { id = id, name = meta.name }
            , value = \_ _ -> value
            , controls = \_ -> []
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
            , controls = \pl -> p.controls pl ++ b.controls
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
            , value = \pl l -> p.value pl l Msg
            , controls = p.controls
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
                \pl l ->
                    p.value pl l (b.fromType l |> Maybe.withDefault b.default)
            , controls = \pl -> p.controls pl ++ b.controls
            }
    in
    pState
        |> State.andThen (\p -> State.map (inner p) (Block.unwrap (blockF label)))
        |> Preview


withAnonymous : Block t x a -> Preview t msg (a -> b) -> Preview t msg b
withAnonymous block (Preview pState) =
    let
        inner : Preview_ t msg (a -> b) -> Block_ t x a -> Preview_ t msg b
        inner p b =
            { meta = p.meta
            , value =
                \pl l ->
                    p.value pl l (b.fromType l |> Maybe.withDefault b.default)
            , controls = \pl -> p.controls pl ++ b.controls
            }
    in
    pState
        |> State.andThen (\p -> State.map (inner p) (Block.unwrap block))
        |> Preview


withSubcomponent : String -> (Library t msg -> String -> Block t x b) -> Preview t msg (b -> a) -> Preview t msg a
withSubcomponent label componentBlock (Preview pState) =
    let
        inner : Preview_ t msg (b -> a) -> Ref -> Preview_ t msg a
        inner p ref =
            { meta = p.meta
            , value =
                \pl l ->
                    let
                        b =
                            Ref.from ref (Block.unwrap (componentBlock pl label))
                    in
                    p.value pl l (b.fromType l |> Maybe.withDefault b.default)
            , controls =
                \pl ->
                    let
                        b =
                            Ref.from ref (Block.unwrap (componentBlock pl label))
                    in
                    p.controls pl ++ b.controls
            }
    in
    pState |> State.andThen (\p -> State.map (inner p) Ref.take) |> Preview


subcomponent : Library t msg -> String -> Block t x (Html msg)
subcomponent ((Library currentComponentId lib_) as lib) label =
    let
        inner : Ref -> Block_ t x (Html msg)
        inner ref =
            let
                currentPreview : Lookup t -> Maybe (Preview t msg (Html msg))
                currentPreview lookup =
                    lookup ref
                        |> Maybe.andThen Type.stringValue
                        |> Maybe.andThen lib_.lookup
                        |> Maybe.orElseLazy
                            (\() ->
                                List.head lib_.index
                                    |> Maybe.andThen (.id >> lib_.lookup)
                            )

                fromType : Lookup t -> Maybe (Html msg)
                fromType lookup =
                    currentPreview lookup
                        |> Maybe.map
                            (\(Preview pState) ->
                                (Ref.from ref pState).value lib lookup
                            )

                default : Html msg1
                default =
                    Html.div []
                        [ Html.text "Component not found"
                        ]

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

                control : Lookup t -> Html (List ( Ref, Type t ))
                control lookup =
                    currentPreview lookup
                        |> Maybe.map
                            (\(Preview pState) ->
                                let
                                    p =
                                        Ref.from ref pState
                                in
                                controlUI p.meta.id <|
                                    List.map (\c -> c lookup) (p.controls lib)
                            )
                        |> Maybe.withDefault default
            in
            -- Need a string selector to get the identifier to look up
            -- Controls is that PLUS nested controls for the preview. (See list for examples)
            { fromType = fromType
            , toType = \_ -> []
            , controls = [ control ]
            , default = default
            }
    in
    Ref.withNestedRef inner |> Block


map : (a -> b) -> Preview t msg a -> Preview t msg b
map f (Preview pState) =
    State.map
        (\p ->
            { meta = p.meta
            , value = \lib l -> f (p.value lib l)
            , controls = p.controls
            }
        )
        pState
        |> Preview
