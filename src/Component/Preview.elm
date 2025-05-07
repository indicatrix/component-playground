module Component.Preview exposing
    ( Library(..)
    , Library_
    , Msg(..)
    , Preview(..)
    , Preview_
    , library_
    , map
    , preview
    , subcomponent
    , withControl
    , withMsg
    , withState
    , withSubcomponent
    , withUnlabelled
    )

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
    = Preview (Preview_ t msg a)


type alias Preview_ t msg a =
    { meta : Meta
    , value : Library t msg -> Lookup t -> State Ref a
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
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
        withRef (Preview p) =
            Ref.take |> State.map (\ref -> ( p.meta.id, ( ref, p ) ))

        lib =
            Dict.fromList <| Ref.fromTop <| State.traverse withRef previews
    in
    { index = List.map (\(Preview p) -> p.meta) previews
    , lookup = \s -> Dict.get s lib |> Maybe.map (Tuple.second >> Preview)
    , lookup_ = \s -> Dict.get s lib
    }


type alias Library_ t msg =
    { index : List { name : String, id : String }
    , lookup : String -> Maybe (Preview t msg (Html msg))
    , lookup_ : String -> Maybe ( Ref, Preview_ t msg (Html msg) )
    }


{-| Create a preview with the name, id and definition.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    Preview <|
        { meta = { id = id, name = meta.name }
        , value = \_ _ -> State.state value
        , controls = \_ -> State.state []
        }


withState :
    String
    -> (String -> Block t a a)
    -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c)
    -> Preview t (Msg t msg) c
withState label blockF =
    withHelper (blockF label) <|
        \_ lookup f b ->
            f (b.fromType lookup |> Maybe.withDefault b.default)
                (b.toType >> SetState)


withControl : String -> (String -> Block t x a) -> Preview t msg (a -> b) -> Preview t msg b
withControl label blockF =
    withHelper (blockF label) <|
        \_ lookup f b -> f (b.fromType lookup |> Maybe.withDefault b.default)


withUnlabelled : Block t x a -> Preview t msg (a -> b) -> Preview t msg b
withUnlabelled block =
    withHelper block <|
        \_ lookup f b -> f (b.fromType lookup |> Maybe.withDefault b.default)


withHelper :
    Block t x a
    -> (Library t msg -> Lookup t -> b -> Block_ t x a -> c)
    -> Preview t msg b
    -> Preview t msg c
withHelper (Block bState) body (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \lib lookup ->
                State.map2 (body lib lookup) (p.value lib lookup) bState
        , controls =
            \lib ->
                State.map2 (\c b -> c ++ b.controls) (p.controls lib) bState
        }


withSubcomponent : String -> (Library t msg -> String -> Block t x b) -> Preview t msg (b -> a) -> Preview t msg a
withSubcomponent label componentBlock (Preview p) =
    Preview <|
        { meta = p.meta
        , value =
            \lib lookup ->
                State.map2
                    (\f b ->
                        f (b.fromType lookup |> Maybe.withDefault b.default)
                    )
                    (p.value lib lookup)
                    (Block.unwrap <| componentBlock lib label)
        , controls =
            \lib ->
                State.map2
                    (\c b -> c ++ b.controls)
                    (p.controls lib)
                    (Block.unwrap <| componentBlock lib label)
        }


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
withMsg (Preview p) =
    Preview <|
        { meta = p.meta
        , value = \pl l -> State.map (\f -> f Msg) (p.value pl l)
        , controls = p.controls
        }


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
                            (\(Preview p) -> Ref.from ref (p.value lib lookup))

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
                            (\(Preview p) ->
                                let
                                    controls =
                                        Ref.from ref (p.controls lib)
                                in
                                controlUI p.meta.id <|
                                    List.map (\c -> c lookup) controls
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
map f (Preview p) =
    Preview <|
        { meta = p.meta
        , value = \lib l -> State.map f (p.value lib l)
        , controls = p.controls
        }
