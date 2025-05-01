module Component.Internal exposing (..)

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
    , controls : List (PreviewLookup t msg -> Lookup t -> Html (List ( Ref, Type t )))
    , state : List (Lookup t -> Html ())
    }


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
    -> Block t a a
    -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c)
    -> Preview t (Msg t msg) c
withState label (Block bState) (Preview pState) =
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
        |> State.andThen (\p -> Ref.nested (bState label) |> State.map (inner p))
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


withControl : String -> Block t x a -> Preview t msg (a -> b) -> Preview t msg b
withControl label (Block bState) (Preview pState) =
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
        |> State.andThen (\p -> Ref.nested (bState label) |> State.map (inner p))
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


type alias Lookup t =
    Ref -> Maybe (Type t)


type alias PreviewLookup t msg =
    String -> Preview t msg (Html msg)


type Block t x a
    = Block (String -> State Ref (Block_ t x a))


type alias Block_ t x a =
    { fromType : Lookup t -> Maybe a
    , toType : x -> List ( Ref, Type t )
    , control : List (Lookup t -> Html (List ( Ref, Type t )))
    , display : List (Lookup t -> Html ())
    , default : a
    }


pure : a -> Block t x a
pure a =
    Block <|
        \_ ->
            State.state
                { fromType = \_ -> Just a
                , toType = \_ -> []
                , control = []
                , display = []
                , default = a
                }


andMap : String -> Block t x1 a -> Block t x (a -> b) -> Block t x b
andMap label (Block state1) (Block stateF) =
    let
        func : Block_ t x (a -> b) -> Block_ t x1 a -> Block_ t x b
        func bF b1 =
            let
                fromType : Lookup t -> Maybe b
                fromType lookup =
                    bF.fromType lookup |> Maybe.andMap (b1.fromType lookup)

                toType : x -> List ( Ref, Type t )
                toType x =
                    bF.toType x

                control : List (Lookup t -> Html (List ( Ref, Type t )))
                control =
                    Debug.todo ""

                display : List (Lookup t -> Html ())
                display =
                    Debug.todo ""

                default : b
                default =
                    bF.default b1.default
            in
            { fromType = fromType
            , toType = toType
            , control = control
            , display = display
            , default = default
            }
    in
    Block <|
        \nextLabel ->
            stateF nextLabel
                |> State.andThen (\bF -> state1 label |> State.map (func bF))


string : Block t String String
string =
    let
        inner label ref =
            let
                toType s =
                    [ ( ref, Type.StringValue s ) ]

                fromType lookup =
                    lookup ref |> Maybe.andThen Type.stringValue

                default =
                    "Value"

                control lookup =
                    UI.textField
                        { msg = toType
                        , id = Ref.toString ref
                        , label = label
                        , value = fromType lookup |> Maybe.withDefault default
                        }

                display lookup =
                    UI.text []
                        [ Html.text <|
                            label
                                ++ ": "
                                ++ (fromType lookup |> Maybe.withDefault default)
                        ]
            in
            { fromType = fromType
            , toType = toType
            , control = [ control ]
            , display = [ display ]
            , default = default
            }
    in
    Block <| \label -> State.map (inner label) Ref.take


identifier : Block t x String
identifier =
    Block <|
        \_ ->
            Ref.take
                |> State.map
                    (\ref ->
                        { fromType = \_ -> Nothing
                        , toType = \_ -> []
                        , control = []
                        , display = []
                        , default = Ref.toString ref
                        }
                    )


list : Block t a a -> Block t (List a) (List a)
list (Block block) =
    let
        inner : String -> Ref -> Block_ t (List a) (List a)
        inner listLabel ref =
            let
                fromType : Lookup t -> Maybe (List a)
                fromType lookup =
                    lookup ref
                        |> Maybe.andThen Type.intValue
                        |> Maybe.map
                            (\len ->
                                State.traverse
                                    (\i ->
                                        State.map
                                            (\b ->
                                                b.fromType lookup
                                                    |> Maybe.withDefault b.default
                                            )
                                            (block (String.fromInt i))
                                    )
                                    (List.range 0 (len - 1))
                                    |> Ref.from ref
                            )

                toType : List a -> List ( Ref, Type t )
                toType values =
                    ( ref, Type.IntValue <| List.length values )
                        :: List.concat
                            (Ref.from ref
                                (State.traverse
                                    (\( i, value ) ->
                                        State.map (\b -> b.toType value)
                                            (block (String.fromInt i))
                                    )
                                    (List.indexedMap Tuple.pair values)
                                )
                            )

                default =
                    State.traverse
                        (\i -> State.map .default (block (String.fromInt i)))
                        (List.range 0 2)
                        |> Ref.from ref

                control : Lookup t -> Html (List ( Ref, Type t ))
                control lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)
                    in
                    UI.vStack [ UI.style "gap" "8px" ]
                        [ UI.text [] [ Html.text listLabel ]
                        , UI.vStack [ UI.style "gap" "8px", UI.style "padding-left" "16px" ]
                            (UI.hStack [ UI.style "gap" "8px" ]
                                [ UI.button [ UI.onClick [ ( ref, Type.IntValue (len + 1) ) ] ] [ Html.text "Add Item" ]
                                , UI.button [ UI.onClick [ ( ref, Type.IntValue (len - 1) ) ] ] [ Html.text "Remove Item" ]
                                ]
                                :: List.concat
                                    (State.traverse
                                        (\i ->
                                            State.map
                                                (\b ->
                                                    List.map (\f -> Html.map ((::) ( ref, Type.IntValue len )) <| f lookup) b.control
                                                )
                                                (block (String.fromInt i))
                                        )
                                        (List.range 0 (len - 1))
                                        |> Ref.from ref
                                    )
                            )
                        ]

                display : Lookup t -> Html ()
                display lookup =
                    let
                        len =
                            lookup ref
                                |> Maybe.andThen Type.intValue
                                |> Maybe.withDefault (List.length default)
                    in
                    UI.vStack []
                        (UI.hStack [] [ UI.text [] [ Html.text listLabel ], UI.text [] [ Html.text "List:" ] ]
                            :: List.concat
                                (State.traverse
                                    (\i ->
                                        State.map
                                            (\b -> List.map (\f -> f lookup) b.display)
                                            (block (String.fromInt i))
                                    )
                                    (List.range 0 (len - 1))
                                    |> Ref.from ref
                                )
                        )
            in
            { fromType = fromType
            , toType = toType
            , control = [ control ]
            , display = [ display ]
            , default = default
            }
    in
    Block <| \label -> State.map (inner label) Ref.take
