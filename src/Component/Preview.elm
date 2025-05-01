module Component.Preview exposing
    ( Block(..)
    , Identifier
    , Lookup
    , Meta
    , Msg(..)
    , Preview(..)
    , controls
    , identifier
    , map
    , name
    , preview
    , state
    , view
    , withControl
    , withMsg
    , withState
    )

import Component.Type exposing (Type)
import Html exposing (Html)


type Msg t msg
    = SetState (List ( String, Type t ))
    | Msg msg


type Preview t a
    = Preview
        { meta : Meta

        -- Prefix is separate from meta.id since sometimes we'll recurse into
        -- previews with a different prefix.
        , prefix : String
        , pointer : Int
        , value : Lookup t -> a
        , controls : List (Lookup t -> Html (List ( String, Type t )))
        , state : List (Lookup t -> Html ())
        }


type alias Meta =
    { id : String, name : String }


{-| Create a preview with the name, id and definition. Similar to
'succeed'-style functions.
-}
preview : String -> { name : String } -> a -> Preview t a
preview id meta value =
    Preview
        { meta = { id = id, name = meta.name }
        , pointer = 0
        , prefix = id ++ "."
        , value = \_ -> value
        , controls = []
        , state = []
        }


withState : String -> Block t a -> Preview t (a -> (a -> Msg t msg) -> c) -> Preview t c
withState label (Block b) ((Preview p) as preview_) =
    let
        id =
            idFunc preview_
    in
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer + 1
        , value =
            \lookup ->
                p.value lookup
                    (b.fromType (id >> lookup) |> Maybe.withDefault b.default)
                    (b.toType >> List.map (Tuple.mapFirst id) >> SetState)
        , controls = p.controls
        , state = p.state ++ [ \lookup -> b.display label (id >> lookup) ]
        }


idFunc : Preview t x -> String -> String
idFunc (Preview p) s =
    let
        prefix =
            p.prefix ++ String.fromInt p.pointer ++ "."
    in
    prefix ++ s


withMsg : Preview t ((msg -> Msg t msg) -> a) -> Preview t a
withMsg ((Preview p) as preview_) =
    let
        id =
            idFunc preview_
    in
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer + 1
        , value =
            \lookup ->
                p.value (id >> lookup)
                    Msg
        , controls = p.controls
        , state = p.state
        }


withControl : String -> Block t a -> Preview t (a -> b) -> Preview t b
withControl label (Block b) ((Preview p) as preview_) =
    let
        id =
            idFunc preview_
    in
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer + 1
        , value =
            \lookup ->
                p.value lookup
                    (b.fromType (id >> lookup) |> Maybe.withDefault b.default)
        , controls =
            p.controls
                ++ [ \lookup ->
                        b.control id label (id >> lookup)
                            |> Html.map (List.map (Tuple.mapFirst id))
                   ]
        , state = p.state
        }


map : (a -> b) -> Preview t a -> Preview t b
map f (Preview p) =
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer
        , value = \lookup -> f (p.value lookup)
        , controls = p.controls
        , state = p.state
        }


{-| A bag of functions to support controls with type `a`, interfacing with `Type
t`.
-}
type Block t a
    = Block
        { fromType : Lookup t -> Maybe a
        , toType : a -> List ( String, Type t )
        , control : Identifier -> String -> Lookup t -> Html (List ( String, Type t ))
        , display : String -> Lookup t -> Html ()
        , default : a
        }


type alias Lookup t =
    String -> Maybe (Type t)


type alias Identifier =
    String -> String


identifier : Preview t a -> String
identifier (Preview p) =
    p.meta.id


name : Preview t a -> String
name (Preview p) =
    p.meta.name


{-| I have a hunch that some function needs to be parsed in here to support
embedding components.
-}
view : Lookup t -> Preview t (Html msg) -> Html msg
view lookup (Preview p) =
    p.value lookup


controls :
    Lookup t
    -> (List ( String, Type t ) -> msg)
    -> Preview t (Html msg)
    -> List (Html msg)
controls lookup msg (Preview b) =
    let
        apply :
            (Lookup t -> Html (List ( String, Type t )))
            -> Html msg
        apply f =
            f lookup |> Html.map msg
    in
    List.map apply b.controls


state :
    Lookup t
    -> (List ( String, Type t ) -> msg)
    -> Preview t (Html msg)
    -> List (Html msg)
state lookup msg (Preview p) =
    let
        apply : (Lookup t -> Html ()) -> Html msg
        apply f =
            f lookup |> Html.map (\_ -> msg [])
    in
    List.map apply p.state
