module Component.Preview exposing
    ( Block(..)
    , Identifier
    , Lookup
    , Meta
    , Msg(..)
    , Preview(..)
      -- , ReaderWriter
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

import Component.Type as Type exposing (Type)
import Html exposing (Html)


type Msg t msg
    = SetState (List ( String, Type t ))
    | Msg msg


type Preview t msg a
    = Preview
        { meta : Meta

        -- Prefix is separate from meta.id since sometimes we'll recurse into
        -- previews with a different prefix.
        , prefix : String
        , pointer : Int
        , value : PreviewLookup t msg -> Lookup t -> a
        , controls : List (PreviewLookup t msg -> Lookup t -> Html (List ( String, Type t )))
        , state : List (Lookup t -> Html ())
        }


type alias Meta =
    { id : String, name : String }


{-| Create a preview with the name, id and definition. Similar to
'succeed'-style functions.
-}
preview : String -> { name : String } -> a -> Preview t msg a
preview id meta value =
    Preview
        { meta = { id = id, name = meta.name }
        , pointer = 0
        , prefix = id ++ "."
        , value = \_ _ -> value
        , controls = []
        , state = []
        }


withState :
    String
    -> Block t a
    -> Preview t (Msg t msg) (a -> (a -> Msg t msg) -> c)
    -> Preview t (Msg t msg) c
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
            \pl lookup ->
                p.value pl
                    lookup
                    (b.fromType (id >> lookup) |> Maybe.withDefault b.default)
                    (b.toType >> List.map (Tuple.mapFirst id) >> SetState)
        , controls = p.controls
        , state = p.state ++ [ \lookup -> b.display label (id >> lookup) ]
        }


idFunc : Preview t msg x -> String -> String
idFunc (Preview p) s =
    let
        prefix =
            p.prefix ++ "." ++ String.fromInt p.pointer ++ "."
    in
    prefix ++ s


withMsg : Preview t (Msg t m) ((m -> Msg t m) -> a) -> Preview t (Msg t m) a
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
            \pl lookup ->
                p.value pl
                    (id >> lookup)
                    Msg
        , controls = p.controls
        , state = p.state
        }


withControl : String -> Block t a -> Preview t msg (a -> b) -> Preview t msg b
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
            \pl lookup ->
                p.value pl
                    lookup
                    (b.fromType (id >> lookup) |> Maybe.withDefault b.default)
        , controls =
            p.controls
                ++ [ \_ lookup ->
                        b.control id label (id >> lookup)
                            |> Html.map (List.map (Tuple.mapFirst id))
                   ]
        , state = p.state
        }


withSubcomponent : String -> Preview t msg (Html msg -> b) -> Preview t msg b
withSubcomponent label ((Preview p) as preview_) =
    let
        id =
            idFunc preview_
    in
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer + 1
        , value =
            \pl lookup ->
                let
                    lookup_ =
                        id >> lookup

                    subcomponentId =
                        lookup_ "component"
                            |> Maybe.andThen Type.stringValue
                            |> Maybe.withDefault "default-component"

                    id_ s =
                        let
                            prefix =
                                subcomponentId ++ "."
                        in
                        prefix ++ s
                in
                pl subcomponentId
                    |> view pl (id_ >> lookup_)
                    |> p.value pl lookup
        , controls =
            p.controls
                ++ [ \pl lookup ->
                        let
                            lookup_ =
                                id >> lookup

                            subcomponentId =
                                lookup_ "component"
                                    |> Maybe.andThen Type.stringValue
                                    |> Maybe.withDefault "default-component"

                            id_ s =
                                let
                                    prefix =
                                        subcomponentId ++ "."
                                in
                                prefix ++ s
                        in
                        pl subcomponentId
                            |> controls pl (id_ >> lookup) identity
                            |> Html.div []
                   ]
        , state = p.state
        }


map : (a -> b) -> Preview t msg a -> Preview t msg b
map f (Preview p) =
    Preview
        { meta = p.meta
        , prefix = p.prefix
        , pointer = p.pointer
        , value = \scl lookup -> f (p.value scl lookup)
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


type alias PreviewLookup t msg =
    String -> Preview t msg (Html msg)


type alias Identifier =
    String -> String


identifier : Preview t msg a -> String
identifier (Preview p) =
    p.meta.id


name : Preview t msg a -> String
name (Preview p) =
    p.meta.name


{-| I have a hunch that some function needs to be parsed in here to support
embedding components.
-}
view : PreviewLookup t m -> Lookup t -> Preview t m (Html msg) -> Html msg
view pl lookup (Preview p) =
    p.value pl lookup


controls :
    PreviewLookup t m
    -> Lookup t
    -> (List ( String, Type t ) -> msg)
    -> Preview t m (Html m)
    -> List (Html msg)
controls pl lookup msg (Preview b) =
    let
        apply :
            (PreviewLookup t m -> Lookup t -> Html (List ( String, Type t )))
            -> Html msg
        apply f =
            f pl lookup |> Html.map msg
    in
    List.map apply b.controls


state :
    Lookup t
    -> (List ( String, Type t ) -> msg)
    -> Preview t m (Html m)
    -> List (Html msg)
state lookup msg (Preview p) =
    let
        apply : (Lookup t -> Html ()) -> Html msg
        apply f =
            f lookup |> Html.map (\_ -> msg [])
    in
    List.map apply p.state
