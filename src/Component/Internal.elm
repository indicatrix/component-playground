module Component.Internal exposing
    ( Builder(..)
    , ComponentE
    , ComponentRef(..)
    , Control(..)
    , ControlI_
    , Frame(..)
    , Index(..)
    , Library(..)
    , Library_
    , Lookup
    , Playground(..)
    , Update(..)
    , View
    )

import Component.Ref exposing (Ref)
import Component.Type exposing (Type)
import Dict exposing (Dict)
import Html exposing (Html)
import State exposing (State)


{-| Lookup function to retrieve stored values by Ref.
-}
type alias Lookup t =
    Ref -> Maybe (Type t)



-- CONTROLS TYPES


{-| Control with potentially different input and output types.

Type variables:

  - `e` — the effect type produced when the controls' state changes.
  - `t` — the library-consumer's custom type for storing their own types.
  - `i` — the internal representation (storage type).
  - `a` — the output type (what the view receives). For simple controls,
    `i` and `a` are the same; `map` handles the conversion when they differ.

-}
type Control e t i a
    = Control (Library e t -> State Ref (ControlI_ e t i i a))


{-| Internal record describing how to store, retrieve and render a value.

  - `r` is the "ultimate" type when used inside a Builder (the whole record
    being constructed). Builders pass `r` through the chain so each field
    can access the full record default.

-}
type alias ControlI_ e t i r a =
    --| Create a type from the lookup, using a default. The ultimate type, `r`,
    -- is also provided for use in Builders.
    { fromType : r -> i -> Lookup t -> i

    --| Convert a type for later use in Lookup t.
    , toType : r -> List ( Ref, Type t )

    --| A list of controls to use. Again uses the ultimate type, `r`, for use
    -- in builders. Each control can get and set Lookup t. The Maybe String is
    -- the label shown on this control in the UI, supplied at render time rather
    -- than baked in at block construction time. Nothing suppresses the group
    -- heading in toControls, rendering fields flat without indentation.
    , controls : Maybe String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))

    --| The default value. Note this is passed into fromType so it can be
    -- overridden (see withDefault).
    , default : i

    --| Map the internal representation to the output type.
    , map : Lookup t -> i -> a

    --| Transform the value and produce effects after a state change.
    -- Receives the old value (before) and the new value (after).
    , update : i -> i -> ( i, List e )

    --| Optional label for this control when used at the top level of a
    -- component (i.e. not inside a builder group). When Just, it overrides
    -- the component name as the label passed to `controls`. When Nothing,
    -- the component name flows through as the section heading.
    , description : Maybe String
    }


{-| Builder for composing controls for record types.
-}
type Builder e t i r a
    = Builder (Library e t -> State Ref (ControlI_ e t i r a))



-- PLAYGROUND TYPES


{-| Update type for component state changes and effects.
-}
type Update t e
    = Update (List ( Ref, Type t )) (List e)


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    ( Html msg, Dict String (Html msg) )


{-| A Component with the model type `m` erased. Stores the rendered view and
controls as closures over the allocated Refs, so they only need a Lookup to
produce HTML.
-}
type alias ComponentE e t =
    { render : Lookup t -> View (Update t e)
    , controls : Lookup t -> List (Html (Update t e))
    }


{-| A frame within a playground page. The `msg` type parameter is the message
type of the HTML in `StaticFrame`; interactive frames fix it to `Update t e`.

InteractiveFrame and ExampleFrame carry the component id for library lookup.
Component ids must be unique across all components in the playground.

-}
type Frame e t msg
    = InteractiveFrame { id : String, name : String } (Library e t -> State Ref (ComponentE e t))
    | ExampleFrame { id : String, name : String } String (Library e t -> State Ref (ComponentE e t))
    | StaticFrame (Html msg)


{-| A playground is a recursive tree of named pages and groups.
-}
type Playground e t msg
    = Page { id : String, name : String } (List (Frame e t msg))
    | Group { id : String, name : String } (List (Playground e t msg))


{-| Opaque library type. The Library\_ carries navigation metadata used by
blocks that need to reference other pages (e.g. Control.preview).
-}
type Library e t
    = Library
        -- Current page id
        String
        (Library_ e t)


{-| Navigation metadata for the library.
-}
type alias Library_ e t =
    { index : List { id : String, name : String }
    , groups : List { name : String, pages : List { id : String, name : String } }
    , lookupDef : String -> Maybe (Library e t -> State Ref (ComponentE e t))
    }


{-| Opaque reference to a component, wrapping a string id.
-}
type ComponentRef
    = ComponentRef String


{-| Sidebar index tree. Pages are leaves (empty children), groups are nodes.
-}
type Index
    = Index { id : String, name : String, children : List Index }
