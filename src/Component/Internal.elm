module Component.Internal exposing
    ( Block
    , Builder(..)
    , Controls(..)
    , ControlsI_
    , Frame(..)
    , FrameInternals
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


{-| Controls where input type equals output type. Alias for `Controls e t a a`.
-}
type alias Block e t a =
    Controls e t a a


{-| Controls with potentially different input and output types.

Type variables:

  - `e` — the effect type produced when the controls' state changes.
  - `t` — the library-consumer's custom type for storing their own types.
  - `i` — the internal representation (storage type).
  - `a` — the output type (what the view receives).

-}
type Controls e t i a
    = Block (Library e t -> State Ref (ControlsI_ e t i i a))


{-| Internal record describing how to store, retrieve and render a value.

  - `r` is the "ultimate" type when used inside a Builder (the whole record
    being constructed). Builders pass `r` through the chain so each field
    can access the full record default.

-}
type alias ControlsI_ e t i r a =
    --| Create a type from the lookup, using a default. The ultimate type, `r`,
    -- is also provided for use in Builders.
    { fromType : r -> i -> Lookup t -> i

    --| Convert a type for later use in Lookup t.
    , toType : r -> List ( Ref, Type t )

    --| A list of controls to use. The String label and default `r` are
    -- supplied at render time rather than baked in at construction time.
    , controls : String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))

    --| The default value.
    , default : i

    --| Map the internal representation to the output type.
    , map : Lookup t -> i -> a

    --| Transform the value and produce effects after a state change.
    -- Receives the old value (before) and the new value (after).
    , update : i -> i -> ( i, List e )
    }


{-| Builder for composing controls for record types.
-}
type Builder e t i r a
    = Builder (Library e t -> State Ref (ControlsI_ e t i r a))



-- PLAYGROUND TYPES


{-| Update type for component state changes and effects.
-}
type Update t e
    = Update (List ( Ref, Type t )) (List e)
    | WithEffect (List ( Ref, Type t )) (List e)
    | Computed (Lookup t -> ( List ( Ref, Type t ), List e ))


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    ( Html msg, Dict String (Html msg) )


{-| The internals of an interactive frame, with refs already allocated.
-}
type alias FrameInternals e t =
    { render : Lookup t -> View (Update t e)
    , controls : Lookup t -> List (Html (Update t e))
    }


{-| A frame within a playground page.
-}
type Frame e t
    = InteractiveFrame (State Ref (FrameInternals e t))
    | ExampleFrame String (State Ref (FrameInternals e t))
    | DocoFrame (Html (Update t e))


{-| A playground is a recursive tree of named pages and groups.
-}
type Playground e t
    = Page { id : String, name : String } (List (Frame e t))
    | Group { id : String, name : String } (List (Playground e t))


{-| Opaque library type. The Library\_ carries navigation metadata used by
blocks that need to reference other components (e.g. Controls.preview).
-}
type Library e t
    = Library
        -- Current page id
        String
        Library_


{-| Navigation metadata for the library.
-}
type alias Library_ =
    { index : List { id : String, name : String }
    , groups : List { name : String, pages : List { id : String, name : String } }
    }
