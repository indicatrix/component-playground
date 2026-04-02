module Component.Internal exposing
    ( Block
    , BlockI(..)
    , BlockI_
    , Builder(..)
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



-- BLOCK TYPES


{-| Simple block where input type equals output type.
-}
type alias Block e t a =
    BlockI e t a a


{-| Block with potentially different input and output types.
-}
type BlockI e t i a
    = Block (Library e t -> State Ref (BlockI_ e t i i a))


{-| Internal type for representing types that can be used in a Component
Playground.

Type definitions:

  - `e` is the effect type produced when the block's state changes.
  - `a` is the end type. Using blocks in previews applies the `a` type.
  - `t` is the library-consumer's custom type for storing their own types.
  - `i` is the internal representation. This allows for an internal
    representation. BlockI makes this explicit, while Block assumes i == a.
    `.map` provides a mapping from i to a.
  - `r` is the ultimate type when used inside a Builder. We need this to store
    types and get defaults at each step while building the type.

-}
type alias BlockI_ e t i r a =
    --| Create a type from the lookup, using a default. The ultimate type, `r`,
    -- is also provided for use in Builders.
    { fromType : r -> i -> Lookup t -> i

    --| Convert a type for later use in Lookup t.
    , toType : r -> List ( Ref, Type t )

    --| A list of controls to use. Again uses the ultimate type, `r` for use in
    -- builders. Each control can get and set Lookup t. The String is the label
    -- shown on this control in the UI, supplied at render time by withControl
    -- and friends rather than baked in at block construction time.
    , controls : String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))

    --| The default value for some type. Note this is passed into fromType so
    -- it can be overridden.
    , default : i

    --| Map the internal representation to the end type.
    , map : Lookup t -> i -> a

    --| Transform the value and produce effects. Called after state changes.
    -- Receives the old value (before the change) and the new value (after).
    , update : i -> i -> ( i, List e )
    }


{-| Builder for composing block types.
-}
type Builder e t i r a
    = Builder (Library e t -> State Ref (BlockI_ e t i r a))



-- PLAYGROUND TYPES


{-| Update type for component state changes and effects.
-}
type Update t e
    = Update (List ( Ref, Type t )) (List e)
    | WithEffect (List ( Ref, Type t )) (List e)
    | Computed (Lookup t -> ( List ( Ref, Type t ), List e ))


{-| A view is the main HTML plus optional auxiliary views (portals).
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
