module Component.Internal exposing
    ( Block
    , BlockI(..)
    , BlockI_
    , Builder(..)
    , Component(..)
    , ComponentRef(..)
    , Component_
    , Library(..)
    , Library_
    , Lookup
    , Meta
    , Preview
    , PreviewGroup
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
    , update : i -> ( i, List e )
    }


{-| Builder for composing block types.
-}
type Builder e t i r a
    = Builder (Library e t -> State Ref (BlockI_ e t i r a))



-- COMPONENT TYPES


{-| Update type for component state changes and effects.
-}
type Update t e
    = Update (List ( Ref, Type t )) (List e)
    | WithEffect (List ( Ref, Type t )) (List e)
    | Computed (Lookup t -> ( List ( Ref, Type t ), List e ))


{-| Opaque component type.
-}
type Component e t a
    = Component (Component_ e t a)


{-| Internal component record.
-}
type alias Component_ e t a =
    { value : Library e t -> Lookup t -> State Ref a
    , controls : Library e t -> State Ref (List (Lookup t -> Html ( List ( Ref, Type t ), List e )))
    , reference : State Ref Ref
    }


{-| Component metadata.
-}
type alias Meta =
    { id : String, name : String }


{-| A preview is a component with metadata.
-}
type alias Preview e t =
    ( Meta, Component e t (View (Update t e)) )


{-| A group of previews.
-}
type alias PreviewGroup e t =
    { name : String, previews : List (Preview e t) }


{-| A view is the main HTML plus optional auxiliary views (portals).
-}
type alias View msg =
    ( Html msg, Dict String (Html msg) )


{-| Opaque library type wrapping the current component ID and library data.
-}
type Library e t
    = Library
        -- Current component id (used in previewBlock)
        String
        (Library_ e t)


{-| Internal library record.
-}
type alias Library_ e t =
    { index : List Meta
    , groups : List { name : String, components : List Meta }
    , lookup : String -> Maybe ( String, Component e t (View (Update t e)) )
    , lookup_ : String -> Maybe ( String, Ref, Component_ e t (View (Update t e)) )
    }


{-| Reference to a component by ID.
-}
type ComponentRef
    = ComponentRef String
