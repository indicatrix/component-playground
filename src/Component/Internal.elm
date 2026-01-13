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
    , Msg(..)
    , Preview
    , PreviewGroup
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
type alias Block t a =
    BlockI t a a


{-| Block with potentially different input and output types.
-}
type BlockI t i a
    = Block (State Ref (BlockI_ t i i a))


{-| Internal type for representing types that can be used in a Component
Playground.

Type definitions:

  - `a` is the end type. Using blocks in previews applies the `a` type.
  - `t` is the library-consumer's custom type for storing their own types.
  - `i` is the internal representation. This allows for an internal
    representation. BlockI makes this explicit, while Block assumes i == a.
    `.map` provides a mapping from i to a.
  - `r` is the ultimate type when used inside a Builder. We need this to store
    types and get defaults at each step while building the type.

-}
type alias BlockI_ t i r a =
    --| Create a type from the lookup, using a default. The ultimate type, `r`,
    -- is also provided for use in Builders.
    { fromType : r -> i -> Lookup t -> i

    --| Convert a type for later use in Lookup t.
    , toType : r -> List ( Ref, Type t )

    --| A list of controls to use. Again uses the ultimate type, `r` for use in
    -- builders. Each control can get and set Lookup t.
    , controls : r -> List (Lookup t -> Html (List ( Ref, Type t )))

    --| The default value for some type. Note this is passed into fromType so
    -- it can be overridden.
    , default : i

    --| Map the internal representation to the end type.
    , map : Lookup t -> i -> a
    }


{-| Builder for composing block types.
-}
type Builder t i r a
    = Builder (State Ref (BlockI_ t i r a))



-- COMPONENT TYPES


{-| Message type for component state updates.
-}
type Msg t msg
    = SetState (List ( Ref, Type t ))
    | Msg (List ( Ref, Type t )) msg
    | Update (Lookup t -> ( List ( Ref, Type t ), msg ))


{-| Opaque component type.
-}
type Component t msg a
    = Component (Component_ t msg a)


{-| Internal component record.
-}
type alias Component_ t msg a =
    { value : Library t msg -> Lookup t -> State Ref a
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
    , reference : State Ref Ref
    }


{-| Component metadata.
-}
type alias Meta =
    { id : String, name : String }


{-| A preview is a component with metadata.
-}
type alias Preview t msg =
    ( Meta, Component t msg (View msg) )


{-| A group of previews.
-}
type alias PreviewGroup t msg =
    { name : String, previews : List (Preview t msg) }


{-| A view is the main HTML plus optional auxiliary views (portals).
-}
type alias View msg =
    ( Html msg, Dict String (Html msg) )


{-| Opaque library type wrapping the current component ID and library data.
-}
type Library t msg
    = Library
        -- Current component id (used in previewBlock)
        String
        (Library_ t msg)


{-| Internal library record.
-}
type alias Library_ t msg =
    { index : List Meta
    , groups : List { name : String, components : List Meta }
    , lookup : String -> Maybe ( String, Component t msg (View msg) )
    , lookup_ : String -> Maybe ( String, Ref, Component_ t msg (View msg) )
    }


{-| Reference to a component by ID.
-}
type ComponentRef
    = ComponentRef String
