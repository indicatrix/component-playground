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

import Component.Application.Theme exposing (Theme)
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


{-| Control with potentially different storage and output types.

Type variables:

  - `e` — the effect type produced when the controls' state changes.
  - `t` — the library-consumer's custom type for storing their own types.
  - `state` — the storage type.
  - `value` — the output type (what the view receives). For simple controls,
    `state` and `value` are the same; `map` handles the conversion when they
    differ.

-}
type Control e t state value
    = Control (Library e t -> State Ref (ControlI_ e t state state value))


{-| Internal record describing how to store, retrieve, and render a control.

  - `state` — what this control holds: appears in `default`, `fromType`,
    `update`, and as the input to `map`.
  - `final` — the final/complete record that `fromType`, `toType`, and
    `controls` read from.
  - `value` — what `map` produces.

-}
type alias ControlI_ e t state final value =
    { fromType : final -> state -> Lookup t -> state
    , toType : final -> List ( Ref, Type t )
    , controls : Theme -> Maybe String -> final -> List (Lookup t -> Html (List ( Ref, Type t )))
    , default : state
    , map : Lookup t -> state -> value
    , update : state -> state -> ( state, List e )
    , description : Maybe String
    }


{-| Builder for composing controls for record types.
-}
type Builder e t state final value
    = Builder (Library e t -> State Ref (ControlI_ e t state final value))



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
    , controls : Theme -> Lookup t -> List (Html (Update t e))
    }


{-| A frame within a playground page.

InteractiveFrame and ExampleFrame carry the component id for library lookup.
Component ids must be unique across all components in the playground.
StaticFrame carries HTML that can produce effects but not state changes.
GalleryFrame carries a display name and pre-assembled static HTML.

-}
type Frame e t
    = InteractiveFrame
        { id : String, name : String }
        (Library e t -> State Ref (ComponentE e t))
        (Html (Update t e) -> Html (Update t e))
    | ExampleFrame
        { id : String, name : String }
        String
        (Library e t -> State Ref (ComponentE e t))
        (Html (Update t e) -> Html (Update t e))
    | StaticFrame (Html (List e))
    | GalleryFrame String (Library e t -> State Ref (Html (List e)))


{-| A playground is a recursive tree of named pages and groups.
-}
type Playground e t
    = Page { id : String, name : String } (List (Frame e t))
    | Group { id : String, name : String } (List (Playground e t))


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
