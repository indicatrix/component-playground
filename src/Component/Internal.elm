module Component.Internal exposing
    ( Builder(..)
    , ComponentE
    , ComponentInstance(..)
    , ComponentRef(..)
    , Component_(..)
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
    , update : ComponentInstance -> (state -> Update t) -> state -> state -> ( state, List e )
    , description : Maybe String
    }


{-| Builder for composing controls for record types.
-}
type Builder e t state final value
    = Builder (Library e t -> State Ref (ControlI_ e t state final value))



-- PLAYGROUND TYPES


{-| Message type for frames. State changes from an interactive component,
tagged with the owning ComponentInstance so Application.update can look up
the ComponentE and call its update function at dispatch time.

Static frames produce no messages (use `Html Never`). Galleries use a
sentinel ComponentInstance that Application.update silently no-ops on.

-}
type Update t
    = Update ComponentInstance (List ( Ref, Type t ))


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    ( Html msg, Dict String (Html msg) )


{-| A Component with the model type `m` erased. Stores the rendered view and
controls as closures over the allocated Refs, so they only need a Lookup to
produce HTML.

The `update` field is called by Application.update after applying state
changes. It receives the old and new lookups and returns any additional
state changes plus effects.

-}
type alias ComponentE e t =
    { render : Lookup t -> View (Update t)
    , controls : Theme -> Lookup t -> List (Html (Update t))
    , update : Lookup t -> Lookup t -> ( List ( Ref, Type t ), List e )
    }


{-| A component with its controls, view, and identifying metadata. The `i`/`m`
type parameters are retained here so the constructors that consume a component
(e.g. `Frame.fromComponent`) can thread the storage/value types through their
rendering closures. Once stored in a `Frame` those parameters are erased.
-}
type Component_ e t i m msg
    = Component_
        { id : String
        , name : String
        , controls : Control e t i m
        , view : i -> m -> (i -> msg) -> View msg
        }


{-| A frame within a playground page.

InteractiveFrame and ExampleFrame carry the component id for library lookup.
Component ids must be unique across all components in the playground.

All four variants use `Html (Update t)` uniformly, so `Frame.wrap` applies
across every variant. Static frames wrap `Html Never` via `Html.map never`;
gallery frames use a sentinel ComponentInstance that Application.update
silently no-ops on.

-}
type Frame e t
    = InteractiveFrame { id : String, name : String } (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | ExampleFrame { id : String, name : String } String (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | StaticFrame (Html (Update t))
    | GalleryFrame String (Library e t -> State Ref (Html (Update t)))


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


{-| Opaque handle to a specific component instance, capturing both the
component definition id (ComponentRef) and the Ref root for this
instance's state slots. Provided to `Control.withUpdate` so controls can
construct portal content closures via `renderPortal`.
-}
type ComponentInstance
    = ComponentInstance ComponentRef Ref


{-| Sidebar index tree. Pages are leaves (empty children), groups are nodes.
-}
type Index
    = Index { id : String, name : String, children : List Index }
