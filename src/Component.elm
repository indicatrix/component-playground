module Component exposing
    ( Component, Component_, ComponentRef, Control, Control_
    , Update, View
    , component, component_, componentWithPortals, componentWithPortals_
    , toRef
    )

{-| Component Playground — an interactive component testing library for Elm.

Build interactive playgrounds for your UI components in three steps:

1.  **Components** (this module) define _what_ to render: a set of controls
    and a view function. Controls describe how to store, edit, and display
    each parameter your component accepts.

2.  **Frames** (`Component.Frame`) define _how_ to present a component on a
    page. `Frame.fromComponent` gives an interactive frame with a live
    controls panel. `Frame.example` pins a specific starting state.
    `Frame.static` inserts static HTML. `Frame.gallery` enumerates variants.
    `Frame.wrap` adds chrome around any frame.

3.  **Playgrounds** (`Component.Playground`) organise frames into named pages
    and groups, producing a navigable sidebar. Pass the playground tree to
    `Component.Application.element` to run it.


# Core Types

@docs Component, Component_, ComponentRef, Control, Control_


# Supporting Types

@docs Update, View


# Component Constructors

@docs component, component_, componentWithPortals, componentWithPortals_


# References

@docs toRef

-}

import Component.Internal as Internal
    exposing
        ( ComponentRef(..)
        , Component_(..)
        )
import Dict
import Html exposing (Html)



-- TYPE RE-EXPORTS


{-| Alias for the control type used in `Component` records. This is the same
type as `Control.Control` — re-exported here so users can annotate component
definitions without importing the `Component.Control` module.
-}
type alias Control e t state =
    Internal.Control e t state state


{-| General control type where storage type `state` may differ from output
`value`.
-}
type alias Control_ e t state value =
    Internal.Control e t state value


{-| A component where storage and output types are the same.
Create with `component` or `componentWithPortals`.
-}
type alias Component e t m msg =
    Internal.Component_ e t m m msg


{-| A component where storage type `i` may differ from output type `m`.
Create with `component_` or `componentWithPortals_`.
-}
type alias Component_ e t i m msg =
    Internal.Component_ e t i m msg


{-| Opaque reference to a component. Use `toRef` to create and pass to
`Control.componentRef` defaults.
-}
type alias ComponentRef =
    Internal.ComponentRef


{-| Update type for component state changes and effects.
-}
type alias Update t e =
    Internal.Update t e


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    Internal.View msg



-- COMPONENT CONSTRUCTORS


{-| Create a component from a plain `Html` view (no portals). This is the
common case — use `componentWithPortals` if you need named portal slots.

    myButton =
        Component.component
            { id = "button"
            , name = "Button"
            , controls =
                Control.builder ButtonModel
                    |> Control.add "Label" .label Control.string
                    |> Control.toControl
            , view =
                \model setter ->
                    Html.button [ Html.Events.onClick (setter { model | clicked = True }) ]
                        [ Html.text model.label ]
            }

-}
component :
    { id : String
    , name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> Html msg
    }
    -> Component e t m msg
component c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \_ m setter -> ( c.view m setter, Dict.empty )
        }


{-| Create a component whose view returns named portal slots alongside the
main HTML. Use `component` instead if you don't need portals.
-}
componentWithPortals :
    { id : String
    , name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> View msg
    }
    -> Component e t m msg
componentWithPortals c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \_ m setter -> c.view m setter
        }


{-| Create a component where storage type `i` differs from output type `m`.
The view receives both the storage record and the mapped output.
-}
component_ :
    { id : String
    , name : String
    , controls : Control_ e t i m
    , view : i -> m -> (i -> msg) -> Html msg
    }
    -> Component_ e t i m msg
component_ c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \i m setter -> ( c.view i m setter, Dict.empty )
        }


{-| Like `component_`, but the view returns named portal slots.
-}
componentWithPortals_ :
    { id : String
    , name : String
    , controls : Control_ e t i m
    , view : i -> m -> (i -> msg) -> View msg
    }
    -> Component_ e t i m msg
componentWithPortals_ c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = c.view
        }



-- REFERENCES


{-| Extract an opaque component reference. Use this to provide default
values for `Control.componentRef` controls.

    Control.componentRef
        |> Control.withDefault (Component.toRef myComponent)

-}
toRef : Component_ e t i m msg -> ComponentRef
toRef (Component_ c) =
    ComponentRef c.id
