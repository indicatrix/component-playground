module Component exposing
    ( Component, Component_, ComponentInstance, ComponentRef, Control, Control_
    , Preset, Token, TokenGroup, Update, View
    , component, component_, componentWithPortals, componentWithPortals_
    , preset, withPresets
    , withInspectorBinding
    , withRemeasure
    , tokenGroup, withTokens, withTokensFrom
    , withReference
    , toRef
    , ComponentReference
    )

{-| Component Playground — an interactive component testing library for Elm.

Build interactive playgrounds for your UI components in three steps:

1.  **Components** (this module) define _what_ to render: a set of controls
    and a view function. Controls describe how to store, edit, and display
    each parameter your component accepts.

2.  **Frames** (`Component.Frame`) define _how_ to present a component on a
    page. `Frame.fromComponent` gives an interactive frame with a live
    controls panel. `Frame.presets` adds a preset tab bar across the top.
    `Frame.static` inserts static HTML. `Frame.gallery` enumerates variants.
    `Frame.wrap` adds chrome around any frame.

3.  **Playgrounds** (`Component.Playground`) organise frames into named pages
    and groups, producing a navigable sidebar. Pass the playground tree to
    `Component.Application.element` to run it.


# Core Types

@docs Component, Component_, ComponentInstance, ComponentRef, Control, Control_


# Supporting Types

@docs Preset, Token, TokenGroup, Update, View


# Component Constructors

@docs component, component_, componentWithPortals, componentWithPortals_


# Presets

@docs preset, withPresets


# Inspector

@docs withInspectorBinding


# Layout remeasurement

@docs withRemeasure


# Design tokens

@docs tokenGroup, withTokens, withTokensFrom


# Source reference

@docs withReference


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


{-| Opaque handle to a specific component instance. Provided to
`Control.withUpdate` so controls can construct portal content closures
via `Component.Application.renderPortal`.
-}
type alias ComponentInstance =
    Internal.ComponentInstance


{-| Opaque reference to a component. Use `toRef` to create and pass to
`Control.componentRef` defaults.
-}
type alias ComponentRef =
    Internal.ComponentRef


{-| A named preset configuration for a component. Construct with `preset`
(for the common no-wrap case) or build the record directly to provide a
per-preset `wrap` function.
-}
type alias Preset t i =
    Internal.Preset t i


{-| A category of design tokens a component consumes (e.g. Colour, Motion),
with the specific tokens within it. Build with `tokenGroup` and attach with
`withTokens`.
-}
type alias TokenGroup =
    Internal.TokenGroup


{-| A single design token: its name (e.g. `pw-ink`) and resolved value
(e.g. `#0A0F22`).
-}
type alias Token =
    Internal.Token


{-| A canonical source reference for a component: the repo-relative
`sourcePath` to its implementation, and an optional `identifier` (e.g. an Elm
`Module.function`) used when the file holds more than one component. Attach with
`withReference`; the Inspector renders it as the component's Component-section
reference.
-}
type alias ComponentReference =
    Internal.ComponentReference


{-| Update type for component state changes. Tagged with the owning
ComponentInstance so Application.update can dispatch correctly.
-}
type alias Update t =
    Internal.Update t


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
        , presets = []
        , tokens = always []
        , inspectorBinding = Nothing
        , reference = Nothing
        , remeasure = Nothing
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
        , presets = []
        , tokens = always []
        , inspectorBinding = Nothing
        , reference = Nothing
        , remeasure = Nothing
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
        , presets = []
        , tokens = always []
        , inspectorBinding = Nothing
        , reference = Nothing
        , remeasure = Nothing
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
        , presets = []
        , tokens = always []
        , inspectorBinding = Nothing
        , reference = Nothing
        , remeasure = Nothing
        }



-- PRESETS


{-| Build a `Preset` with the default (identity) wrap function. Pair with
`withPresets` to declare the presets a component offers.

    chart
        |> Component.withPresets
            [ Component.preset "Bar" barConfig
            , Component.preset "Line" lineConfig
            ]

-}
preset : String -> i -> Preset t i
preset name value =
    { name = name, value = value, wrap = identity }


{-| Attach a list of named preset configurations to a component. Each preset
is a canonical state value for the component's storage type; picking a preset
replaces the whole state at once.

The first preset in the list becomes the component's initial state.

With presets attached, the component's controls panel gains a "Preset"
dropdown. When the component is rendered via `Frame.presets`, the dropdown
is suppressed in favour of a first-class tab bar above the view. Embedded
components (via `Control.componentRef`) always show the dropdown inline with
their controls.

-}
withPresets : List (Preset t i) -> Component_ e t i m msg -> Component_ e t i m msg
withPresets ps (Component_ c) =
    Component_ { c | presets = ps }


{-| Link a component's own state to its Inspector panel's open/close.

By default the Inspector's open state is owned by the shell (a global toggle on
the ribbon). A component with an inspector binding owns it instead: the shell
reads `isOpen` to decide whether the panel is shown, and calls `setOpen` when
the user opens it from the ribbon or closes it from the panel's own control.

This lets a component couple selection to the panel as a single state — open the
Inspector when something is selected, and close it (clearing the selection) when
the panel is dismissed — so the two never drift out of sync:

    assetBrowser
        |> Component.withInspectorBinding
            { isOpen = \state -> state.inspectorOpen
            , setOpen =
                \open state ->
                    if open then
                        { state | inspectorOpen = True }

                    else
                        -- dismissing the panel also clears the selection
                        { state | inspectorOpen = False, selected = Nothing }
            }

`isOpen` may report `True` with nothing selected — that is the Inspector's empty
state, reached by opening it from the ribbon before picking anything.

-}
withInspectorBinding :
    { isOpen : i -> Bool, setOpen : Bool -> i -> i }
    -> Component_ e t i m msg
    -> Component_ e t i m msg
withInspectorBinding binding (Component_ c) =
    Component_ { c | inspectorBinding = Just binding }


{-| Give a component a generic **post-layout remeasurement** hook.

Some components render from live DOM measurements (a scroll viewport's
`clientWidth` / `scrollWidth`, an element's box) rather than from state alone.
Those measurements go stale when the available width changes for a reason the
component never sees as a state update — the browser window resizes, the
Inspector opens or closes, the user navigates to the page. The shell calls this
hook for the current page's live components after any such layout change, once
the new layout has been rendered, so the component can re-read the DOM.

The callback receives the component instance, its state setter, and its current
state, and returns effects (typically a `Browser.Dom.getViewportOf` measurement
that folds fresh metrics back through the setter). It changes no state itself;
its only job is to emit the measurement effects. The shell owns _when_ to
remeasure; the component owns _what_ to measure — so the mechanism carries no
component-specific knowledge.

    ribbon
        |> Component.withRemeasure
            (\_ setter state -> [ measureViewport setter state ])

-}
withRemeasure :
    (ComponentInstance -> (i -> Update t) -> i -> List e)
    -> Component_ e t i m msg
    -> Component_ e t i m msg
withRemeasure hook (Component_ c) =
    Component_ { c | remeasure = Just hook }



-- DESIGN TOKENS


{-| Build a design-token category from a name and its `( name, value )` token
pairs:

    Component.tokenGroup "Colour"
        [ ( "pw-ink", "#0A0F22" )
        , ( "pw-surface", "#FEFEFE" )
        ]

-}
tokenGroup : String -> List ( String, String ) -> TokenGroup
tokenGroup category tokens =
    { category = category
    , tokens = List.map (\( name, value ) -> { name = name, value = value }) tokens
    }


{-| Declare a static set of design tokens a component consumes, grouped by
category. The Inspector renders exactly these groups for the selected component —
and only these — so the token reference is component-aware rather than a global
list. Categories a component does not consume are simply omitted.

    button
        |> Component.withTokens
            [ Component.tokenGroup "Colour" [ ( "pw-ink", "#0A0F22" ) ]
            , Component.tokenGroup "Motion" [ ( "ease-out", "100ms" ) ]
            ]

Use `withTokensFrom` instead when the tokens depend on the component's current
configuration (style, size, state…).

-}
withTokens : List TokenGroup -> Component_ e t i m msg -> Component_ e t i m msg
withTokens ts (Component_ c) =
    Component_ { c | tokens = always ts }


{-| Declare the design tokens a component consumes as a function of its current
output model, so the Inspector reports exactly the tokens the configuration on
screen actually renders — not a static union across every variant. The function
is re-evaluated on every state change, so the token reference updates live as the
user edits the controls.

    button
        |> Component.withTokensFrom
            (\model ->
                [ Component.tokenGroup "Colour" (fillTokens model.style model.state)
                , Component.tokenGroup "Sizing" [ sizeToken model.size ]
                ]
            )

`withTokens groups` is the constant special case (`withTokensFrom (always groups)`).

-}
withTokensFrom : (m -> List TokenGroup) -> Component_ e t i m msg -> Component_ e t i m msg
withTokensFrom f (Component_ c) =
    Component_ { c | tokens = f }



-- SOURCE REFERENCE


{-| Declare the canonical source reference for a component — the repo-relative
path to its implementation and, when the file holds more than one component, a
precise symbol identifier. The Inspector's Component section renders it so the
exact implementation can be located (and copied into a coding-agent prompt)
without confusing it with playground examples, wrappers, consumers or
similarly-named components.

    button
        |> Component.withReference
            { sourcePath = "js/src/UI/Button/Regular.elm"
            , identifier = Nothing
            }

    workspaceTab
        |> Component.withReference
            { sourcePath = "js/src/UI/RibbonTab.elm"
            , identifier = Just "UI.RibbonTab.workspace"
            }

Omit `identifier` (pass `Nothing`) when the path points to a dedicated
single-component file and is already unambiguous. Provide it when several
components, constructors or exported functions share the file.

-}
withReference : ComponentReference -> Component_ e t i m msg -> Component_ e t i m msg
withReference ref (Component_ c) =
    Component_ { c | reference = Just ref }



-- REFERENCES


{-| Extract an opaque component reference. Use this to provide default
values for `Control.componentRef` controls.

    Control.componentRef
        |> Control.withDefault (Component.toRef myComponent)

-}
toRef : Component_ e t i m msg -> ComponentRef
toRef (Component_ c) =
    ComponentRef c.id
