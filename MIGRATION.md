# Component Playground — Redesign

A ground-up redesign of the public API. The previous API mixed concerns across
`Component`, `Block`, and `Builder` types with a pipeline-builder that was
difficult to extend and reason about. This redesign replaces that with three
orthogonal concepts and a significantly simpler API.

## Design Goals

### 1. Support examples ("stories")

The previous API had a single presentation mode — an interactive preview. The
new API introduces **Frames**, which determine how a component appears on a
page:

- `explore` — fully interactive with a live controls panel (replaces the old
  preview).
- `example` — pins a specific starting state, useful for showing configured
  variants of a component (e.g. "Empty state", "Error state").
- `static` — static HTML for documentation, embedding Figma designs, etc.

Frames live inside named **Playground** pages, organised into groups that form
the sidebar tree.

### 2. Natural update loop

The previous approach scattered state management across `withMsg`, `withMsgF`,
`withUpdateF`, and a `Computed` variant. This is now consolidated into a single
`Control.withUpdate` modifier:

```elm
Control.withUpdate (\oldModel newModel -> ( clamp 0 100 newModel, [] ))
```

This receives the old and new model after any control interaction, and returns
the final model plus any side effects — a pattern that mirrors Elm's own
`update` function.

### 3. Reduce API complexity

The previous API exposed a large surface of overlapping functions (`withState`,
`withControl`, `withState_`, `withStateF`, `withStateF_`, `withUnlabelled`,
`withUnlabelled_`, `withComponent_`, `build`, `addVia`, `finish`, `finish_`,
etc). These are all replaced with a small set of **Control combinators**:

**Primitives:** `string`, `int`, `float`, `bool`, `identifier`, `withPresets`,
`fromLookup`, `componentRef`, `custom`

**Modifiers:** `withDefault`, `withDescription`, `hidden`, `withUpdate`

**Composition:** `builder`, `add`, `toControl`, `list`, `maybe`

Controls are composed using a builder pipeline where field order matches
constructor argument order:

```elm
Control.builder TextFieldModel
    |> Control.add "Value" .value Control.string
    |> Control.add "Label" .label Control.string
    |> Control.toControl
```

### The `_` suffix convention

Some controls store a different type than they produce. `componentRef` stores
an opaque `ComponentRef` but outputs rendered `Html`. `fromLookup` stores a
`String` key but outputs the looked-up value.

For these cases, the API provides `_` variants: `Control_`, `add_`,
`toControl_`, `component_`, `componentWithPortals_`. The `_` suffix
consistently means "storage and output types differ". This enables:

- **Component references** — embed one component inside another.
- **Named function sets** — store a `String` key, produce the corresponding
  function.
- **Sum types** — `addWhen` conditionally shows controls based on current
  state. Combined with `toControl_`, a discriminator field can switch which
  controls are visible while all fields remain in state.

## What Changed

### Components

Previously, components were opaque pipeline values built with `Component.new`
and a chain of `withControl`/`withState` calls. The view received curried
arguments positionally.

Now, components are built with `Component.component` (or `component_` for
mapped controls), taking a record of `{ id, name, controls, view }`. The view
receives a record model and a setter function. Components are opaque — the
record arg is validated and wrapped by the constructor.

### Controls

Previously, control primitives were mixed into the `Component` module alongside
builder, preview, and application functions. The `Block` and `Builder` types
were exposed and confusing.

Now, all control logic lives in `Component.Control`. The `Block` type is gone.
Controls are the single way to describe how values are stored, edited, and
displayed.

### Previews -> Frames + Playgrounds

Previously, `toPreview` produced a `Preview` value, and `group` took a plain
string name.

Now, presentation (`Frame`) is separated from organisation (`Playground`).
`group` takes `{ id, name }` for stable URL routing.

### Eliminated Functions

| Removed | Replaced by |
|---|---|
| `Component.new`, `withControl`, `withControl_` | `Component.component` record arg |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Control.add` + `Control.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF`, `withUpdateF` | `Control.withUpdate` |
| `withUnlabelled`, `withUnlabelled_`, `withInternalModel` | `Control.hidden` |
| `build`, `addVia`, `finish`, `finish_` | `Control.builder` + `Control.add` + `Control.toControl` |
| `toPreview`, `toPortalPreview` | `Component.explore` / `Component.example` |
| `Component.group "Name" [...]` | `Component.group { id, name } [...]` |
| `Preview`, `PreviewGroup` | `Playground`, `Frame` |
| `Block`, `BlockI`, `Builder` (public re-exports) | `Control`, `Builder` (in `Component.Control`) |
| `Component.withDefault` | `Control.withDefault` |
| `previewBlock`, `fromPreview` | `Control.componentRef`, `Component.toRef` |
| `Component.view` | View function passed directly to `component` record |

## Modules

```
Exposed:
  Component              — types, component/frame/playground constructors, toRef
  Component.Application  — browser runner (element, init, update, view)
  Component.Control      — control primitives, modifiers, and builder

Internal:
  Component.Internal     — type definitions (not exposed)
  Component.Ref          — reference/ID generation
  Component.Type         — runtime type representation
  Component.UI           — styled UI primitives
```

## Tests

A test suite has been added covering control primitives, builder composition,
list controls, and component lifecycle.

## Migration Quick Reference

```elm
-- Before
import Component

-- After
import Component
import Component.Control as Control

-- Before: pipeline builder
Component.new (\a b -> ...)
    |> Component.withControl "Label" Component.string "default"
    |> Component.toPreview { id = "x", name = "X" }

-- After: record + control builder
Component.component
    { id = "x"
    , name = "X"
    , controls =
        Control.builder MyModel
            |> Control.add "Label" .label Control.string
            |> Control.toControl
            |> Control.withDefault { label = "default" }
    , view = \model setter -> Html.text model.label
    }

-- Before: wrapping previews
Component.Application.element
    [ Component.group "Group" [ myPreview ] ]
    Nothing

-- After: frames inside playgrounds
Component.Application.element
    [ Component.group { id = "group", name = "Group" }
        [ Component.playground { id = "x", name = "X" }
            [ Component.explore myComponent
            , Component.example "Empty" emptyModel myComponent
            , Component.static (Html.text "Some docs")
            ]
        ]
    ]
    Nothing
```
