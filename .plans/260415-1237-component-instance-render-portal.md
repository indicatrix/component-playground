
# ComponentInstance and renderPortal

**Date:** 260415-1237
**Status:** Implemented.

---

## Summary

Enable host applications to render a component's named portal content
outside the library's main `view` pipeline. The motivating use case is
a lazy popover system in a host app, where popover content is a
`model -> Maybe (Html msg)` closure that re-renders every frame from
the current model. The library needs to provide a way to produce portal
HTML on demand given the current state.

Two additions:

1. **`ComponentInstance`** — an opaque handle to a specific component
   instance (ComponentRef + Ref), threaded into `Control.withUpdate` so
   controls can reference their component's portals when emitting effects.

2. **`renderPortal`** — given the current `Model`, a `ComponentInstance`,
   and a portal name, re-renders that portal's HTML by looking up the
   component definition and running it from the instance's Ref root.

---

## Design decisions

### Update stays on Control, not Component

`Control.withUpdate` is where state changes are detected — a control
value change can invalidate state and require an effect (e.g. opening a
popover). That trigger must stay at the Control level.

The previous blocker was that Control didn't know which Component it
belonged to, so it couldn't reference portals. `ComponentInstance`
solves this: the library constructs it at frame processing time (where
the component identity is already known) and threads it through
`wrapControl` into `b.update`.

Signature change:

```elm
-- before
withUpdate : (m -> m -> (m, List e)) -> Control e t m -> Control e t m

-- after
withUpdate : (ComponentInstance -> m -> m -> (m, List e)) -> Control e t m -> Control e t m
```

Existing callers that don't need `ComponentInstance` just add `_` as the
first parameter.

### Store Library\_ in Model

`renderPortal` needs `lookupDef` to find the component factory at render
time. `Library_` was previously built during `init` but discarded. Now
stored in the `Model`.

### Re-run factory from Ref, don't cache ComponentE

A `ComponentE` has its Refs baked in from init time. If the same
component definition has multiple instances (e.g. via `Control.list`),
a cached `ComponentE` would produce wrong state lookups. Instead,
`renderPortal` re-runs the factory from the instance's Ref via
`Ref.from` — the same mechanism `Ref.nested` uses internally.

---

## Implementation

### `Component/Internal.elm`

- New type: `ComponentInstance = ComponentInstance ComponentRef Ref`
- `ControlI_.update` signature: `ComponentInstance -> state -> state -> (state, List e)`

### `Component/Frame.elm`

- `fromComponent` / `example`: expanded `Ref.nested` to capture the
  allocated Ref and construct `ComponentInstance (ComponentRef c.id) ref`.
- `makeComponentE`: accepts `ComponentInstance`, passes it to `wrapControl`.
- `wrapControl`: accepts `ComponentInstance`, passes it to `b.update`.

### `Component/Control.elm`

- `withUpdate`: signature takes `ComponentInstance` as first parameter.
- All default `update` lambdas in primitives: `\_ _ x -> (x, [])`.

### `Component/Application.elm`

- `Model t e` gains `library : Library_ e t` field.
- `init` stores the `Library_`.
- New function:

  ```elm
  renderPortal : Model t e -> ComponentInstance -> String -> Maybe (Html (Msg t e))
  ```

  Internally: extract ComponentRef/Ref → `lookupDef` → `Ref.from ref (factory lib)` →
  `componentE.render (lookupCurrent model)` → `Dict.get portalName portals` →
  `Html.map ComponentUpdate`.

### `Component.elm`

- Re-exports `ComponentInstance` type alias.

---

## Not changing

- Component constructors (`component`, `componentWithPortals`, etc.) — no
  new fields.
- `gallery` frames — don't go through `wrapControl`, don't need
  `ComponentInstance`.
- The main `view` function — still discards portal content
  (`Tuple.first`). Rendering portals in the main view is a separate
  concern.
