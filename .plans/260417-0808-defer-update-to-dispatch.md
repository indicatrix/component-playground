
# Defer b.update from render to dispatch

**Date:** 260417-0808
**Status:** Not started.

---

## Problem

`makeComponentE.render` creates a `setter` closure that the component
view uses to produce messages for event handlers (e.g. `onClick`). The
setter currently calls `b.update instance currentState newState` eagerly
to compute effects:

```elm
setter newState =
    let
        ( finalState, effects ) =
            b.update instance currentState newState
    in
    Update (b.toType finalState) effects
```

In Elm, event handler message values are **eagerly evaluated** when the
virtual DOM is constructed. `Events.onClick msg` evaluates `msg` to
build the vDOM node, even though the message is only dispatched on
click. This means `b.update` runs on **every render**, not just on user
interaction.

Consequences:

1. **Unexpected side-effects during render.** `Debug.log` in
   `withUpdate` callbacks fires on every render cycle (auth ticks,
   debugger open/close, popover measurement ticks, etc.). An Elm
   programmer expects event handler construction to be pure — running
   `withUpdate` during render violates that expectation.

2. **Wasted work.** Effects (including `renderPortal` content closures)
   are created on every render and thrown away unless the user clicks.
   Cheap for simple callbacks but scales poorly with expensive
   `withUpdate` logic.

3. **Confusing debugging.** Logs from `withUpdate` appear to show
   duplicate or phantom state transitions that never actually dispatch,
   making it hard to distinguish real interactions from render noise.

## Fix

Move `b.update` out of the setter and into `Component.Application.update`,
so it only runs when a message is actually dispatched.

### Setter becomes pure

The setter should produce state changes only, no effects:

```elm
setter newState =
    Update (b.toType newState) []
```

This is what it was before the `withUpdate` integration. Event handler
message values are cheap to construct and have no side-effects during
render.

### Application.update calls b.update after applying state changes

`Component.Application.update` receives `ComponentUpdate (Update refs effects)`.
After applying the ref changes to `model.state`, it should:

1. Look up the component's `b.update` function
2. Compute old state (from the previous lookup) and new state (from the
   updated lookup)
3. Call `b.update instance oldState newState`
4. Apply any additional state changes from `b.update`'s returned state
5. Collect the returned effects

This requires storing enough information for Application.update to
find and call `b.update`. Options:

**Option A: Store update functions in Model.** During `init`, collect
a mapping from Ref ranges to `(ComponentInstance, update function)`
entries. When `Application.update` processes a `ComponentUpdate`,
determine which component's Refs were touched and call its update.

**Option B: Embed the update call in the Update message.** Instead of
`Update (List (Ref, Type t)) (List e)`, extend to carry an optional
deferred update:

```elm
type Update t e
    = Update (List (Ref, Type t)) (List e)
    | UpdateWithDeferred (List (Ref, Type t)) (Lookup t -> (List (Ref, Type t), List e))
```

The deferred function receives the post-change lookup and returns
additional state changes + effects. The setter constructs
`UpdateWithDeferred` instead of `Update`, and `Application.update`
calls the deferred function after applying the initial changes.

**Option C: Encode the update in the ComponentE.** Add an `update`
field to `ComponentE` alongside `render` and `controls`:

```elm
type alias ComponentE e t =
    { render : Lookup t -> View (Update t e)
    , controls : Theme -> Lookup t -> List (Html (Update t e))
    , update : Lookup t -> Lookup t -> (List (Ref, Type t), List e)
    }
```

`Application.update` calls `componentE.update oldLookup newLookup`
after applying state changes. This avoids extending the `Update` type.
Requires mapping from changed Refs to the owning ComponentE.

### wrapControl stays as-is

`wrapControl` already calls `b.update` at the right time (when control
panel events fire). It can stay unchanged — it intercepts control panel
HTML events, which are dispatched normally.

## Not changing

- `wrapControl` — already correct for controls panel path.
- `withUpdate` public API — signature stays the same.
- `renderPortal` — unaffected by where `b.update` is called.

## Notes

The double-dispatch issue reported during testing (two messages per
click) may or may not be related to this. It could also be caused by
event bubbling in the popover system's global click handler. Fixing the
render-time evaluation issue will make it easier to diagnose by removing
the render noise from logs.
