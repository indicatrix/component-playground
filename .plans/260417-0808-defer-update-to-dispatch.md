
# Defer b.update from render to dispatch

**Date:** 260417-0808
**Status:** Implemented.

---

## Problem

`makeComponentE.render` created a `setter` closure that the component
view uses to produce messages for event handlers (e.g. `onClick`). The
setter called `b.update instance currentState newState` eagerly to
compute effects:

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
click. This meant `b.update` ran on **every render**, not just on user
interaction.

Consequences:

1. **Unexpected side-effects during render.** `Debug.log` in
   `withUpdate` callbacks fired on every render cycle.
2. **Wasted work.** Effects (including `renderPortal` content closures)
   were created on every render and thrown away.
3. **Confusing debugging.** Logs from `withUpdate` appeared to show
   phantom state transitions that never actually dispatched.

## Fix

Moved `b.update` out of the setter and into `Component.Application.update`,
so it only runs when a message is actually dispatched.

### Update type carries ComponentInstance, drops `e`

```elm
type Update t
    = Update ComponentInstance (List ( Ref, Type t ))
```

The `e` type parameter drops off entirely — HTML produced by component
views is no longer parameterised around the effect type. Effects come
purely from `ComponentE.update` at dispatch time. The setter tags each
message with its `ComponentInstance` so `Application.update` knows which
ComponentE to dispatch to.

### ComponentE gains an `update` field

```elm
type alias ComponentE e t =
    { render : Lookup t -> View (Update t)
    , controls : Theme -> Lookup t -> List (Html (Update t))
    , update : Lookup t -> Lookup t -> ( List ( Ref, Type t ), List e )
    }
```

`makeComponentE` populates `update` from `b.update`, `b.fromType`,
`b.toType`, and `instance`. At dispatch time, it reconstructs old and
new states from the lookups and calls `b.update instance oldState newState`.

### Setter becomes pure

```elm
setter newState =
    Update instance (b.toType newState)
```

### wrapControl removed

Since Update no longer carries effects, wrapControl is gone. Controls
produce state changes with the ComponentInstance directly:

```elm
b.controls theme b.description currentState
    |> List.map (\ctrl ->
        ctrl lookup
            |> Html.map (\changes -> Update instance changes)
    )
```

`Application.update` calls `ComponentE.update` uniformly for both setter
and controls paths.

### Application.update dispatches via existing lookupDef

No new dict on the Model — uses existing `model.library.lookupDef` with
the same pattern as `renderPortal`:

```elm
ComponentUpdate (Internal.Update (ComponentInstance (ComponentRef componentId) ref) updates) ->
    let
        oldLookup = lookupCurrent model
        modelWithUpdates = applyUpdates updates model
        newLookup = lookupCurrent modelWithUpdates
    in
    case model.library.lookupDef componentId of
        Just factory ->
            let
                componentE =
                    State.finalValue ref (factory (Library componentId model.library))

                ( additionalUpdates, effects ) =
                    componentE.update oldLookup newLookup
            in
            ( applyUpdates additionalUpdates modelWithUpdates, effects )

        Nothing ->
            ( modelWithUpdates, [] )
```

### Static frames: truly static

`static : Html Never -> Frame e t` — static HTML must be genuinely
non-interactive. Use native HTML elements (links, iframes) for
interactivity. Internally wrapped via `Html.map never`.

### Gallery frames: sentinel instance

Galleries use a sentinel `ComponentInstance` for their no-op setter.
Messages from gallery HTML are dispatched but Application.update
silently no-ops on them (the changes list is always empty and the
ComponentE lookup succeeds with the real component's id, but the
`componentE.update` call receives identical old/new lookups since no
state changed).

### Removed unused exports

`fromEffect` and `fromPreviewUpdate` were removed from `Application`'s
exposing list — neither was used anywhere.

## Alternatives considered

- **Option A (store update functions in Model):** Rejected — requires
  maintaining a Ref-range-to-update-function registry, coupling things
  unnecessarily.
- **Option B (embed deferred fn in Update message):** Rejected — each
  message carries its own deferred computation, which is self-contained
  but less idiomatic Elm. Would keep `e` on the Update type.
- **Adding `Maybe ComponentRef` or a `DeferredUpdate` variant:** Rejected
  — always including the ComponentInstance is simpler.
- **Adding an `Effects` variant to Update** (for static/gallery pure-effect
  path): Rejected during implementation — static frames can be truly
  static (`Html Never`) and galleries use a sentinel instance. Dropping
  `e` from `Update` is cleaner.

## Files changed

- `Component/Internal.elm` — `Update` type, `ComponentE` gains `update` field
- `Component/Frame.elm` — pure setter, removed `wrapControl`, populated
  ComponentE.update, rewrote `static` for `Html Never`, rewrote `gallery`
  with sentinel instance
- `Component/Application.elm` — dispatch via `lookupDef`, removed
  `fromEffect`/`fromPreviewUpdate` exports
- `Component/Control.elm` — `componentRef`'s `unwrapUpdate` pattern match
- `Component/Component.elm` — `Update` re-export shape
- `Component/Playground.elm` — `Update` re-export shape

## Notes

Debug.log in `withUpdate` callbacks now only fires on actual dispatches,
not during every render cycle. This makes it much easier to diagnose
behavioural issues using log output.

---

# Follow-up: pass a setter to withUpdate

**Status:** Implemented.

## Problem

`Control.withUpdate` gives effect-producing callbacks access to
`ComponentInstance`, old state, and new state — but no way to construct
`Update t` messages. Effects that carry `msg`-typed callbacks (e.g.
`Popover.dropdown`'s `onClick : Bool -> Identifier -> Maybe msg`) have
nothing to return for "update state to X" because the caller can't build
a valid `Update t` without access to the underlying `b.toType`.

Concretely: a dropdown that wants to close on outside-click needs the
popover's `onClick` handler to dispatch `Update instance (b.toType { new | openState = Closed })`.
Currently the caller has to fall back to `\_ _ -> Nothing` and close
via some other path.

## Fix

Pass a setter into the `withUpdate` callback, mirroring the view's
setter in `makeComponentE.render`:

```elm
-- before
withUpdate : (ComponentInstance -> state -> state -> (state, List e)) -> Control_ e t state value -> Control_ e t state value

-- after
withUpdate : (ComponentInstance -> (state -> Update t) -> state -> state -> (state, List e)) -> Control_ e t state value -> Control_ e t state value
```

The library builds `setter = \s -> Update instance (b.toType s)` — same
shape as the view's setter. Callers can produce Update messages from
effect callbacks:

```elm
Popover.dropdown identifier placement
    (\isInside _ ->
        if isInside then Nothing
        else Just (setter { new | openState = Closed })
    )
    content
```

## Files changed

- `Component/Control.elm` — `withUpdate` signature, default update
  lambdas (add one more `_` parameter)
- `Component/Frame.elm` — `wrapControl`/`ComponentE.update` call site:
  build setter and pass it to `b.update`
