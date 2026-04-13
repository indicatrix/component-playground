# Plan: Split Frame and Playground into their own modules

**Date:** 260413-1700
**Status:** Decisions made; ready to implement

---

## Summary

Reorganise the public API by concept rather than cramming everything into `Component`. Extract `Component.Frame` and `Component.Playground` as new modules, and reshape the frame constructors into a combinator-style API with a `Frame.wrap` modifier instead of per-variant `Wrapped` suffix constructors.

This is a breaking change. The library is unpublished and the downstream consumers are all internal, so migration is handled by updating `examples/src/` in the same change.

---

## Motivation

- `Component.elm` currently exposes component constructors, frame constructors (`explore`, `example`, `static`, `exploreFrame`, `galleryFrame`), playground constructors (`playground`, `group`), `toRef`, and type re-exports. It has four distinct concerns in one module.
- Frame constructors are inconsistently named: `explore`/`example`/`static` have no suffix, `exploreFrame`/`galleryFrame` do. This shows up every time a new frame variant is added.
- Wrappers (`exploreFrame`) are a second copy of `explore` that only differs in a post-render step. That scales poorly — any future wrapped variant (`exampleFrame`, etc.) is another duplicated constructor.
- A `Frame.wrap` modifier collapses the wrapped/unwrapped pair into one constructor plus one combinator. It also composes (`|> wrap |> wrap`) and applies to any frame where wrapping is meaningful, including `example` (currently unsupported).

---

## Decisions made (recap of the design discussion)

- **Module layout**: split into `Component`, `Component.Frame`, `Component.Playground`, `Component.Control` (existing), `Component.Application` (existing).
- **Naming direction**: use `from*` consistently (`Frame.fromComponent`, `Playground.fromComponent`, `Playground.fromFrames`). Drop the `Frame` suffix from constructors now that they live in the `Frame` module.
- **Wrapping**: `Frame.wrap` modifier, not `Wrapped` suffix constructors. Unified so `wrap` applies consistently across all four frame variants — see "Unify the Html message type" below.
- **Unify the Html message type**: `Internal.Frame`'s `StaticFrame` and `GalleryFrame` currently carry `Html (List e)`. Normalise them to `Html (Update t e)` at construction time (the conversion `Html.map (\effects -> Update [] effects)` is lossless — a frame that only produces effects can always be expressed as one that produces `Update [] effects`). `Application.processFrame` already does this conversion lazily at render time; we just hoist it into the `Frame.static` / `Frame.gallery` constructors. Result: `Frame.wrap`'s signature is uniform, no no-op caveat, and `processFrame` simplifies.
- **`example` accepts `Component_`**: signature takes `i` (storage type) as the initial state, not `m`. This lets `example` work with mapped components the same way `fromComponent` does.
- **`Playground.fromComponent` still takes `{ id, name }`**: component ids often contain their own structure (e.g. `ui/tableview`), and groups prepend their own path segments to build page URLs (e.g. `layouts/table/ui/tableview`). Inheriting the component id verbatim would conflate component identity with page identity and produce confusing paths. Keep page identity explicit.
- **`group` lives in `Component.Playground`**, not `Component.Application`. Application stays focused on the runner.
- **Breaking changes are fine.** No compatibility shims, no deprecated re-exports.

---

## New API

### `Component` (slimmed)

Exports:

```elm
module Component exposing
    ( Component, Component_, ComponentRef, Control, Control_, Update, View
    , component, component_, componentWithPortals, componentWithPortals_
    , toRef
    )
```

No frame constructors, no playground constructors. `Frame` and `Playground` types are re-exported from their own modules (see below) — users import those directly.

### `Component.Frame` (new)

```elm
module Component.Frame exposing
    ( Frame
    , fromComponent, example, gallery, static
    , wrap
    )

fromComponent : Component_ e t i m (Update t e) -> Frame e t
example       : String -> i -> Component_ e t i m (Update t e) -> Frame e t
gallery       : String -> Component_ e t i m (Update t e) -> ((i -> Html (List e)) -> Html (List e)) -> Frame e t
static        : Html (List e) -> Frame e t
wrap          : (Html (Update t e) -> Html (Update t e)) -> Frame e t -> Frame e t
```

Notes:

- `example` takes `i`, not `m`. For a plain `Component e t m ...` (where `i == m`) this is the same value. For `Component_`, callers pass a storage-shape record.
- `wrap` works uniformly on all four variants because all variants store `Html (Update t e)` after the unification described above. Public signatures of `static`/`gallery` still take `Html (List e)` — that's the callers' natural type when they have no state to update — but the `Html.map` to `Html (Update t e)` happens inside the constructor before the value is stored in the `Frame` variant.
- Multiple `wrap` calls compose: the outer-most `wrap` is the outer-most layer in the DOM.

### `Component.Playground` (new)

```elm
module Component.Playground exposing
    ( Playground
    , fromComponent, fromFrames, group
    )

fromComponent : { id : String, name : String } -> Component_ e t i m (Update t e) -> Playground e t
fromFrames    : { id : String, name : String } -> List (Frame e t) -> Playground e t
group         : { id : String, name : String } -> List (Playground e t) -> Playground e t
```

- `fromComponent` is sugar for `fromFrames { id, name } [ Frame.fromComponent c ]`. Covers the single-component-per-page case that is 8/10 of the current example usage. `{ id, name }` is explicit rather than inherited: component ids often have their own structure (`ui/tableview`) and groups prepend path segments to produce page URLs — inheriting would create confusing nested paths like `layouts/table/ui/tableview`.
- `fromFrames` replaces the current `playground` constructor, renamed for consistency with `fromComponent`.
- `group` moves here unchanged.

### Import shape for a typical caller

```elm
import Component
import Component.Control as Control
import Component.Frame as Frame
import Component.Playground as Playground
import Component.Application
```

Callers that only render static pages can skip `Component.Control` and `Component.Frame`.

---

## Implementation

### 1. `Component/Internal.elm` — unify Html message type in Frame variants

Before the Frame module can be implemented cleanly, change `Internal.Frame` so every variant carries `Html (Update t e)`:

```elm
-- before
type Frame e t
    = InteractiveFrame { id, name } (Library e t -> State Ref (ComponentE e t))
    | ExampleFrame { id, name } String (Library e t -> State Ref (ComponentE e t))
    | StaticFrame (Html (List e))
    | GalleryFrame String (Library e t -> State Ref (Html (List e)))

-- after
type Frame e t
    = InteractiveFrame { id, name } (Library e t -> State Ref (ComponentE e t))
    | ExampleFrame { id, name } String (Library e t -> State Ref (ComponentE e t))
    | StaticFrame (Html (Update t e))
    | GalleryFrame String (Library e t -> State Ref (Html (Update t e)))
```

The `Html.map (\effects -> Update [] effects)` conversion already happens in `Application.processFrame` at [src/Component/Application.elm:274-283](src/Component/Application.elm#L274-L283); we're hoisting it into the constructors instead. `processFrame` simplifies: `StaticFrame html -> State.state (ProcessedStatic html)` and `GalleryFrame name f -> f lib |> State.map (ProcessedGallery name)`.

### 2. `Component/Frame.elm` (new file)

- Move `explore`, `example`, `static`, `galleryFrame`, `exploreFrame`, and the `makeComponentE`/`wrapControl` helpers from `Component.elm`.
- Rename: `explore` → `fromComponent`, `galleryFrame` → `gallery`, `exploreFrame` → delete (replaced by `fromComponent |> wrap ...`).
- Change `example`'s signature from `String -> m -> Component e t m ... -> Frame e t` to `String -> i -> Component_ e t i m ... -> Frame e t`. Implementation stays the same except `initialModel : i` replaces `initialModel : m` in the closure; `b.default` override becomes `{ b | default = initialModel }`.
- `Frame.static` public signature stays `Html (List e) -> Frame e t`; internally applies `Html.map (\es -> Update [] es)` before wrapping in `StaticFrame`.
- `Frame.gallery` public signature stays as today (callers return `Html (List e)` from the assemble callback); internally `Html.map`s the assembled result before wrapping in `GalleryFrame`.
- Implement `wrap` uniformly across all four variants:

  ```elm
  wrap : (Html (Update t e) -> Html (Update t e)) -> Frame e t -> Frame e t
  wrap f frame =
      case frame of
          InteractiveFrame meta build ->
              InteractiveFrame meta (build >> State.map (applyWrapper f))

          ExampleFrame meta name build ->
              ExampleFrame meta name (build >> State.map (applyWrapper f))

          StaticFrame html ->
              StaticFrame (f html)

          GalleryFrame name build ->
              GalleryFrame name (build >> State.map f)

  applyWrapper : (Html (Update t e) -> Html (Update t e)) -> ComponentE e t -> ComponentE e t
  applyWrapper f base =
      { base
          | render =
              \lookup ->
                  let
                      ( html, portals ) = base.render lookup
                  in
                  ( f html, portals )
      }
  ```

- Re-export `Frame` type alias from `Component.Internal`.

### 3. `Component/Playground.elm` (new file)

- Move `playground` (renamed `fromFrames`) and `group` from `Component.elm`.
- Implement `fromComponent`:

  ```elm
  fromComponent : { id : String, name : String } -> Component_ e t i m (Update t e) -> Playground e t
  fromComponent meta comp =
      fromFrames meta [ Frame.fromComponent comp ]
  ```

  No pattern-match on `Component_` needed — the page's `{ id, name }` is explicit, not inherited from the component.
- Re-export `Playground` type alias from `Component.Internal`.

### 4. `Component.elm` (slim down)

- Delete: `explore`, `example`, `static`, `exploreFrame`, `galleryFrame`, `playground`, `group`, `makeComponentE`, `wrapControl`.
- Move `Component_` definition to `Component.Internal` (see §1).
- Keep: `component`, `component_`, `componentWithPortals`, `componentWithPortals_`, `toRef`, type aliases.
- Update module docstring to reflect the reorganisation.

### 5. `Component/Application.elm` — simplify `processFrame`

After the Frame unification in §1, `processFrame` no longer needs `Html.map` conversions for static/gallery:

```elm
processFrame : Library e t -> Frame e t -> State Ref (ProcessedFrame e t)
processFrame lib frame =
    case frame of
        InteractiveFrame _ f ->
            State.map ProcessedInteractive (f lib)

        ExampleFrame _ name_ f ->
            State.map (ProcessedExample name_) (f lib)

        StaticFrame html ->
            State.state (ProcessedStatic html)

        GalleryFrame name f ->
            State.map (ProcessedGallery name) (f lib)
```

### 6. `examples/src/Index.elm`

Migrate call sites:

```elm
-- before
Component.group { id = "components", name = "Components" }
    [ Component.playground { id = "text-field", name = "Text field" }
        [ Component.explore Components.textField ]
    , ...
    ]

-- after
Playground.group { id = "components", name = "Components" }
    [ Playground.fromComponent { id = "text-field", name = "Text field" } Components.textField
    , ...
    ]
```

```elm
-- before
Component.playground { id = "explore-frame", name = "exploreFrame (with wrapper)" }
    [ Component.exploreFrame
        (\inner -> Html.div [...] [ inner ])
        Components.textField
    ]

-- after
Playground.fromFrames { id = "explore-frame", name = "fromComponent (with wrapper)" }
    [ Frame.fromComponent Components.textField
        |> Frame.wrap (\inner -> Html.div [...] [ inner ])
    ]
```

```elm
-- before
Component.galleryFrame "Text field states" Components.textField (\render -> ...)

-- after
Frame.gallery "Text field states" Components.textField (\render -> ...)
```

Also add a small example showing `Frame.wrap` on `Frame.example` to demonstrate the combinator composition that was impossible before.

### 7. Tests & review

- Run `npx elm-test tests/`.
- Run `npx elm-format --yes src/ tests/ examples/src/`.
- Run `npx elm-review` and `cd examples && npx elm make src/Index.elm` to confirm the downstream compiles.

---

## Files touched

| File | Change |
|------|--------|
| `src/Component.elm` | Slim down to component constructors + `toRef` + type re-exports |
| `src/Component/Frame.elm` | NEW — frame constructors + `wrap` modifier |
| `src/Component/Playground.elm` | NEW — `fromComponent`, `fromFrames`, `group` |
| `src/Component/Internal.elm` | Add `Component_` type; change `StaticFrame` / `GalleryFrame` payloads to `Html (Update t e)` |
| `src/Component/Application.elm` | Simplify `processFrame` — drop the now-redundant `Html.map` conversions |
| `elm.json` | Add `Component.Frame` and `Component.Playground` to `exposed-modules` |
| `examples/src/Index.elm` | Migrate all call sites to new modules + add `wrap` example |
| `MIGRATION.md` | Document the rename/move table |

---

## Not changing

- `Component.Control` — unchanged.
- `Component.Ref`, `Component.Type`, `Component.Ui` — unchanged.
- Component constructors themselves (`component`, `component_`, `componentWithPortals`, `componentWithPortals_`).
- Runtime semantics of any frame variant — the `Html (Update t e)` unification is a refactor of *where* the existing `Html.map` conversion happens, not *whether* it happens.

---

## Risks / things to watch

- **Moving `Component_` to `Internal`** — `Component.elm` currently defines `Component_` inline. Moving it to `Internal` changes whether the constructor is accessible; make sure `Component.elm`'s `Component_` becomes a `type alias` re-export and the constructor stays usable there. Same pattern as the existing `Frame`/`Playground` re-exports.
- **`Frame` type in `Internal.elm` is a breaking change for any other code reaching into `Internal`** — grep confirms only `Component.elm` (to be deleted) and `Component/Application.elm` (updated in §5) construct these variants. Worth re-grepping after the rename in case anything else has landed.
- **`elm.json` exposed-modules ordering** — keep the list alphabetical to minimise diff churn in future changes.
