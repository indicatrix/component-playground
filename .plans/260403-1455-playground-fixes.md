# Playground Fixes — Regressions from v1 Redesign

Tracks the regressions logged in `260329-2005-model-entry-redesign.md` plus
related cleanup. The `feat-stories` branch is the reference for how things
worked before.

---

## 1. ~~Fix compilation — broken `Application.elm`~~ (done: 91bbefd)

The code doesn't compile. Three issues:

- Deleted dead `library_` function
- Fixed `init` to flatten processed playgrounds into `pages : Dict`
- Added `type Index = Index { id : String, name : String, children : List Index }`
  to `Component.Internal`, stored on Model, rendered via `viewIndex`

---

## 2. ~~Rename `FrameInternals` → `ComponentE`~~ (done: 91bbefd)

Renamed across Internal, Component (`makeComponentE`), and Application.

---

## 3. ~~Clean up remaining `Block` references~~ (done: 91bbefd)

- Removed `Block` type alias from Internal
- Renamed `stringEntryBlock` → `stringEntry`
- Renamed `blockF` → `controlsF`, `blockState` → `controlsState`
- Updated doc comment in Component.elm

---

## 4. Re-add missing examples

### 4a. ~~Test 2~~ (done: 91bbefd)

### 4b. Combination Element

This is the big regression — it requires `previewBlock` / `ComponentRef`
infrastructure which was removed. See section 5.

---

## 5. Restore component embedding + populate Library\_.lookup

### Design

**Key insight:** `ComponentE` is post-allocation (refs baked in). A
`previewBlock` needs to dynamically render *different* components in the same
slot, so it needs the pre-allocation form. The inner function of
`InteractiveFrame` — `Library e t -> State Ref (ComponentE e t)` — is exactly
this. Ref scoping happens at render time via `Ref.from slotRef`.

No chicken-and-egg: feat-stories avoided it by storing unapplied functions
that *accept* `Library` as a parameter. The lookup is built before any frames
are processed, and `Library` is only passed in at render time.

### Type changes

```elm
-- Internal.elm
type alias Library_ e t =
    { index : List { id : String, name : String }
    , groups : List { name : String, pages : List { id : String, name : String } }
    , lookupDef : String -> Maybe (Library e t -> State Ref (ComponentE e t))
    }
```

`lookupDef` maps `"<pageId>/<componentId>"` → the unapplied frame function.
Replaces the current `lookup` field.

### Building the lookup (Application.elm)

Two-pass approach in `init`:

1. **Extract defs** — walk the `Playground` tree, collect
   `( prefixedId, Library e t -> State Ref (ComponentE e t) )` pairs from
   each `InteractiveFrame` and `ExampleFrame`. Build into a `Dict`.

   ```elm
   extractDefs :
       Maybe String
       -> List (Playground e t (Update t e))
       -> Dict String (Library e t -> State Ref (ComponentE e t))
   ```

2. **Build `Library_`** — `lookupDef = \id -> Dict.get id defs`. Also
   `index` and `groups` as before.

3. **Process frames** — `processPlayground` as now, but the `Library` it
   threads through carries the populated `lookupDef`.

### Controls.componentRef (new control in Controls.elm)

```elm
Controls.componentRef : Controls e t String (Html (Update t e))
```

A `Controls` with `i = String` (component id stored in state) and
`a = Html (Update t e)` (rendered component output). Internal type is
`Internal.Controls e t String (Html (Update t e))`.

Behaviour:
- **`map`**: look up the id via `Library_.lookupDef`, apply `Library`,
  allocate refs via `Ref.from slotRef`, call `.render lookup`. Falls back
  to "Component not found" text.
- **`controls`**: render a dropdown of all components from `Library_.index`
  (excluding the current page to prevent recursion). Also render the
  embedded component's own controls inline.
- **`fromType`/`toType`**: store the id as a `StringValue`.
- **`default`**: first component id from `Library_.index`.

This is the equivalent of the old `previewBlock`. It receives the `Library`
via the standard `Controls (Library e t -> State Ref ...)` wrapper, so it
has access to `lookupDef` and the current page id.

### Component.toRef (helper in Component.elm)

```elm
Component.toRef : Component e t m msg -> String
```

Extracts `.id` from a component record. Used to provide default values for
`componentRef` controls (equivalent of old `fromPreview`). Lives in Component
so it can be called from other modules that have a Component but don't
import Controls.

### Rendering embedded components

When `previewBlock`'s `map` runs:
1. Get the stored component id from state
2. `lookupDef id` → `Maybe (Library e t -> State Ref (ComponentE e t))`
3. Apply `Library`, then `Ref.from slotRef` to scope the refs
4. Call `componentE.render lookup` to get the view HTML

The slot ref is the one allocated for this control instance — same ref that
stores the component id string. This means swapping components reuses the
same ref scope, so old state for a different component is harmless (just
ignored by the new component's `fromType`).

---

## Remaining work

### Step A — Type changes + lookup population
- Change `Library_` to use `lookupDef`
- Add `extractDefs` in Application.elm
- Wire into `init` (build defs first, then process frames)

### Step B — `Controls.componentRef` + `Component.toRef`
- Implement the control with map/controls/fromType/toType
- Add `Component.toRef` helper (extracts `.id` from a Component)

### Step C — Combination Element example (§4b)
- Re-add the example using `Controls.componentRef` and `Controls.list`
