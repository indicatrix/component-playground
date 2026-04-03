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

## 5. Restore component embedding (`previewBlock` / `ComponentRef`)

The ability to embed one component inside another via a dropdown selector was
the key feature lost in the redesign. In `feat-stories` this worked via:

1. **`ComponentRef`** — a `String` wrapper referencing a component by ID
2. **`previewBlock`** — a `Block` that stores a `ComponentRef` in the model
   and renders the referenced component by looking it up in the `Library`
3. **`withComponent_`** — composed blocks while threading `Library` through
4. **`fromPreview`** — extracted the ID from a Preview for use as a default
5. **`Library_.lookup`** — looked up a component by ID at render time

In the new API, the equivalent needs:

- A **`ComponentRef`** type (can remain `ComponentRef String` in Internal)
- A **`Controls.componentRef`** (or `Controls.preview`) control that:
  - Stores a `ComponentRef` as its model
  - Renders a dropdown of all available components from the `Library`
  - Looks up and renders the selected component via `Library_.lookup`
- **`Library_.lookup`** needs to return `Maybe (ComponentE e t)` (currently
  defined but may need adjustment)
- A way to get a `ComponentRef` from a component's ID (equivalent of
  `fromPreview`)

The `Library` is already threaded through `Frame` constructors
(`InteractiveFrame (Library e t -> State Ref ...)`) so `Controls` already
receive it — the plumbing exists.

**Key question:** In the new API, what's the equivalent of `fromPreview`?
Since components are now record literals (not opaque `Preview` values), we
just need `ComponentRef "some-id"` or a helper like
`Component.ref : Component e t m msg -> ComponentRef` that extracts `.id`.

---

## 6. Populate `Library_.lookup`

Currently `extractLibrary` only populates `index` and `groups` — `lookup` is
defined in the type but never populated (it would need `FrameInternals`/
`ComponentE` values which require `Ref` allocation).

The fix: during `init`, after processing all playgrounds into
`ProcessedFrame` values, build the lookup function from the processed
interactive frames and attach it to the `Library_`. This may require a
two-pass approach:
1. First pass: extract metadata for `index`/`groups`
2. Process frames (allocating Refs)
3. Build `lookup` from the processed results
4. **Problem**: `lookup` is needed *during* frame processing (for
   `previewBlock` to resolve references). This is the same chicken-and-egg
   that `feat-stories` solved by processing all components upfront into a
   `Dict` before any rendering.

**Resolution**: In `feat-stories`, `library_` built the full lookup from
`PreviewGroup` data before any frame was processed. We need a similar
approach: process all components' `Controls` to get their `ControlsI_`
records first (allocating Refs), build the lookup, *then* wrap them in frames.
Alternatively, use lazy evaluation / tie the knot (Elm doesn't support this
natively, but a Dict-based approach with stable Refs can work).

---

## Remaining work

- **§5 + §6** — Restore `previewBlock` / `ComponentRef` infrastructure and
  populate `Library_.lookup`. The core architectural piece.
- **§4b** — Re-add Combination Element example (depends on §5/§6).
