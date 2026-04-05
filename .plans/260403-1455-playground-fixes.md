# Playground Fixes — Regressions from v1 Redesign

Tracks the regressions logged in `260329-2005-model-entry-redesign.md` plus
related cleanup. The `feat-stories` branch is the reference for how things
worked before.

**Status: Complete.** All regressions resolved.

---

## 1. ~~Fix compilation — broken `Application.elm`~~ (done: 91bbefd)

- Deleted dead `library_` function
- Fixed `init` to flatten processed playgrounds into `pages : Dict`
- Added `type Index = Index { id : String, name : String, children : List Index }`
  to `Component.Internal`, stored on Model, rendered via `viewIndex`

## 2. ~~Rename `FrameInternals` → `ComponentE`~~ (done: 91bbefd)

Renamed across Internal, Component (`makeComponentE`), and Application.

## 3. ~~Clean up remaining `Block` references~~ (done: 91bbefd)

- Removed `Block` type alias from Internal
- Renamed `stringEntryBlock` → `stringEntry`
- Renamed `blockF` → `controlsF`, `blockState` → `controlsState`

## 4. ~~Re-add missing examples~~

- 4a. Test 2 (done: 91bbefd)
- 4b. Combination Element (done: 8578876)

## 5. ~~Restore component embedding + populate Library\_.lookup~~ (done: 8578876, 04ce6f2, 5d5683e)

### Design (as implemented)

- `Library_.lookupDef : String -> Maybe (Library e t -> State Ref (ComponentE e t))`
  keyed by **component id** (not page path). Avoids chicken-and-egg by
  storing unapplied functions that accept `Library` at render time.
- `Frame` variants carry `{ id, name }` from the Component record.
  `extractDefs` collects these with their defs; `library.index` is built
  from this list (component ids + display names).
- Default page selection uses `flattenIndex` (prefixed page paths) separately.

### New API surface

- `Controls.componentRef` — stores component id, renders dropdown + embedded
  component via `lookupDef` + `Ref.from slotRef`
- `Controls.addMapped` — like `add` but for `i ≠ a` controls (no getter;
  storage value reconstructed from refs)
- `Controls.listMapped` — `list` for mapped controls
- `Controls.withDefaultMapped` — `withDefault` for mapped controls
- `Component.toRef` — extracts `.id` for use as componentRef defaults

### Bug fixes along the way

- Index ids now include group prefix (a13f529)
- Text field error default fixed (75d71f1)
- Dropdown defaults added (06ff73d)
