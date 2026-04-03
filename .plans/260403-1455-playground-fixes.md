# Playground Fixes — Regressions from v1 Redesign

Tracks the regressions logged in `260329-2005-model-entry-redesign.md` plus
related cleanup. The `feat-stories` branch is the reference for how things
worked before.

---

## 1. Fix compilation — broken `Application.elm`

The code doesn't compile. Three issues:

### 1a. Remove dead `library_` function (lines 232–250)

`library_` references undefined names (`groups`, `allPreviews`, `Component`)
and is never called. It's a leftover from the old `PreviewGroup` API. Delete it.

### 1b. Remove `processedTree` from `init` (line 226)

`init` sets `processedTree` on the model but the `Model` type only has
`pages : Dict String (List (ProcessedFrame e t))`. The `processPlayground`
call is correct but its result needs to be flattened into a `Dict` and stored
in `pages`.

Current (broken):
```elm
{ state = Dict.empty
, processedTree = processedTree
, currentPage = currentPage
, search = ""
}
```

Fix: flatten the `List (List (String, List (ProcessedFrame e t)))` from
`State.traverse (processPlayground ...)` into a `Dict`:
```elm
pages =
    processedTree
        |> List.concat
        |> Dict.fromList
```

### 1c. Sidebar tree — define `ProcessedPlayground` or change approach

`viewPlaygroundTree` (line 379) references a `ProcessedPlayground` type that
doesn't exist. The sidebar needs a tree structure for rendering groups with
collapsible children, but the model only stores a flat `pages` Dict.

**Options:**

A. **Add a `ProcessedPlayground` type and store the tree in the model** — a
   new union type mirroring `Playground` but with processed data:
   ```elm
   type ProcessedPlayground e t
       = ProcessedPage { id : String, name : String }
       | ProcessedGroup { id : String, name : String } (List (ProcessedPlayground e t))
   ```
   Store as `sidebarTree : List (ProcessedPlayground e t)` on the model.
   The page frames stay in the `pages` Dict for content rendering.

B. **Build the sidebar index directly from the `Playground` input** — keep
   just the metadata (ids/names) in a tree, no need for `ProcessedFrame` data
   in the sidebar. This is lighter and separates concerns.

**Recommendation: Option B.** The sidebar only needs `{id, name}` metadata
plus the group hierarchy. A single recursive type keeps it simple:
```elm
type Index = Index { id : String, name : String, children : List Index }
```
Pages are leaves (`children = []`), groups are nodes. Built from the raw
`Playground` list during `init`, stored on the model, and rendered in
`viewPlaygroundTree`. This avoids threading processed frame data through
the sidebar and avoids a pointless union type — the structure is already
enforced by how we build it from `Playground`.

---

## 2. Rename `FrameInternals` → `ComponentE`

`FrameInternals` is a Component with the `m` type erased (existential-style).
`ComponentE` is a clearer name. This is a pure rename across:

- `Component.Internal` (definition)
- `Component.elm` (usage in `makeFrameInternals`)
- `Component.Application` (usage in `ProcessedFrame`, `Library_`)

---

## 3. Clean up remaining `Block` references

The `Block` type alias and "block" naming are leftovers from pre-rename.

- **`Internal.elm:2,33-36`** — `Block` type alias. Evaluate if still needed;
  if so rename to something consistent (e.g. `SimpleControls`), or inline
  since it's just `Controls e t a a`.
- **`Controls.elm:7`** — `stringEntryBlock` export. Rename to
  `stringEntry` or `stringEntryControls`.
- **`Controls.elm:502-511`** — `stringEntryBlock` definition. Rename.
- **`Component.elm:246`** — doc comment "Wrap a block control". Update text.
- **Variable names** `blockF`, `blockState` in Controls.elm and Component.elm
  — rename to `controlsF`, `controlsState` or similar.

---

## 4. Re-add missing examples

From `feat-stories` Index.elm, these are missing:

### 4a. Test 2

Simple two-identifier test:
```elm
{ id = "test-2"
, name = "Test 2"
, controls =
    Controls.builder (\a b -> (a, b))
        |> Controls.add "Unlabelled 1" Tuple.first Controls.identifier
        |> Controls.add "Unlabelled 2" Tuple.second Controls.identifier
        |> Controls.toControls
, view =
    Component.view <|
        \(a, b) _ ->
            UI.vStack []
                [ Html.div [] [ UI.text [] [ Html.text a ] ]
                , Html.div [] [ UI.text [] [ Html.text b ] ]
                ]
}
```

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

## Proposed execution order

1. **Fix compilation** (§1a, §1b, §1c) — get the code building again
2. **Sidebar rendering** (§1c) — implement `SidebarItem` approach
3. **Rename FrameInternals → ComponentE** (§2)
4. **Clean up Block references** (§3)
5. **Re-add Test 2** (§4a) — simple, no new infra needed
6. **Restore previewBlock infra** (§5, §6) — the biggest piece
7. **Re-add Combination Element** (§4b) — depends on §6

Steps 1–5 are straightforward. Steps 6–7 are the core architectural work and
should be designed carefully before implementation.
