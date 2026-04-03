# Controls + Playground Redesign — v1

This was effectively a full rewrite of the public API. The only existing use
(the examples) was migrated directly. No back-compat layer was added.
See `MIGRATION.md` for how to update call sites.

---

## Decision

Replace the pipeline-builder `Component`/`Block`/`Builder` layering with three
orthogonal concepts:

- **`Controls e t m`** — describes a value of type `m`: how to store, retrieve,
  and render it as interactive controls, with an optional update hook. Lives in
  its own `Controls` module.
- **`Component e t m msg`** — a plain record: id, name, controls, and a view
  function. No opaque builder machinery.
- **`Playground e t msg`** — a recursive tree of named pages and groups,
  assembled from `Frame` values built from components.

---

## Implementation Record

### Step 1 — `Controls.builder`/`add`/`toControls` (8facce6)

Introduced the new record-composition API in a new `Controls` module.
`Controls.builder` takes a constructor; `Controls.add` takes label, getter, and
inner controls (field order must match constructor argument order);
`Controls.toControls` finalises into `Controls e t m`.

```elm
Controls.builder (\label value -> { label = label, value = value })
    |> Controls.add "Label" .label Controls.string
    |> Controls.add "Value" .value Controls.string
    |> Controls.toControls
```

The old `build`/`addVia`/`finish`/`finish_` were removed at this step. The
`Builder` internal type was retained as the accumulator for `Controls.add`
(exposed as `Controls.ControlsBuilder`).

---

### Steps 2 + 3 — `Component` record + `Frame`/`Playground` tree (828b232)

Implemented together because the types are mutually dependent.

**`Component e t m msg`** became a plain record:

```elm
type alias Component e t m msg =
    { id : String
    , name : String
    , controls : Controls e t m
    , view : m -> (m -> msg) -> View msg
    }
```

The view receives the whole model and a setter `(m -> msg)` instead of curried
individual arguments. `Component.view` lifts a plain `Html msg` function (no
portal slots).

**`Frame e t msg`** and **`Playground e t msg`** were added. Both carry a `msg`
type parameter so that `doco` can accept `Html msg` with a free message type.
Interactive constructors fix `msg` to `Update t e` at call sites:

```elm
Component.explore : Component e t m (Update t e) -> Frame e t (Update t e)
Component.example : String -> m -> Component e t m (Update t e) -> Frame e t (Update t e)
Component.doco    : Html msg -> Frame e t msg

Component.playground : { id : String, name : String } -> List (Frame e t msg) -> Playground e t msg
Component.group      : { id : String, name : String } -> List (Playground e t msg) -> Playground e t msg
```

`explore` creates a fully interactive frame. `example` pins an initial model as
a named variant but remains interactive (the given model overrides the controls'
default). `doco` is a static HTML frame; its free `msg` avoids a spurious type
constraint.

`Application.element` was updated to accept `List (Playground e t (Update t e))`,
build the `Library_` metadata from the tree upfront, and thread `Library pageId
library_` through each page's frame processing (so interactive frames can close
over the library rather than relying on a dummy value).

The `Component.new` / `withControl` / `withState` / `withMsg` / `withUpdateF`
pipeline and `toPreview`/`toPortalPreview` were all removed. `Index.elm` was
rewritten using the new record API.

---

### Step 4 — `Controls.withUpdate` (828b232)

```elm
Controls.withUpdate : (m -> m -> ( m, List e )) -> Controls e t m -> Controls e t m
```

Receives old model and new model (post-interaction); returns the final model
plus any side effects. Replaces `withMsg`, `withMsgF`, `withUpdateF`, and the
`Computed` update variant for components with internal behaviour.

---

### Step 5 — `Controls.hidden` (68ba611)

```elm
Controls.hidden : Controls e t m -> Controls e t m
```

Strips the controls UI while keeping `fromType`/`toType` so the field
participates in state serialisation. Replaces `withUnlabelled`/`withUnlabelled_`.

---

### Step 6 — Rename `BlockI`/`BlockI_` → `Controls`/`ControlsI_` (a0e11ce)

Pure rename pass in `Internal.elm`. The constructor was renamed to match the
type name (`type Controls e t i a = Controls (...)`), consistent with `Html`,
`Json.Decode.Decoder`, etc. All pattern matches and construction sites updated.

---

### Step 7 — Module restructure + API trim (af891cc)

Final public surface:

```
Controls               -- record composition + primitives + modifiers
Component              -- Component type alias + frame/playground constructors
Component.Application  -- browser runner (init / update / view / element)
```

`withDefault` moved from `Component` to `Controls`. Removed all leftover
re-exports and internal helpers that were no longer needed.

---

### PR Review Fixes (25a5554)

Applied reviewer comments from PR #1:

- **`Controls` constructor name**: confirmed `type Controls e t i a = Controls (...)`
  — type and constructor share the name (valid Elm, like `Html`). All
  `Block <| ...` and `(Block ...)` sites in `Controls.elm` renamed to `Controls`.
- **`controls` field doc comment**: restored verbose original comment on
  `ControlsI_.controls`.
- **`doco` message type**: `Frame` and `Playground` gained a `msg` type parameter
  so `doco : Html msg -> Frame e t msg` is well-typed with a free `msg`.
- **Library threading**: interactive frames now store
  `Library e t -> State Ref (FrameInternals e t)`. Application extracts
  `Library_` metadata (index + groups) from the raw tree before processing,
  then passes `Library pageId library_` when processing each page's frames.
- **`withDefault` in Controls**: `Controls.withDefault` implemented and exported.
- **`Controls.elm` symlink**: added to `examples/src/` alongside existing symlinks.

---

### Migration Guide (d2273f7)

`MIGRATION.md` added at the repo root. Covers all breaking changes with before/
after examples for each pattern.

---

## Final API

```elm
-- Controls module
Controls.string      : Controls e t String
Controls.float       : Controls e t Float
Controls.int         : Controls e t Int
Controls.bool        : Controls e t Bool
Controls.identifier  : Controls e t String
Controls.withPresets : ( m, String ) -> List ( m, String ) -> Controls e t m
Controls.fromLookup  : ( String, a ) -> List ( String, a ) -> Controls e t String a
Controls.custom      : (t -> Maybe a) -> (a -> t) -> a -> Controls e t a
Controls.list        : Controls e t m -> Controls e t (List m)
Controls.stringEntryBlock : { ... } -> Controls e t a

Controls.builder    : (a -> ... -> m) -> ControlsBuilder e t (a -> ... -> m) m
Controls.add        : String -> (m -> a) -> Controls e t a -> ControlsBuilder e t (a -> ...) m -> ControlsBuilder e t (...) m
Controls.toControls : ControlsBuilder e t m m -> Controls e t m

Controls.hidden     : Controls e t m -> Controls e t m
Controls.withUpdate : (m -> m -> ( m, List e )) -> Controls e t m -> Controls e t m
Controls.withDefault : m -> Controls e t m -> Controls e t m

-- Component module
type alias Component e t m msg =
    { id : String, name : String, controls : Controls e t m, view : m -> (m -> msg) -> View msg }

Component.view : (m -> (m -> msg) -> Html msg) -> (m -> (m -> msg) -> View msg)

Component.explore  : Component e t m (Update t e) -> Frame e t (Update t e)
Component.example  : String -> m -> Component e t m (Update t e) -> Frame e t (Update t e)
Component.doco     : Html msg -> Frame e t msg
Component.playground : { id : String, name : String } -> List (Frame e t msg) -> Playground e t msg
Component.group    : { id : String, name : String } -> List (Playground e t msg) -> Playground e t msg

-- Application module
Component.Application.element : List (Playground e t (Update t e)) -> Maybe Url -> ComponentPlayground t e
Component.Application.init    : List (Playground e t (Update t e)) -> Maybe Url -> Model t e
Component.Application.update  : Msg t e -> Model t e -> ( Model t e, List e )
Component.Application.view    : Model t e -> Html (Msg t e)
```

## Eliminations

| Removed | Replaced by |
|---|---|
| `Component.new`, `withControl`, `withControl_` | `Component` record literal |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Controls.add` + `Controls.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF`, `withUpdateF` | `Controls.withUpdate` |
| `withUnlabelled`, `withUnlabelled_`, `withInternalModel` | `Controls.hidden` |
| `withComponent`, `withComponent_`, `list2` | removed (previewBlock/combination element dropped) |
| `build`, `addVia`, `finish`, `finish_` | `Controls.builder` + `Controls.add` + `Controls.toControls` |
| `toPreview`, `toPortalPreview` | `Component.explore`, `Component.example` |
| `Component.group "Name" [previews]` | `Component.group { id, name } [Component.playground ... [frames]]` |
| `fromPreview` | `Component.example` with explicit model value |
| `Preview`, `PreviewGroup` | `Playground`, `Frame` |
| `Block`, `BlockI`, `Builder` (public re-exports) | `Controls.Controls`, `Controls.ControlsBuilder` |
| `Component.withDefault` | `Controls.withDefault` |
| `previewBlock`, combination elements | removed |
