# Controls + Playground Redesign

This is effectively v1 — the only existing use will be migrated directly.
No back-compat layer needed.

## Testing

Previous regressions (element updates, list labels) fixed in 6ab1dbc.

## Decision

Replace the current `Component`/`Block`/`Builder` layering with three orthogonal
concepts:

- **`Controls e t m`** — describes a value of type `m`: how to store, retrieve,
  and render it as interactive controls, and optionally how to update it.
  Named `Controls` (not `Model`) to avoid collision with users' own `Model` modules.
- **`Component e t m msg`** — a named, self-contained component definition:
  controls, a view function, and an id/name for display.
- **`Playground e t`** — a recursive tree of named pages and groups, assembled
  from frames built from components.

## Current State

The spike is complete. The internals are in good shape for the collapse:

- `BlockI e t i a = Block (Library e t -> State Ref (BlockI_ e t i i a))`
- `Builder e t i r a = Builder (Library e t -> State Ref (BlockI_ e t i r a))`
- `BlockI_.controls : String -> r -> List (Lookup t -> Html ...)` — label is
  render-time, not baked into the block at construction time
- `previewBlock : BlockI e t ComponentRef (Html (Update t e))` — plain block,
  no Library arg needed
- `list previewBlock` works without `list2`
- `withComponent`/`withComponent_`/`list2` are redundant but still in the API
- `finish`/`finish_` no longer take a String; `addVia` takes `BlockI` directly;
  all primitive blocks are plain values

## Remaining Work

Steps in order:

### 1. Introduce `Controls.builder`/`Controls.add`/`Controls.toControls`, retire `build`/`addVia`/`finish_`

`Controls.builder` takes a constructor function; `Controls.add` takes a label,
getter, and inner controls. Field order must match constructor argument order.
`Controls.toControls` finalises the builder into a `Controls e t m`.

```elm
-- Before
build (\label value -> { label = label, value = value })
    |> addVia .label "Label" string
    |> addVia .value "Value" string
    |> finish_

-- After
Controls.builder (\label value -> { label = label, value = value })
    |> Controls.add "Label" .label Controls.string
    |> Controls.add "Value" .value Controls.string
    |> Controls.toControls
```

No default value required upfront — each inner `Controls` provides its own
default. Works with opaque types and sum types since the constructor function
handles reconstruction:

```elm
Controls.builder MyThing.create
    |> Controls.add "Label" MyThing.label Controls.string
    |> Controls.toControls
```

The `Builder` type and all its machinery (`build`, `addVia`, `finish`,
`finish_`, `finishI`) are removed. `Controls.add` does the same accumulation
directly.

### 2. Introduce `Component e t m msg`, retire `Component.new`/`withControl`/`withState` family

`Component` becomes a plain record, fully decoupled from the playground tree:

```elm
type alias Component e t m msg =
    { id : String
    , name : String
    , controls : Controls e t m
    , view : m -> (m -> msg) -> Html msg
    }
```

The view receives the current model and a setter callback `(m -> msg)`. This is
the standard controlled-component pattern: the view owns no internal message
type and calls the setter to emit model updates. The `msg` type parameter allows
`Component` to be used in any message context; the playground fixes it to
`Update t e` at frame-construction time.

All `Component.new f |> withControl ... |> withState ...` call sites become a
`Component` record literal. The `withControl`/`withState`/`withStateF`/
`withMsg`/`withUpdateF` family and `Component.new` are removed.

The distinction between "control" (value only) and "state" (value + setter) is
eliminated: the view receives `m` directly and the update loop (if any) handles
changes via `Controls.withUpdate`.

### 3. Introduce `Playground e t` + `Frame e t`, retire `toPreview`/`toPortalPreview`/`group`

```elm
-- Recursive tree type (opaque)
type Playground e t
    = Page  { id : String, name : String } (List (Frame e t))
    | Group { id : String, name : String } (List (Playground e t))

-- Constructors
Component.playground : { id : String, name : String } -> List (Frame e t) -> Playground e t
Component.group      : { id : String, name : String } -> List (Playground e t) -> Playground e t

-- Frame constructors
Component.explore : Component e t m msg -> Frame e t
Component.example : String -> m -> Component e t m msg -> Frame e t
Component.doco    : Html msg -> Frame e t
```

`playground` takes its own `id`/`name` independently of any component, so a
page can contain frames from multiple components (e.g. a table page showing
the table alongside its cell variants).

`explore` creates an interactive frame driven by the controls.
`example` pins a specific model value as a named variant and still shows
controls, using the given `m` as the initial state — all frames are interactive.
`doco` is a prose/HTML frame; it takes `Html msg` to align with the other
frame constructors.

By convention, component modules export a value named `playground`:

```elm
-- Ui/Button.elm
playground : Component.Playground () ()
playground =
    Component.playground { id = "button", name = "Button" }
        [ Component.explore button
        , Component.example "Disabled" { label = "Submit", disabled = True } button
        ]
```

### 4. `Controls.withUpdate` for the update loop

Replaces `withState`/`withMsg`/`withStateF` for components with internal
behaviour (toggles, accordions, etc.).

```elm
Controls.withUpdate : (m -> m -> ( m, List e )) -> Controls e t m -> Controls e t m
```

Takes the **old** model and the **new** model (post-user-interaction) and
returns the final model plus any side effects. The old model is available for
diffing. No `msg` type variable needed.

### 5. `Controls.hidden` for fields with no control UI

Replaces `withUnlabelled`/`withUnlabelled_`/`withInternalModel`:

```elm
Controls.hidden : Controls e t m -> Controls e t m
```

Strips controls but keeps `fromType`/`toType` so the field participates in
state serialisation.

### 6. Rename `Block`/`BlockI`/`Builder`/`BlockI_` → `Controls`/`ControlsI_`

Pure rename pass after the structural changes are in place:

- `Internal.BlockI` → `Internal.Controls`
- `Internal.BlockI_` → `Internal.ControlsI_`
- `Internal.Builder` → removed (collapsed into `Controls.add`)
- `Component.Block`/`Component.BlockI` re-exports → `Controls.Controls`

### 7. Module restructure

```
Controls               -- public controls combinators (new, replaces Block/Builder API)
Component              -- Component type + frame/playground constructors (renamed/slimmed)
Component.Application  -- runner (largely unchanged)
```

`elm.json` exposed-modules updated accordingly.

## API Shape (target)

```elm
-- Primitives
Controls.string : Controls e t String
Controls.float  : Controls e t Float
Controls.int    : Controls e t Int
Controls.bool   : Controls e t Bool

-- Record/constructor composition
Controls.builder    : (a -> ... -> m) -> Builder e t (a -> ... -> m) m
Controls.add        : String -> (m -> a) -> Controls e t a -> Builder e t ... -> Builder e t ...
Controls.toControls : Builder e t m m -> Controls e t m

-- Modifiers
Controls.hidden     : Controls e t m -> Controls e t m
Controls.withPresets : ( m, String ) -> List ( m, String ) -> Controls e t m -> Controls e t m
Controls.withUpdate : (m -> m -> ( m, List e )) -> Controls e t m -> Controls e t m

-- Other combinators
Controls.fromLookup : ( String, m ) -> List ( String, m ) -> Controls e t m
Controls.custom     : (t -> Maybe m) -> (m -> t) -> m -> Controls e t m
Controls.list       : Controls e t m -> Controls e t (List m)
Controls.preview    : Controls e t (Html (Update t e))

-- Component
type alias Component e t m msg =
    { id : String, name : String, controls : Controls e t m, view : m -> (m -> msg) -> Html msg }

-- Playground tree
type Playground e t

Component.playground : { id : String, name : String } -> List (Frame e t) -> Playground e t
Component.group      : { id : String, name : String } -> List (Playground e t) -> Playground e t

-- Frame constructors
Component.explore : Component e t m msg -> Frame e t
Component.example : String -> m -> Component e t m msg -> Frame e t
Component.doco    : Html msg -> Frame e t
```

## Eliminations

| Current | Replaced by |
|---|---|
| `Component_`, `Component.new` | `Component` record literal |
| `withControl`, `withControl_` | `Component` record + `Component.explore` |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Controls.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF`, `withUpdateF`, `Computed` | `Controls.withUpdate` |
| `withUnlabelled`, `withUnlabelledState`, etc. | `Controls.hidden` |
| `withComponent`, `withComponent_` | `Controls.preview` (already redundant post-spike) |
| `list2` | `Controls.list Controls.preview` (already redundant post-spike) |
| `build`, `addVia`, `finish`, `finish_`, `Builder` | `Controls.builder` + `Controls.add` + `Controls.toControls` |
| `toPreview`, `toPortalPreview` | `Component.explore`, `Component.example` |
| `group` | `Component.group` |
| `fromPreview` | `Component.example` with explicit model value, or removed |
| `identifier` | `Controls.identifier` or removed |
| `Preview`, `PreviewGroup` | `Playground`, `Frame` |
| `Block`, `BlockI`, `Builder` (public) | `Controls` |

## Open Questions

1. **`portal` support** — the old `toPortalPreview` allowed components to render
   into named portal slots. Needs a `Component.portal` equivalent or a portal
   variant of the `Component` type. Defer until the core API is stable.
