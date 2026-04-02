# Model + Playground Redesign

## Testing

Previous regressions (element updates, list labels) fixed in 6ab1dbc.

## Decision

Replace the current `Component`/`Block`/`Builder` layering with three orthogonal
concepts:

- **`Model e t m`** — describes a value of type `m`: how to store, retrieve,
  and render it as interactive controls, and optionally how to update it.
- **`Component e t m msg`** — a named, self-contained component definition:
  a model, a view function, and an id/name for display.
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

### 1. Introduce `Model.record` + `Model.field`, retire `build`/`addVia`/`finish_`

`Model.record` takes the default value directly; `Model.field` takes a label,
getter, setter, and inner model:

```elm
-- Before
build (\label value -> { label = label, value = value })
    |> addVia .label "Label" string
    |> addVia .value "Value" string
    |> finish_

-- After
Model.record { label = "", value = "" }
    |> Model.field "Label" .label (\a m -> { m | label = a }) Model.string
    |> Model.field "Value" .value (\a m -> { m | value = a }) Model.string
```

The setter `(a -> m -> m)` is required because field reconstruction needs to
write back into `m`. The getter/setter pair is effectively a lens.

The `Builder` type and all its machinery (`build`, `addVia`, `finish`,
`finish_`, `finishI`) are removed. `Model.field` does the same accumulation
directly on `Model e t m`.

### 2. Introduce `Component e t m msg`, retire `Component.new`/`withControl`/`withState` family

`Component` becomes a plain record, fully decoupled from the playground tree:

```elm
type alias Component e t m msg =
    { id : String
    , name : String
    , model : Model e t m
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
changes via `Model.withUpdate`.

### 3. Introduce `Playground e t` + `Frame e t`, retire `toPreview`/`toPortalPreview`/`group`

```elm
-- Recursive tree type
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

`explore` creates an interactive frame driven by the model's controls.
`example` pins a specific model value as a named variant; it still shows
controls, using the given `m` as the initial state.
`doco` is a prose/HTML frame; it takes `Html msg` to align with the other frame
constructors.

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

### 4. `Model.withUpdate` for the update loop

Replaces `withState`/`withMsg`/`withStateF` for components with internal
behaviour (toggles, accordions, etc.).

```elm
Model.withUpdate : (m -> m -> ( m, List e )) -> Model e t m -> Model e t m
```

Takes the **old** model and the **new** model (post-user-interaction) and
returns the final model plus any side effects. The old model is available for
diffing. No `msg` type variable is needed: the update function compares model
values directly.

### 5. `Model.hidden` for fields with no control UI

Replaces `withUnlabelled`/`withUnlabelled_`/`withInternalModel`:

```elm
Model.hidden : Model e t m -> Model e t m
```

Strips controls but keeps `fromType`/`toType` so the field participates in
state serialisation.

### 6. Rename `Block`/`BlockI`/`Builder`/`BlockI_` → `Model`/`ModelI_`

Pure rename pass after the structural changes are in place:

- `Internal.BlockI` → `Internal.Model`
- `Internal.BlockI_` → `Internal.ModelI_`
- `Internal.Builder` → removed (collapsed into `Model.field`)
- `Component.Block`/`Component.BlockI` re-exports → `Model.Model`

### 7. Module restructure

```
Model                  -- public model combinators (new)
Component              -- Component type + frame/playground constructors (renamed/slimmed)
Component.Application  -- runner (largely unchanged)
```

`elm.json` exposed-modules updated accordingly.

## API Shape (target)

```elm
-- Primitives
Model.string : Model e t String
Model.float  : Model e t Float
Model.int    : Model e t Int
Model.bool   : Model e t Bool

-- Record composition
Model.record : m -> Model e t m
Model.field  : String -> (m -> a) -> (a -> m -> m) -> Model e t a -> Model e t m -> Model e t m

-- Modifiers
Model.hidden     : Model e t m -> Model e t m
Model.withPresets : ( m, String ) -> List ( m, String ) -> Model e t m -> Model e t m
Model.withUpdate : (m -> m -> ( m, List e )) -> Model e t m -> Model e t m

-- Other combinators
Model.fromLookup : ( String, m ) -> List ( String, m ) -> Model e t m
Model.custom     : (t -> Maybe m) -> (m -> t) -> m -> Model e t m
Model.list       : Model e t m -> Model e t (List m)
Model.preview    : Model e t (Html (Update t e))

-- Component
type alias Component e t m msg =
    { id : String, name : String, model : Model e t m, view : m -> (m -> msg) -> Html msg }

-- Playground tree
type Playground e t
    = Page  { id : String, name : String } (List (Frame e t))
    | Group { id : String, name : String } (List (Playground e t))

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
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Model.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF`, `withUpdateF`, `Computed` | `Model.withUpdate` |
| `withUnlabelled`, `withUnlabelledState`, etc. | `Model.hidden` |
| `withComponent`, `withComponent_` | `Model.preview` (already redundant post-spike) |
| `list2` | `Model.list Model.preview` (already redundant post-spike) |
| `build`, `addVia`, `finish`, `finish_`, `Builder` | `Model.record` + `Model.field` |
| `toPreview`, `toPortalPreview` | `Component.explore`, `Component.example` |
| `group` | `Component.group` |
| `fromPreview` | `Component.example` with explicit model value, or removed |
| `identifier` | `Model.identifier` or removed |
| `Preview`, `PreviewGroup` | `Playground`, `Frame` |

## Open Questions

1. **`Model.field` setter ergonomics** — the explicit `(a -> m -> m)` setter is
   correct but slightly verbose. Options: keep two args
   `(m -> a) (a -> m -> m)`, combine as a tuple/record, or explore whether a
   single `(m -> a)` accessor + Elm's anonymous record update syntax can work
   without a setter. Lean toward the explicit pair for now.

2. **`Model` namespace collision** — users will have their own `Model` types.
   The module must be used qualified. Document this clearly; suggest
   `import Model as CM` or similar in the migration guide.

3. **Back-compat** — breaking change, major version bump. No shim layer; clear
   migration guide is sufficient.

4. **`example` and controls** — does an `example` frame show controls (starting
   from the given `m`) or is it purely static? Leaning toward showing controls
   so all frames are interactive, consistent with `explore`.

5. **`portal` support** — the old `toPortalPreview` allowed components to render
   into named portal slots. Needs a `Component.portal` equivalent or a portal
   variant of the `Component` type. Defer until the core API is stable.
