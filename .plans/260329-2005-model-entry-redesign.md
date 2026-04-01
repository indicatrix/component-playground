# Model + Entry Redesign

## Testing

### Current issues

- Elements don't update any more. See Combination Element Component. Seems to have been introduced in 831cb1c981278631619cfdf06ccff2b8725ecd1f Add effectful updates to blocks.
- List labels are broken (0, 1, 2, 0, 1). See List test Component. Introduced in 03e73f32113fceb64c03efb89d349892df6d6479 (Push String label into controls field of BlockI_)

## Decision

Replace the current `Component`/`Block`/`Builder` layering with two orthogonal
concepts matching the standard Elm MVU framing:

- **`Model e t m`** — describes a value of type `m`: how to store, retrieve,
  and render it as interactive controls, and optionally how to update it.
- **`Entry e t`** — a named registration of a view function + model + stories.

An Entry is the composition of **Model + View + Stories**.

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
write back into `m`. This is standard Elm record update syntax. The
getter/setter pair is effectively a lens; worth evaluating whether a combined
`( m -> a, a -> m -> m )` argument reads better than two separate args.

The `Builder` type and all its machinery (`build`, `addVia`, `finish`,
`finish_`, `finishI`) are removed. `Model.field` does the same accumulation
directly on `Model e t m`.

### 2. Introduce `Entry`, retire `toPreview`/`toPortalPreview`/`Component.new`

```elm
Entry.entry
    : { id : String, name : String }
    -> (m -> Html (Update t e))
    -> Model e t m
    -> List ( String, m )   -- stories: named initial states
    -> Entry e t

Entry.portal
    : { id : String, name : String }
    -> (m -> View (Update t e))
    -> Model e t m
    -> List ( String, m )
    -> Entry e t
```

Stories are `List ( String, m )` — named initial states converted to
`List (Ref, Type t)` at registration time using the model's `toType`. No Ref
exposure to users. All stories are interactive: the model's update loop (if
any) applies to all of them.

`Entry.entry` constructs the existing `Component_` record directly from
`(m -> view, Model e t m, List (String, m))` — same internal representation,
different construction path.

### 3. Retire `withControl`/`withState`/`withStateF` family

All `Component.new f |> withControl ... |> withState ...` call sites become
`Entry.entry view model stories`. The `withControl`/`withState`/`withStateF`/
`withMsg`/`withUpdateF` family and `Component.new` are removed.

The distinction between "control" (value only) and "state" (value + setter) is
eliminated: the view receives `m` directly and the update loop handles changes.

### 4. `Model.withUpdate` for the update loop

Replaces `withState`/`withMsg`/`withStateF` for components with internal
behaviour (toggles, accordions, etc.).

```elm
Model.withUpdate : (msg -> m -> ( m, List e )) -> Model e t m -> Model e t m
```

See open question 1 below regarding the `msg` type variable.

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
Model            -- public model combinators (new)
Entry            -- component registration (new)
Playground       -- group helper (rename from Component.group)
Application      -- runner (largely unchanged)
```

`Component` module removed. `Component.Application` → `Application` (or kept
as `Component.Application` with a deprecation note for back-compat).

`elm.json` exposed-modules updated accordingly.

## API Shape (target)

```elm
-- Primitives
Model.string  : Model e t String
Model.float   : Model e t Float
Model.int     : Model e t Int
Model.bool    : Model e t Bool

-- Record composition
Model.record : m -> Model e t m
Model.field  : String -> (m -> a) -> (a -> m -> m) -> Model e t a -> Model e t m -> Model e t m

-- Modifiers
Model.hidden     : Model e t m -> Model e t m
Model.withPresets : ( m, String ) -> List ( m, String ) -> Model e t m -> Model e t m
Model.withUpdate : (msg -> m -> ( m, List e )) -> Model e t m -> Model e t m

-- Other combinators
Model.fromLookup : ( String, m ) -> List ( String, m ) -> Model e t m
Model.custom     : (t -> Maybe m) -> (m -> t) -> m -> Model e t m
Model.list       : Model e t m -> Model e t (List m)
Model.preview    : Model e t (Html (Update t e))

-- Entry
Entry.entry  : { id : String, name : String } -> (m -> Html (Update t e)) -> Model e t m -> List ( String, m ) -> Entry e t
Entry.portal : { id : String, name : String } -> (m -> View (Update t e)) -> Model e t m -> List ( String, m ) -> Entry e t

-- Playground
Playground.group : String -> List (Entry e t) -> EntryGroup e t
```

## Eliminations

| Current | Replaced by |
|---|---|
| `Component`, `Component_`, `Component.new` | Constructed internally by `Entry.entry` |
| `withControl`, `withControl_` | `Entry.entry view model stories` |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Model.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF`, `withUpdateF`, `Computed` | `Model.withUpdate` |
| `withUnlabelled`, `withUnlabelledState`, etc. | `Model.hidden` |
| `withComponent`, `withComponent_` | `Model.preview` (already redundant post-spike) |
| `list2` | `Model.list Model.preview` (already redundant post-spike) |
| `build`, `addVia`, `finish`, `finish_`, `Builder` | `Model.record` + `Model.field` |
| `toPreview`, `toPortalPreview` | `Entry.entry`, `Entry.portal` |
| `group` | `Playground.group` |
| `fromPreview` | `Entry.ref` or removed |
| `identifier` | `Model.identifier` or removed |

## Open Questions

1. **`Model.withUpdate` msg type** — `(msg -> m -> (m, List e))` requires `msg`
   as a type variable. Two options:
   - Add `msg` as a fourth type parameter: `Model e t msg m`. More explicit but
     heavier at call sites; most models have no update loop so `msg = Never`.
   - Encode the message as a closure: `Model.withUpdate` takes
     `m -> (m, List e)` directly (message already applied), and the view
     produces `Html (m -> (m, List e))`. Unusual Elm idiom but avoids the extra
     type param.
   Decision needed before implementing step 4.

2. **`Model.field` setter ergonomics** — the explicit `(a -> m -> m)` setter is
   correct but slightly verbose. Options: keep two args
   `(m -> a) (a -> m -> m)`, combine as a tuple/record, or explore whether a
   single `(m -> a)` accessor + Elm's anonymous record update syntax can work
   without a setter. Lean toward the explicit pair for now.

3. **`Model` namespace collision** — users will have their own `Model` types.
   The module must be used qualified. Document this clearly; suggest
   `import Model as CM` or similar in the migration guide.

4. **Back-compat** — breaking change, major version bump. No shim layer; clear
   migration guide is sufficient.
