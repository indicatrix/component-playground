# Model + Entry Redesign

## Decision

Replace the current `Component`/`Block`/`Builder` layering with two orthogonal concepts matching the standard Elm MVU framing:

- **`Model e t m`** — describes a value of type `m`: how to store, retrieve, render it as interactive controls, and optionally how to update it.
- **`Entry e t`** — a named registration of a view function + model + stories.

`Component` is eliminated entirely. `Block`/`BlockI`/`Builder` collapse into `Model`.

An Entry is the composition of **Model + View + Stories** — the three things an Elm programmer already knows.

## Prerequisite Spike: Library in BlockI_ Thunks

**This must land before entity collapse.**

Currently `withComponent`/`withComponent_` require special Component-level handling because `Library` is not available inside blocks. The spike adds a second constructor to `BlockI_`:

```
BlockI_ = PlainBlock ... | LibraryBlock (Library -> BlockI_ ...)
```

This implements `Model.preview` as a plain model combinator. Until the spike validates that `Model.list Model.preview` resolves Library correctly through `list`'s `State Ref` traversal — with no call-site wrapper — `Component` cannot be eliminated.

## Motivation

The current API has three overlapping concepts (`Block`, `Builder`, `Component`) that partially duplicate each other. The `Builder` (`build`/`addVia`/`finish`) is already a mini component assembler — closing that gap and making the model type `m` explicit throughout unlocks:

- The familiar Elm MVU framing at the Entry level: Model + View + Stories
- Stories as plain named initial states — no Ref exposure, always interactive
- A standard Elm `update` loop instead of `withState`/`withMsg`/`withStateF` variants
- Library-dependent models (`preview`) composable with `list` and `record` without `list2` or `withComponent`
- Single level of combinators — no distinction between "block combinators" and "component combinators"

## API Shape

### Model

```elm
-- Primitives
Model.string  : Model e t String
Model.float   : Model e t Float
Model.int     : Model e t Int
Model.bool    : Model e t Bool

-- Presets (dropdown)
Model.withPresets : ( m, String ) -> List ( m, String ) -> Model e t m -> Model e t m

-- Lookup (key → value, for sum types with functions)
Model.fromLookup : ( String, m ) -> List ( String, m ) -> Model e t m

-- Custom (opaque, no control)
Model.custom : (t -> Maybe m) -> (m -> t) -> m -> Model e t m

-- Record composition
Model.record { label = "", disabled = False }
    |> Model.field "Label" .label Model.string
    |> Model.field "Disabled" .disabled Model.bool
-- produces Model e t { label : String, disabled : Bool }

-- Hidden field (no control UI, but participates in fromType/toType)
Model.hidden : Model e t m -> Model e t m

-- List
Model.list : Model e t m -> Model e t (List m)

-- Embedded component preview (library-dependent, resolved via internal thunks)
Model.preview : Model e t (Html (Update t e))

-- Add an update loop
Model.withUpdate : (msg -> m -> ( m, List e )) -> Model e t m -> Model e t m
```

### Entry

```elm
Entry.entry
    : { id : String, name : String }
    -> (m -> Html (Update t e))
    -> Model e t m
    -> List ( String, m )       -- stories: named initial states
    -> Entry e t

Entry.portal
    : { id : String, name : String }
    -> (m -> View (Update t e))
    -> Model e t m
    -> List ( String, m )
    -> Entry e t
```

Stories are `List ( String, m )` — plain named initial states. Converted to
`List (Ref, Type t)` internally using the model's `toType` at registration
time. No Ref exposure to users.

All stories are interactive: the model's `withUpdate` loop (if any) applies to
all of them. A story without an update loop simply doesn't respond to messages.

### Playground / Application

```elm
-- Replaces List (PreviewGroup e t) input
Playground.group : String -> List (Entry e t) -> EntryGroup e t

-- Application unchanged in shape
Application.element : List (EntryGroup e t) -> Maybe Url -> Program ...
Application.init / update / view
```

## Internal Structure

### Model internals

`Model e t m` wraps the existing `BlockI_ e t i i a` machinery under a new
opaque type. Two constructors:

- `PlainModel (ModelI_ e t m)` — the existing `BlockI_` record, renamed
- `LibraryModel (Library -> ModelI_ e t m)` — for library-dependent models like `preview`

The `record` combinator replaces `build`/`addVia`/`finish`. Each `field` call accumulates:
- `fromType : m -> Lookup t -> m` (field reconstruction from stored state)
- `toType : m -> List (Ref, Type t)` (field serialisation)
- `controls : m -> List (Lookup t -> Html ...)` (field control UI)

At `field` call time, the accessor `(m -> field)` and the `ModelI_` for the
field type are combined to produce a new `ModelI_` for `m`. Same mechanism
as `addVia` today, but with `m` explicit and no separate `finish` step.

`Library` is resolved at Entry registration time: `Entry.entry` receives the
`Library` and uses it to evaluate any `LibraryModel` thunks in the tree before
constructing the `Component_` record.

### Entry internals

`Entry e t` is what `Preview e t` is today — `( Meta, Component e t (View (Update t e)) )` — but constructed from `(m -> view, Model e t m, List (String, m))` rather than from a `Component`. The `Component_` record (`value`, `controls`, `reference`) is still the internal representation; `Entry.entry` constructs it directly from the model.

Stories are converted at construction time:

```elm
-- pseudocode inside Entry.entry
stories =
    List.map
        (\( name, m ) ->
            { name = name
            , state = modelI.toType m
            }
        )
        namedStories
```

and stored on `Component_` as `stories : List { name : String, state : List (Ref, Type t) }`.

## Eliminations

| Current | Replaced by |
|---|---|
| `Component`, `Component_` | Constructed internally by `Entry.entry` |
| `withControl`, `withControl_` | `Entry.entry view model stories` |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Model.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF` | `Model.withUpdate` |
| `withUpdateF`, `Computed` | `Model.withUpdate` |
| `withUnlabelled`, `withUnlabelledState`, etc. | `Model.hidden` |
| `withComponent`, `withComponent_` | `Model.preview` (via `LibraryModel` thunk) |
| `list2` | `Model.list Model.preview` (resolves uniformly) |
| `build`, `addVia`, `finish`, `finish_` | `Model.record` + `Model.field` |
| `toPreview`, `toPortalPreview` | `Entry.entry`, `Entry.portal` |
| `group` | `Playground.group` |
| `fromPreview` | `Entry.ref` or removed |
| `identifier` | `Model.identifier` or removed |

## Exposed Modules (revised)

```
Model            -- public model combinators
Entry            -- component registration
Playground       -- group helper
Application      -- runner (largely unchanged)
```

`Component` module removed. `Component.Application` renamed to `Application` or kept as `Component.Application` for back-compat.

## Open Questions

1. **`Model.withUpdate` type** — the update loop introduces a `msg` type variable. Can it be kept existential inside `Model e t m`, or does `msg` need to be a fourth type parameter? The former is cleaner at call sites; the latter may be required by the Elm type system. Needs investigation during spike.

2. **`Model.preview`** — validate that the `LibraryModel (Library -> ModelI_ ...)` thunk constructor composes correctly with `Model.list` and `Model.record`. This is the key spike needed before full implementation.

3. **`Model` namespace collision** — users will have their own `Model` types. The module must be used qualified. Document this clearly; consider whether an alias like `import Model as Param` is worth suggesting.

4. **Back-compat** — this is a breaking change (major version bump). No shim layer needed; just a clear migration guide.

## Spike Goal

Before full implementation, validate in order:

1. **Library thunks** — add `LibraryModel (Library -> ModelI_)` constructor; implement `Model.preview` using it; confirm `Library` is resolved at `Entry.entry` call time with no leakage to call sites
2. **`Model.list Model.preview`** — confirm Library resolves correctly through `list`'s internal `State Ref` traversal
3. **`Entry.entry`** — construct a valid `Component_` from `(m -> Html msg, Model e t m, List (String, m))`
4. **Stories** — convert `m` to `List (Ref, Type t)` and back without Ref leakage
