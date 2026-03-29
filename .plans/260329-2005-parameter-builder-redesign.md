# Parameter + Entry Redesign

## Decision

Replace the current `Component`/`Block`/`Builder` layering with two orthogonal concepts:

- **`Parameter e t m`** — describes a value of type `m`: how to store, retrieve, and render it as interactive controls.
- **`Entry e t`** — a named registration of a view function + parameter, with optional stories and update loop.

`Component` is eliminated entirely. `Block`/`BlockI`/`Builder` collapse into `Parameter`.

## Motivation

The current API has three overlapping concepts (`Block`, `Builder`, `Component`)
that partially duplicate each other. The `Builder` (`build`/`addVia`/`finish`)
is already a mini component assembler — closing that gap and making the model
type `m` explicit throughout unlocks:

- Stories as plain record literals, no Ref exposure
- A standard Elm `update` loop instead of `withState`/`withMsg`/`withStateF` variants
- Library-dependent parameters (`preview`) composable with `list` and `record` without `list2` or `withComponent`
- Single level of combinators — no distinction between "block combinators" and "component combinators"

## API Shape

### Parameter

```elm
-- Primitives
Parameter.string  : Parameter e t String
Parameter.float   : Parameter e t Float
Parameter.int     : Parameter e t Int
Parameter.bool    : Parameter e t Bool

-- Presets (dropdown)
Parameter.withPresets : ( m, String ) -> List ( m, String ) -> Parameter e t m -> Parameter e t m

-- Lookup (key → value, for sum types with functions)
Parameter.fromLookup : ( String, m ) -> List ( String, m ) -> Parameter e t m

-- Custom (opaque, no control)
Parameter.custom : (t -> Maybe m) -> (m -> t) -> m -> Parameter e t m

-- Record composition
Parameter.record : m -> Parameter e t m
    |> Parameter.field "Label" .label Parameter.string
    |> Parameter.field "Disabled" .disabled Parameter.bool
-- produces Parameter e t { label : String, disabled : Bool }

-- List
Parameter.list : Parameter e t m -> Parameter e t (List m)

-- Embedded component preview (library-dependent, resolved at build time)
Parameter.preview : Parameter e t (Html (Update t e))

-- Add an update loop
Parameter.withUpdate : (i -> (i, List e)) -> ParameterI e t i m -> ParameterI e t i m
```

### Entry

```elm
Entry.entry
    : { id : String, name : String }
    -> (m -> Html (Update t e))
    -> Parameter e t m
    -> List ( String, m )       -- stories: named model snapshots
    -> Entry e t

Entry.portal
    : { id : String, name : String }
    -> (m -> View (Update t e))
    -> Parameter e t m
    -> List ( String, m )
    -> Entry e t
```

Stories are `List ( String, m )` — plain named record literals. Converted to
`List (Ref, Type t)` internally using the parameter's `toType` at registration
time. No Ref exposure to users.

### Playground / Application

```elm
-- Replaces List (PreviewGroup e t) input
Playground.group : String -> List (Entry e t) -> EntryGroup e t

-- Application unchanged in shape
Application.element : List (EntryGroup e t) -> Maybe Url -> Program ...
Application.init / update / view
```

## Internal Structure

### Parameter internals

`Parameter e t m` wraps the existing `BlockI_ e t i i a` machinery under a new
opaque type. The `record` combinator replaces `build`/`addVia`/`finish`. Key
addition: a second constructor for library-dependent parameters:

`ParameterI_` is the existing `BlockI_ e t i i a` record, renamed.

### record combinator

Replaces `build`/`addVia`/`finish`. Each `field` call accumulates:
- `fromType : m -> Lookup t -> m` (field reconstruction from stored state)
- `toType : m -> List (Ref, Type t)` (field serialisation)
- `controls : m -> List (Lookup t -> Html ...)` (field control UI)

At `field` call time, the accessor `(m -> field)` and the `ParameterI_` for the
field type are combined to produce a new `ParameterI_` for `m`. Same mechanism
as `addVia` today, but with `m` explicit and no separate `finish` step.

Not sure how to incorporate the Library into here.

### Entry internals

`Entry e t` is what `Preview e t` is today — `( Meta, Component e t (View
(Update t e)) )` — but constructed from `(m -> view, Parameter e t m, List
(String, m))` rather than from a `Component`. The `Component_` record (`value`,
`controls`, `reference`) is still the internal representation; `Entry.entry`
constructs it directly from the parameter.

Stories on `Entry` are converted at construction time:

```elm
-- pseudocode inside Entry.entry
stories =
    List.map
        (\( name, model ) ->
            { name = name
            , state = parameterI.toType model
            }
        )
        namedStories
```

and stored on `Component_` as `stories : List { name : String, state : List (Ref, Type t) }`.

## Eliminations

| Current | Replaced by |
|---|---|
| `Component`, `Component_` | Constructed internally by `Entry.entry` |
| `withControl`, `withControl_` | `Entry.entry view param` |
| `withState`, `withState_`, `withStateF`, `withStateF_` | `Entry.withUpdate` |
| `withMsg`, `withMsg2`, `withMsg3`, `withMsgF` | `Entry.withUpdate` |
| `withUpdateF`, `Computed` | `Entry.withUpdate` |
| `withUnlabelled`, `withUnlabelledState`, etc. | `Parameter.record` + hidden fields |
| `withComponent`, `withComponent_` | `Parameter.preview` (via `LibraryParameter`) |
| `list2` | `Parameter.list Parameter.preview` (resolves uniformly) |
| `build`, `addVia`, `finish`, `finish_` | `Parameter.record` + `Parameter.field` |
| `toPreview`, `toPortalPreview` | `Entry.entry`, `Entry.portal` |
| `group` | `Playground.group` |
| `fromPreview` | `Entry.ref` or removed |
| `identifier` | `Parameter.identifier` or removed |

## Exposed Modules (revised)

```
Parameter        -- public parameter combinators
Entry            -- component registration
Playground       -- group helper
Application      -- runner (largely unchanged)
```

`Component` module removed. `Component.Application` renamed to `Application` or kept as `Component.Application` for back-compat.

## Open Questions

1. **Hidden fields** — the current `withUnlabelled`/`withInternalParameter`
pattern (parameters with no control UI) should be expressible as
`Parameter.hidden someParam` — a combinator that strips the controls but keeps
`fromType`/`toType`. Confirm this covers all current use cases.

2. **`Parameter.preview`** — validate that previews can be represented by
   pushing Library references into thunks and validate that `Parameter.list
   Parameter.preview` resolves correctly.  This is the key spike needed before
   full implementation.

3. **Back-compat** — this is a breaking change (major version bump). No shim layer needed; just a clear migration guide.

## Spike Goal

Before full implementation, validate:
- `Parameter.list Parameter.preview` resolving `Library` correctly through `list`'s internal `State Ref` traversal
- `Entry.entry` constructing a valid `Component_` from `(m -> Html msg, Parameter e t m)`
- Stories converting `m` to `List (Ref, Type t)` and back without Ref leakage
