# Component Playground

An interactive component testing library for Elm (0.19.1). Published as `indicatrix/component-playground`.

## Module Structure

```
src/
├── Component.elm              # Component constructors, toRef, type re-exports
└── Component/
    ├── Application.elm        # Application runner & UI (exposed)
    ├── Application/
    │   └── Theme.elm          # Theme tokens (exposed)
    ├── Control.elm            # Control combinators & builder (exposed)
    ├── Frame.elm              # Frame constructors + wrap modifier (exposed)
    ├── Internal.elm           # Type definitions only (not exposed)
    ├── Playground.elm         # Playground constructors (exposed)
    ├── Ref.elm                # Reference/ID generation
    ├── Type.elm               # Runtime type representation
    └── Ui.elm                 # Styled UI primitives
```

**Exposed modules** (elm.json): `Component`, `Component.Application`, `Component.Application.Theme`, `Component.Control`, `Component.Frame`, `Component.Playground`

## Architecture

### Three-level structure

1. **Components** (`Component`, `Component_`) — a set of controls and a view function
2. **Frames** (`Component.Frame`) — how a component (or static content) is presented: `fromComponent` (interactive), `example` (pinned state), `gallery` (multi-variant), `static` (static HTML). Modified with `wrap`.
3. **Playgrounds** (`Component.Playground`) — named pages and groups forming a sidebar tree

### Component.elm

- **Component constructors**: `component`, `component_`, `componentWithPortals`, `componentWithPortals_`
- **References**: `toRef`
- **Type re-exports**: `Component`, `Component_`, `Control`, `Control_`, `ComponentRef`, `Update`, `View`

### Component.Frame

- **Constructors**: `fromComponent`, `example`, `gallery`, `static`
- **Modifier**: `wrap` — applies a `Html -> Html` wrapper around the rendered frame. Composes across all variants; on interactive frames it wraps only the component view, not the controls panel.
- **Type re-exports**: `Frame`, `Component_`, `Update`

### Component.Playground

- **Constructors**: `fromComponent` (sugar for single-component page), `fromFrames` (multi-frame page), `group`
- **Type re-exports**: `Playground`, `Component_`, `Frame`, `Update`

### Component.Control (Control combinators)

Controls describe how a value is stored, retrieved, and rendered as interactive UI.

- **Primitives**: `string`, `int`, `float`, `bool`, `identifier`, `withPresets`, `fromLookup`, `componentRef`, `stringEntry`, `custom`
- **Modifiers**: `withUpdate`, `hidden`, `withDefault`, `withDescription`
- **Composing types**: `builder`, `add`, `add_`, `addWhen`, `addWhen_`, `toControl`, `toControl_`, `list`, `maybe`

#### `Control` vs `Control_`

`Control e t m` is an alias for `Control_ e t m m` — storage and output are the same type. `Control_` is the general form where they differ (e.g. `componentRef` stores `ComponentRef`, outputs `Html`). Same pattern for `Component`/`Component_`.

The `_` suffix convention applies throughout: `add_`, `toControl_`, `component_`, `componentWithPortals_`.

### Component.Internal (Type definitions)

Contains only type definitions to preserve invariants:

- `Control e t i a` — opaque, wraps `Library -> State Ref (ControlI_ ...)`
- `ControlI_ e t i r a` — internal record: `fromType`, `toType`, `controls`, `default`, `map`, `update`, `description`
- `Builder e t i r a` — intermediate builder during record composition
- `Component_ e t i m msg` — opaque component record (constructor reachable from `Component`, `Component.Frame`, `Component.Playground`)
- `Update t e` — state changes + effects
- `ComponentE e t` — type-erased component (closures over allocated refs)
- `Frame e t` — `InteractiveFrame | ExampleFrame | StaticFrame | GalleryFrame`. All four variants store `Html (Update t e)` so `Frame.wrap` applies uniformly; static/gallery callers supply `Html (List e)` and the constructors map it up.
- `Playground e t msg` — `Page | Group`
- `Library e t` / `Library_` — navigation metadata for cross-component references
- `ComponentRef` — opaque component reference

### Component.Application (Runner)

- `element` — simple `Browser.element` setup
- `init`, `update`, `view` — for embedding in larger apps
- `fromEffect`, `fromPreviewUpdate` — message helpers

### Supporting Modules

- **Component.Ref**: Reference/ID generation using `State Ref` monad
- **Component.Type**: Runtime type representation (`StringValue`, `IntValue`, `FloatValue`, `CustomValue`)
- **Component.Ui**: Styled UI primitives (`vStack`, `hStack`, `button`, `textField`, `select`)

## Key Patterns

### State Monad for Reference Generation

Uses `folkertdev/elm-state` for stable reference IDs:

```elm
Ref.take : State Ref Ref       -- Get next reference
Ref.from : Ref -> State Ref a -> a   -- Run from nested ref
Ref.nested : State Ref a -> State Ref a  -- Isolate ref allocation
```

### Control system

Controls define how values are:
- Stored/retrieved from lookup (`fromType`, `toType`)
- Displayed as interactive controls (`controls`)
- Mapped from storage to output type (`map`)
- Transformed after state changes (`update`)

The builder pipeline composes controls for record types. `add` consumes one constructor arg per field. `add_` consumes one `{ state, toValue }` record arg for mapped controls.

### `addWhen` (conditional rendering)

`addWhen predicate label getter control` only renders the field's controls when the predicate is true for the current state. The field always participates in state storage regardless of visibility. Used for sum type support.

## Dev

- `nix develop` — enter dev shell
- `npx elm-test tests/` — run tests
- `npx elm-format --yes src/ tests/` — format
- `npx elm-review` — lint
- Examples live in `examples/src/`, compiled with `cd examples && npx elm make src/Index.elm`
