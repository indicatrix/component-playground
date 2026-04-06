# Component Playground

An interactive component testing library for Elm (0.19.1). Published as `indicatrix/component-playground`.

## Module Structure

```
src/
├── Component.elm              # Public API: component/frame/playground constructors
└── Component/
    ├── Application.elm        # Application runner & UI (exposed)
    ├── Control.elm            # Control combinators & builder (exposed)
    ├── Internal.elm           # Type definitions only (not exposed)
    ├── Ref.elm                # Reference/ID generation
    ├── Type.elm               # Runtime type representation
    └── UI.elm                 # Styled UI primitives
```

**Exposed modules** (elm.json): `Component`, `Component.Application`, `Component.Control`

## Architecture

### Three-level structure

1. **Components** (`Component`, `Component_`) — a set of controls and a view function
2. **Frames** (`Frame`) — how a component is presented: `explore` (interactive), `example` (pinned state), `doco` (static HTML)
3. **Playgrounds** (`Playground`) — named pages and groups forming a sidebar tree

### Component.elm (Public API)

- **Component constructors**: `component`, `component_`, `componentWithPortals`, `componentWithPortals_`
- **Frame constructors**: `explore`, `example`, `doco`
- **Playground constructors**: `playground`, `group`
- **References**: `toRef`
- **Type re-exports**: `Component`, `Component_`, `Control`, `Control_`, `ComponentRef`, `Frame`, `Playground`, `Update`, `View`

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
- `Update t e` — state changes + effects
- `ComponentE e t` — type-erased component (closures over allocated refs)
- `Frame e t msg` — `InteractiveFrame | ExampleFrame | DocoFrame`
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
- **Component.UI**: Styled UI primitives (`vStack`, `hStack`, `button`, `textField`, `select`)

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

The builder pipeline composes controls for record types. `add` consumes one constructor arg per field. `add_` consumes two (storage value + mapping function) for mapped controls.

### `addWhen` (conditional rendering)

`addWhen predicate label getter control` only renders the field's controls when the predicate is true for the current state. The field always participates in state storage regardless of visibility. Used for sum type support.

## Dev

- `nix develop` — enter dev shell
- `npx elm-test tests/` — run tests
- `npx elm-format --yes src/ tests/` — format
- `npx elm-review` — lint
- Examples live in `examples/src/`, compiled with `cd examples && npx elm make src/Index.elm`
