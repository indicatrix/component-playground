# Component Playground

An interactive component testing library for Elm (0.19.1). Published as `indicatrix/component-playground`.

## Module Structure

```
src/
├── Component.elm              # Public API - all functions live here
└── Component/
    ├── Application.elm        # Application runner & UI (exposed)
    ├── Internal.elm           # Type definitions only (not exposed)
    ├── Ref.elm                # Reference/ID generation
    ├── Type.elm               # Runtime type representation
    └── UI.elm                 # Styled UI primitives
```

**Exposed modules** (elm.json): `Component`, `Component.Application`

## Architecture

### Component.elm (Public API)

Contains all public functions for building components and blocks:

- **Component builders**: `new`, `withControl`, `withState`, `withMsg`, `withComponent`, etc.
- **Block builders**: `string`, `int`, `float`, `bool`, `oneOf`, `list`, `custom`, etc.
- **Preview helpers**: `toPreview`, `toPortalPreview`, `group`
- **Type re-exports**: All public types aliased from `Component.Internal`

### Component.Internal (Type Definitions)

Contains only type definitions to preserve invariants:

- `Component t msg a` - opaque component type
- `Component_` - internal component record
- `BlockI t i a` - block with input/output types
- `BlockI_` - internal block record
- `Builder t i r a` - block builder
- `Library t msg` - component library
- `Library_` - internal library record
- `Msg t msg` - component messages (SetState, Msg, Update)
- `Preview`, `PreviewGroup`, `View`, `Lookup`, `Meta`, `ComponentRef`

### Component.Application (Runner)

Application framework for running the playground:

- `element` - simple Browser.element setup
- `init`, `update`, `view` - for embedding in larger apps
- `updateAt` - programmatic state updates
- `library_` - internal function to build library from preview groups

### Supporting Modules

- **Component.Ref**: Reference/ID generation using `State Ref` monad
- **Component.Type**: Runtime type representation (`StringValue`, `IntValue`, `FloatValue`, `CustomValue`)
- **Component.UI**: Styled UI primitives (`vStack`, `hStack`, `button`, `textField`, `select`)

## Key Patterns

### Opaque Types with Internal Records

```elm
-- In Component.Internal
type Component t msg a = Component (Component_ t msg a)

type alias Component_ t msg a =
    { value : Library t msg -> Lookup t -> State Ref a
    , controls : Library t msg -> State Ref (List (Lookup t -> Html (List ( Ref, Type t ))))
    , reference : State Ref Ref
    }
```

### State Monad for Reference Generation

Uses `folkertdev/elm-state` for stable reference IDs:

```elm
Ref.take : State Ref Ref  -- Get next reference
Ref.from : Ref -> State Ref a -> a  -- Run from nested ref
```

### Block System

Blocks define how values are:
- Stored/retrieved from lookup (`fromType`, `toType`)
- Displayed as controls (`controls`)
- Mapped to output type (`map`)
