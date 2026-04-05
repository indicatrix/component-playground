# Control complexity & component constructor API

## Context

Two API design issues in the component playground:

1. `Component.component` takes 3 positional args — a record arg would give better compiler errors ("missing field 'controls'")
2. `Controls` module naming, organisation, and the `Mapped` variant question

Through discussion, issue 2 evolved into a broader redesign: replacing the `Mapped` variant API with a dual-record builder pattern that also unlocks sum type support and conditional control rendering.

---

## Issue 1: Record arg for `Component.component`

Currently (3 positional args):
```elm
component :
    { id : String, name : String }
    -> Controls e t m
    -> (m -> (m -> msg) -> Html msg)
    -> Component e t m msg
```

Proposed (single record):
```elm
component :
    { id : String
    , name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> Html msg
    }
    -> Component e t m msg
```

Same change for `componentWithPortals` (with `View msg` return type).

**Trade-off:** Slightly more boilerplate at call sites (field names), but much better compiler errors ("missing field 'controls'" instead of type mismatches). Worth it for a library API.

**Files:** `src/Component.elm`, plus all call sites in example components.

---

## Issue 2: Redesigning the Control API

### 2a. Rename `Controls` -> `Control`, module `Component.Control`

- Type: `Control e t m` (singular, like `Parser`, `Decoder`, etc.)
- Module: `Component.Control` (consistent with `Component.Type`, `Component.Ref`, etc.)
- Import pattern: `import Component.Control as Control`

**Files:** New `src/Component/Control.elm`, remove `src/Controls.elm`, update `src/Component.elm` re-exports, all call sites.

### 2b. The mapped control problem & solution

**Problem:** The current API has parallel "Mapped" variants (`addMapped`, `listMapped`, `withDefaultMapped`, `MappedControls` type) for controls where storage type `i` differs from output type `a`. These feel heavyweight and the types suggest `add`/`addMapped` are interchangeable when they're not (different signatures, different internal behaviour). This is a gotcha waiting to happen.

**Key insight:** Instead of mapping at the control level, map at the builder level. The builder constructs a _storage record_ `i` alongside a _mapping function_ `Lookup t -> i -> m` that converts storage to output. This approach:

1. Eliminates all `*Mapped` functions from the public API
2. Unifies `add`/`addMapped` into the same builder pipeline
3. Unlocks sum type support (the long-standing hard problem)
4. Enables conditional control rendering

### 2c. Proposed API

**Types:**

```elm
-- Simple: storage = output (covers 90% of use cases)
type alias Control e t m = Internal.Controls e t m m

-- General: storage and output may differ
type alias Control_ e t i a = Internal.Controls e t i a
```

**Builder functions:**

```elm
-- Start a builder with a constructor function
builder : i -> Builder e t i m

-- Add a simple field (storage = output)
add : String -> (m -> a) -> Control e t a -> Builder e t (a -> b) m -> Builder e t b m

-- Add a mapped field — constructor receives both storage value and its
-- mapping function (i -> a with Lookup baked in)
add_ : String -> (m -> i) -> Control_ e t i a -> Builder e t (i -> (i -> a) -> b) m -> Builder e t b m

-- Conditionally visible simple field
addWhen : (m -> Bool) -> String -> (m -> a) -> Control e t a -> Builder e t (a -> b) m -> Builder e t b m

-- Conditionally visible mapped field
addWhen_ : (m -> Bool) -> String -> (m -> i) -> Control_ e t i a -> Builder e t (i -> (i -> a) -> b) m -> Builder e t b m

-- Finalise: simple case (storage = output)
toControl : Builder e t m m -> Control e t m

-- Finalise: mapped case (constructor produced a (storage, mapping) pair)
toControl_ : Builder e t (i, Lookup t -> i -> m) i -> Control_ e t i m
```

**Modifiers (unified, general signatures):**

```elm
withDefault : i -> Control_ e t i a -> Control_ e t i a
list : Control_ e t i a -> Control_ e t (List i) (List a)
hidden : Control_ e t i a -> Control_ e t i a
withUpdate : (m -> m -> ( m, List e )) -> Control e t m -> Control e t m
withDescription : String -> Control_ e t i a -> Control_ e t i a
```

### 2d. `Component.toRef` should return opaque `ComponentRef`

Currently `toRef : Component e t m msg -> String`. Should be:

```elm
type ComponentRef  -- opaque, wraps String

toRef : Component e t m msg -> ComponentRef
```

`Control.componentRef` and `Control.withDefault` would work with `ComponentRef` instead of raw `String`, preventing misuse.

### 2e. `Component.component_` for mapped controls

When a component uses `Control_ e t i m` (storage differs from output), the view needs access to both:
- `m` (mapped output) — for rendering derived values like componentRef Html, sum type variants
- `i` (storage record) — for constructing updates via the setter

```elm
-- Simple (existing, storage = output):
component :
    { id : String, name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> Html msg
    }
    -> Component e t m msg

-- Mapped (new, storage ≠ output):
component_ :
    { id : String, name : String
    , controls : Control_ e t i m
    , view : i -> m -> (i -> msg) -> Html msg
    }
    -> Component_ e t i m msg
```

The view receives both `i` and `m`. For simple fields (same type in both records), the view reads from `m` and writes via the setter with `i`. For mapped fields (componentRef, sum type outputs), the view reads from `m` and doesn't write those fields.

Similarly, `componentWithPortals_` for the portals variant.

### 2f. Module organisation

```elm
module Component.Control exposing
    ( -- Types
      Control, Control_, Builder
      
      -- Constructors
    , string, int, float, bool
    , identifier, withPresets, fromLookup, custom
    , componentRef
    
      -- Builder
    , builder, add, add_, addWhen, addWhen_, toControl, toControl_
    , list
    
      -- Modifiers
    , withUpdate, hidden, withDefault, withDescription
    
      -- Lower-level
    , stringEntry
    )
```

---

## Examples

### Simple component (no change from today, just renamed)

```elm
textField =
    Component.component
        { id = "text-field"
        , name = "Text field"
        , controls =
            Control.builder TextFieldModel
                |> Control.add "Value" .value Control.string
                |> Control.add "Label" .label Control.string
                |> Control.toControl
        , view = \model setter -> ...
        }
```

### Component with componentRef (component_ + add_)

```elm
type alias ComboStorage =
    { title : String, refId : ComponentRef }

type alias ComboView =
    { title : String, element : Html (Component.Update CustomType Effect) }

combo =
    Component.component_
        { id = "combo"
        , name = "Combo"
        , controls =
            Control.builder
                (\title refId renderRef ->
                    ( { title = title, refId = refId }
                    , \lookup s -> { title = s.title, element = renderRef s.refId }
                    )
                )
                |> Control.add "Title" .title (Control.string |> Control.withDefault "Title")
                |> Control.add_ "Element" .refId Control.componentRef
                |> Control.toControl_
        , view = \storage model setter ->
            Html.div []
                [ Html.text model.title
                , model.element
                ]
        }
```

### Sum type with conditional rendering

```elm
type Thing = StringThing String | IntThing Int | BoolThing Bool

type alias ThingStorage =
    { branch : String, strVal : String, intVal : Int, boolVal : Bool }

thingControl : Control_ e t ThingStorage Thing
thingControl =
    Control.builder
        (\branch strVal intVal boolVal ->
            ( ThingStorage branch strVal intVal boolVal
            , \_ s ->
                case s.branch of
                    "string" -> StringThing s.strVal
                    "int" -> IntThing s.intVal
                    _ -> BoolThing s.boolVal
            )
        )
        |> Control.add "Type" .branch
            (Control.withPresets "Type"
                ( "string", "String" )
                [ ( "int", "Int" ), ( "bool", "Bool" ) ]
            )
        |> Control.addWhen (\s -> s.branch == "string") "Value" .strVal Control.string
        |> Control.addWhen (\s -> s.branch == "int") "Value" .intVal Control.int
        |> Control.addWhen (\s -> s.branch == "bool") "Value" .boolVal Control.bool
        |> Control.toControl_
```

### Maybe combinator

```elm
maybe : Control e t a -> Control_ e t { has : Bool, val : a } (Maybe a)
maybe inner =
    Control.builder
        (\has val ->
            ( { has = has, val = val }
            , \_ s -> if s.has then Just s.val else Nothing
            )
        )
        |> Control.add "Enabled" .has Control.bool
        |> Control.addWhen .has "Value" .val inner
        |> Control.toControl_
```

---

## Implementation notes

### How `addWhen` works

`ControlsI_.controls` already receives `r` (the full storage record, reconstructed from current Lookup state): `controls : Maybe String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))`. The `addWhen` implementation wraps this to return an empty list when the predicate is false:

```elm
controls outerLabel default =
    if predicate default then
        bF.controls outerLabel default ++ b1.controls (Just label) (getter default)
    else
        bF.controls outerLabel default
```

The `default` here is the `r` value reconstructed from Lookup — it reflects live state. When the branch dropdown changes, `r` updates, and the predicate re-evaluates. Reactive for free.

### How `toControl_` works

The builder's accumulated `default` is `(i, Lookup t -> i -> m)` — a pair of the storage record with default values and the mapping function assembled by the user's constructor. `toControl_` destructures this and wires:
- `fromType`, `toType`, `controls` — operate on the storage record `i` (first element)
- `map` — uses the mapping function (second element) to convert `i -> m` at render time

### How `add_` works

Like `add`, but consumes two constructor args instead of one. The builder feeds the control's storage value `i` AND the control's `map` function (partially applied with `Lookup t`) as `i -> a`. The constructor captures both, using the mapping function when assembling the `Lookup t -> i -> m` mapping.

### Reducing combinatorial blowout

The four builder functions (`add`, `add_`, `addWhen`, `addWhen_`) can share implementation:
- `add` = `addWhen (always True)` with simple control constraint
- `add_` = `addWhen_ (always True)`
- `addWhen` can be implemented in terms of `addWhen_` since simple controls have `map = always identity`

So only `addWhen_` needs a full implementation. The others are thin wrappers.

---

## Research: elm/parser precedent

elm/parser is the only core Elm package with a simple/advanced type split: `Parser` (simple alias) vs `Parser.Advanced` (general type). Functions are fully duplicated; you pick one module. This doesn't map to our case because `add` and `add_` get mixed in the same pipeline. One module with both is the right call.

---

## Migration path

1. Rename `Controls` -> `Component.Control`, `Controls` -> `Control` (mechanical)
2. Change `Component.component`/`componentWithPortals` to record arg
3. Unify modifiers to general signatures (`withDefault`, `list`, `hidden`)
4. Add `add_`, `addWhen`, `addWhen_`, `toControl_` to builder API
5. Add `Component.component_` (and `componentWithPortals_`)
6. Make `ComponentRef` opaque, update `toRef` and `componentRef`
7. Migrate existing `addMapped`/`listMapped`/`withDefaultMapped` call sites to new API
8. Remove old `*Mapped` functions
9. Build `maybe` combinator as proof-of-concept for sum type pattern
10. Add sum type example with conditional rendering

Steps 1-2 are independent mechanical changes, can land first.
Steps 3-6 are the core API redesign.
Steps 7-8 are cleanup.
Steps 9-10 are the payoff — new capabilities.

---

## Resolved decisions

1. **Naming:** `Control_` for the 4-param type — underscore suffix, consistent with `add_`, `toControl_`, `component_`.
2. **`addWhen_`:** Yes, needed. All four variants (`add`, `add_`, `addWhen`, `addWhen_`) exist, with `addWhen_` as the core implementation.
3. **`fromLookup`:** Returns `Control_` (storage = String key, output = looked-up value). Users combine with `add_`.
4. **Component view:** `component_` takes `view : i -> m -> (i -> msg) -> Html msg`. View gets both storage (for updates) and mapped output (for rendering). `component` keeps `view : m -> (m -> msg) -> Html msg` for the simple case.

## Resolved (remaining)

1. **`Component_` type:** Yes, separate type `Component_ e t i m msg` with alias `Component e t m msg = Component_ e t m m msg`. Same pattern as `Control`/`Control_`. The `explore`/`example` frame constructors need variants that work with both (or a general signature on `Component_`).
2. **4 component constructors:** Accepted — `component`, `component_`, `componentWithPortals`, `componentWithPortals_`.
3. **List of Control_:** Likely fine — `list : Control_ e t i a -> Control_ e t (List i) (List a)` should compose. To be validated during implementation.
