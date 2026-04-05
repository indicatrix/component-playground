
# Test harness for the component playground

## Context

The component playground has no test coverage beyond `Component.RefTests`. The
recent v1 API redesign introduced regressions (text field showing ref values,
component embedding bugs) that were caught manually. We need a test harness that
catches these classes of bugs automatically.

**Known bug (red-green target):** The text field control displays a ref string
(e.g. `0.0`, `1.2.0.0`) instead of the actual string value. The control is
also non-editable — input stays as the ref. This is likely a ref allocation
mismatch between `toType` and `fromType`.

## Design principles

- **Unit tests over HTML tests.** The `Controls` internals (`fromType`,
  `toType`, `default`, `map`, `update`) are pure functions over refs and
  lookups. Test those directly via the State monad, same style as `RefTests`.
- **Test non-exposed modules directly.** `elm-test` compiles from
  `source-directories` — `Component.Internal`, `Component.Ref`,
  `Component.Type` are all importable despite not being in `exposed-modules`.
  This is already the pattern in `RefTests`.
- **Use existing example components.** The components in `examples/src/Index.elm`
  (`textField`, `dropdownInput`, `intInput`, `floatInput`, `identifierTest`,
  `test2`, `listTest`, `comboElement`) already cover the interesting control
  combinations. Import these directly in tests rather than creating separate
  test components — this keeps tests and examples in sync.
- **Red-green the text field bug.** First test written should reproduce the
  `Controls.string` ref leak, then we fix it.

## Test architecture

### Helper: unwrapping controls for inspection

Controls are opaque (`Controls (Library e t -> State Ref (ControlsI_ ...))`).
To test them we need to run the State monad and get the `ControlsI_` record.
We'll write a small test helper:

```elm
-- tests/ControlsTestHelper.elm
module ControlsTestHelper exposing (run, runWithLibrary, lookup)

import Component.Internal exposing (Controls(..), ControlsI_, Library(..), Lookup)
import Component.Ref as Ref
import Component.Type exposing (Type)
import Dict exposing (Dict)
import State

{-| Run controls with no library context, returning the ControlsI_ record.
    Sufficient for primitives that don't use the library (string, int, etc).
-}
run : Controls e t i a -> ControlsI_ e t i i a
run (Controls f) =
    f emptyLibrary |> Ref.fromTop

{-| Run controls with a provided library. Needed for componentRef tests.
-}
runWithLibrary : Library e t -> Controls e t i a -> ControlsI_ e t i i a
runWithLibrary lib (Controls f) =
    f lib |> Ref.fromTop

{-| Build a Lookup from a list of (Ref, Type) pairs — i.e. from toType output.
-}
lookup : List ( Ref.Ref, Type t ) -> Lookup t
lookup pairs ref =
    Dict.get (Ref.toString ref) (Dict.fromList (List.map (\( r, v ) -> ( Ref.toString r, v )) pairs))

emptyLibrary : Library e t
emptyLibrary =
    Library "" { index = [], groups = [], lookupDef = \_ -> Nothing }
```

This gives us direct access to `fromType`, `toType`, `controls`, `default`,
`map`, and `update` for any controls value.

### Layer 1: Controls unit tests (`tests/ControlsTests.elm`)

Two kinds of tests per control type:

**A. Serialisation roundtrips** — test `toType`/`fromType`/`default` directly:

```elm
-- Build controls, run via helper to get ControlsI_
-- Call toType with a known value → get [(ref, Type)]
-- Call fromType with that as the lookup → get the value back
-- Assert roundtrip equality
```

**B. Controls HTML + events** — exercise the `controls` field via `Test.Html`:

```elm
-- Call b.controls "Label" b.default to get Html-producing functions
-- Feed a lookup to get Html (List (Ref, Type t))
-- Use Test.Html.Query to find the input/select element
-- Use Test.Html.Event.simulate to fire an input event
-- Assert the message is the expected [(Ref, Type t)] pairs
```

The message type being `List (Ref, Type t)` rather than an opaque Msg is what
makes this pleasant — we assert on data directly.

#### Test cases

**Controls.string (RED-GREEN TARGET):**
- [ ] Roundtrip: `toType "hello"` → `fromType` → `"hello"`
- [ ] Default value is `"Value"`
- [ ] `withDefault "custom"` overrides default
- [ ] `fromType` with empty lookup returns default
- [ ] Control renders text input with current value from lookup
- [ ] Typing in text input produces `[(ref, StringValue "new text")]`

**Controls.int:**
- [ ] Roundtrip: `toType 42` → `fromType` → `42`
- [ ] Default value is `1`
- [ ] Control renders text input showing `"42"` after roundtrip
- [ ] Typing `"7"` produces `[(stringRef, StringValue "7"), (valueRef, IntValue 7)]`

**Controls.float:**
- [ ] Roundtrip: `toType 3.14` → `fromType` → `3.14`
- [ ] Default value is `1.0`
- [ ] Typing invalid input produces error message, no value ref update

**Controls.bool:**
- [ ] Roundtrip: `toType True` → `fromType` → `True`
- [ ] Roundtrip: `toType False` → `fromType` → `False`
- [ ] Default is `True` (first preset)
- [ ] Selecting "False" option produces `[(ref, IntValue 1)]`

**Controls.identifier:**
- [ ] Value is a ref-derived string (stable, not "pending")
- [ ] `toType` produces empty list (no serialisation)
- [ ] `controls` produces empty list (no UI)

**Controls.withPresets:**
- [ ] Roundtrip through index-based storage
- [ ] Unknown value returns default
- [ ] Select control lists all preset labels

**Controls.fromLookup:**
- [ ] Roundtrip through string key storage
- [ ] `map` produces the associated value, not the key

**Controls.hidden:**
- [ ] `controls` returns empty list
- [ ] `fromType`/`toType` still work

**Controls.withUpdate:**
- [ ] `update` function is called with old and new values

### Layer 2: Builder / record composition tests (`tests/ControlsBuilderTests.elm`)

Test that `builder`/`add`/`toControls` correctly composes multiple fields.

```elm
type alias TwoFields = { label : String, value : String }

twoFieldControls : Controls e t TwoFields
twoFieldControls =
    Controls.builder TwoFields
        |> Controls.add "Label" .label Controls.string
        |> Controls.add "Value" .value Controls.string
        |> Controls.toControls
```

#### Test cases

- [ ] Default is constructed from field defaults: `{ label = "Value", value = "Value" }`
- [ ] `withDefault` overrides: `{ label = "Hello", value = "World" }`
- [ ] Roundtrip: `toType { label = "a", value = "b" }` → `fromType` → `{ label = "a", value = "b" }`
- [ ] Fields are independent: changing one doesn't affect the other
- [ ] Mixed types: builder with string + int + bool fields
- [ ] `addMapped` field works alongside `add` fields

### Layer 3: List controls tests (`tests/ControlsListTests.elm`)

Lists are where ref allocation complexity peaks.

#### Test cases

- [ ] Default list has 3 items (hardcoded in `listHelper`)
- [ ] Roundtrip: serialize a 2-item list, read back, get 2 items
- [ ] Item values survive roundtrip
- [ ] `toType` includes length ref + per-item refs
- [ ] Adding an item (incrementing length ref) works
- [ ] Removing an item (decrementing length ref) works
- [ ] `listMapped` roundtrip with `fromLookup` (storage ≠ output type)

### Layer 4: Component lifecycle tests (`tests/ComponentTests.elm`)

Test the full init → update → view cycle through `Component.Application`.

#### Test cases

**Basic lifecycle:**
- [ ] `init` with a single `explore` frame produces correct page and index
- [ ] After init, the component's default model is retrievable via lookup
- [ ] Applying an `Update` changes the state dict correctly
- [ ] After update, `fromType` reads the new value

**Component embedding (highest bug density):**
- [ ] `componentRef` default is first available component (excluding self)
- [ ] Changing component ref dropdown updates stored id
- [ ] Embedded component's controls are accessible
- [ ] Nested refs don't collide with parent refs
- [ ] Self-referencing component is excluded from dropdown

### Test components

Extract the component definitions from `examples/src/Index.elm` into
`tests/Components.elm`. Symlink into `examples/src/` so `Index.elm` can
import them. Tests import `Components` directly (it's in the `tests/`
directory which `elm-test` includes).

This mirrors the existing symlink pattern: `examples/src/` already symlinks
`Component.elm`, `Component/`, and `Controls.elm` from `../../src/`.

| Component         | Controls exercised                                       |
|-------------------|----------------------------------------------------------|
| `textField`       | `builder` + `identifier` + 3x `string` + `withDefault`  |
| `dropdownInput`   | `builder` + `string` + `list` of nested builder          |
| `intInput`        | `int` + `withDefault`                                    |
| `floatInput`      | `float` + `withDefault`                                  |
| `identifierTest`  | `builder` + 3x `identifier`                              |
| `listTest`        | `list string` + `withDefault`                            |
| `comboElement`    | `builder` + `string` + `componentRef` + `listMapped componentRef` |

After the refactor, `Index.elm` becomes thin — just `import Components` and
wire up the playground tree in `main`.

## File structure

```
tests/
  Components.elm               -- extracted component definitions
  ControlsTestHelper.elm       -- helper to unwrap and run controls
  ControlsTests.elm            -- Layer 1: primitive roundtrips + control HTML
  ControlsBuilderTests.elm     -- Layer 2: record composition
  ControlsListTests.elm        -- Layer 3: list controls
  ComponentTests.elm           -- Layer 4: lifecycle + embedding
  Component/
    RefTests.elm               -- existing (unchanged)

examples/src/
  Components.elm -> ../../tests/Components.elm   -- symlink (new)
  Component.elm  -> ../../src/Component.elm      -- symlink (existing)
  Component/     -> ../../src/Component          -- symlink (existing)
  Controls.elm   -> ../../src/Controls.elm       -- symlink (existing)
  Index.elm                                      -- slimmed down, imports Components
```

## Implementation order

1. **Extract `tests/Components.elm`** — move component definitions out of
   `examples/src/Index.elm`. Symlink `examples/src/Components.elm` →
   `../../tests/Components.elm`. Slim down `Index.elm` to import + wire.
   Verify examples still compile (`npx elm make examples/src/Index.elm`).
2. **`ControlsTestHelper.elm`** — the `run`/`lookup` helper. Everything depends
   on this.
3. **`ControlsTests.elm` — `Controls.string` roundtrip (failing).** This is
   the red-green target for the text field bug. Write the test, watch it fail,
   diagnose the ref allocation issue, fix it, watch it pass.
4. **`ControlsTests.elm` — remaining primitives + control HTML tests.** `int`,
   `float`, `bool`, `identifier`, `withPresets`, `fromLookup`. Include
   `Test.Html` event tests alongside the roundtrip tests for each primitive.
5. **`ControlsBuilderTests.elm`** — multi-field record composition. Tests the
   `add` pipeline and `withDefault`. Use `textField` and `dropdownInput` from
   `Components` as realistic test subjects.
6. **`ControlsListTests.elm`** — list serialisation and ref nesting. Use
   `listTest` and `dropdownInput` (which has a nested list) from `Components`.
7. **`ComponentTests.elm`** — lifecycle and embedding. Use `comboElement` from
   `Components` which exercises `componentRef` and `listMapped componentRef`.
   Build a test playground from the `Components` definitions to test the full
   `init`/`update` cycle and `Library` lookup path.

## Open questions

- Do we want fuzz tests for roundtrips (random strings/ints through
  `toType`/`fromType`)? The elm-dev skill says prefer plain tests with
  examples, and for this domain specific examples are probably more
  informative. Could add fuzz later for confidence.
