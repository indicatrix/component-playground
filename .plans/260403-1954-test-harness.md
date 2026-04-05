
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
- [x] Roundtrip: `toType "hello"` → `fromType` → `"hello"`
- [x] Default value is `"Value"`
- [x] `withDefault "custom"` overrides default
- [x] `fromType` with empty lookup returns default
- [x] Control renders text input with current value from lookup
- [x] Typing in text input produces `[(ref, StringValue "new text")]`

**Controls.int:**
- [x] Roundtrip: `toType 42` → `fromType` → `42`
- [x] Default value is `1`
- [x] Control renders text input showing `"42"` after roundtrip
- [ ] Typing `"7"` produces `[(stringRef, StringValue "7"), (valueRef, IntValue 7)]`

**Controls.float:**
- [x] Roundtrip: `toType 3.14` → `fromType` → `3.14`
- [x] Default value is `1.0`
- [ ] Typing invalid input produces error message, no value ref update

**Controls.bool:**
- [x] Roundtrip: `toType True` → `fromType` → `True`
- [x] Roundtrip: `toType False` → `fromType` → `False`
- [x] Default is `True` (first preset)
- [ ] Selecting "False" option produces `[(ref, IntValue 1)]`

**Controls.identifier:**
- [x] Value is a ref-derived string (stable, not "pending")
- [x] `toType` produces empty list (no serialisation)
- [x] `controls` produces empty list (no UI)

**Controls.withPresets:**
- [x] Roundtrip through index-based storage
- [x] Unknown value returns default
- [x] Select control lists all preset labels

**Controls.fromLookup:**
- [x] Roundtrip through string key storage
- [x] `map` produces the associated value, not the key

**Controls.hidden:**
- [x] `controls` returns empty list
- [x] `fromType`/`toType` still work

**Controls.withUpdate:**
- [x] `update` function is called with old and new values

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

- [x] Default is constructed from field defaults: `{ label = "Value", value = "Value" }`
- [x] `withDefault` overrides: `{ label = "Hello", value = "World" }`
- [x] Roundtrip: `toType { label = "a", value = "b" }` → `fromType` → `{ label = "a", value = "b" }`
- [x] Fields are independent: changing one doesn't affect the other
- [x] Mixed types: builder with string + int + bool fields
- [ ] `addMapped` field works alongside `add` fields

### Layer 3: List controls tests (`tests/ControlsListTests.elm`)

Lists are where ref allocation complexity peaks.

#### Test cases

- [x] Default list has 3 items (hardcoded in `listHelper`)
- [x] Roundtrip: serialize a 2-item list, read back, get 2 items
- [x] Item values survive roundtrip
- [x] `toType` includes length ref + per-item refs
- [ ] Adding an item (incrementing length ref) works
- [ ] Removing an item (decrementing length ref) works
- [ ] `listMapped` roundtrip with `fromLookup` (storage ≠ output type)

### Layer 4: Component lifecycle tests (`tests/ComponentTests.elm`)

Test the full init → update → view cycle through `Component.Application`.

#### Test cases

**Basic lifecycle:**
- [x] `init` with a single `explore` frame produces correct page and index
- [ ] After init, the component's default model is retrievable via lookup
- [x] Applying an `Update` changes the state dict correctly
- [ ] After update, `fromType` reads the new value

**Component embedding (highest bug density):**
- [ ] `componentRef` default is first available component (excluding self)
- [ ] Changing component ref dropdown updates stored id
- [x] Embedded component's controls are accessible
- [ ] Nested refs don't collide with parent refs
- [x] Self-referencing component is excluded from dropdown

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

1. ~~**Extract `tests/Components.elm`**~~ **DONE** — Component definitions
   extracted. Note: `examples/src/Components.elm` is a copy, not a symlink as
   originally planned. This works but means the two copies can drift.
2. ~~**`ControlsTestHelper.elm`**~~ **DONE** — `run`, `runWithLibrary`,
   `lookup`, `emptyLibrary` all implemented.
3. ~~**`ControlsTests.elm` — `Controls.string` roundtrip (red-green).**~~
   **DONE** — Text field ref-leak bug was fixed. String roundtrip, default,
   `withDefault`, empty lookup, HTML render, and typing event all tested.
4. ~~**`ControlsTests.elm` — remaining primitives + control HTML tests.**~~
   **DONE** — `int`, `float`, `bool`, `identifier`, `withPresets`,
   `fromLookup`, `hidden`, `withUpdate` all have roundtrip and/or HTML tests.
5. ~~**`ControlsBuilderTests.elm`**~~ **DONE** — Two-field, mixed-type,
   field-independence, and `withDefault` override tests.
6. ~~**`ControlsListTests.elm`**~~ **DONE** — Default list, roundtrips (0/2/5
   items), `toType` structure, `withDefault` override.
7. ~~**`ComponentTests.elm`**~~ **DONE** — init, ViewPage update,
   ComponentUpdate, comboElement rendering, self-exclusion from ref dropdown,
   nested controls rendering.

**All 71 tests pass.** All 7 original implementation steps are complete.

## Current status (2026-04-05)

### What's well covered
- All Controls primitives: string, int, float, bool, identifier, withPresets,
  fromLookup, hidden, withUpdate
- Builder composition: multi-field records, mixed types, independence, defaults
- List controls: default sizing, roundtrips, toType structure
- Component.Ref: sequential and nested ref generation
- Component.Application: init, page navigation, state updates, component
  embedding

### Duplication to clean up

1. **Roundtrip boilerplate** — `string`, `int`, `float`, `bool` each repeat
   the same default → roundtrip → empty-lookup pattern. Factor into a
   parameterised helper to reduce noise.
2. **Builder roundtrip in two places** — `ControlsTests.builderStringRoundtripTest`
   overlaps with `ControlsBuilderTests.twoStringFieldsTest`. Remove the one in
   `ControlsTests` since `ControlsBuilderTests` covers it more thoroughly.
3. **Application render tested twice** — both `ControlsTests.applicationRenderTest`
   and `ControlsTests.builderControlsHtmlTest` verify ref strings don't leak
   into inputs. Consolidate into a single test.

### Missing coverage — next steps

**Priority 1 — untested public API:**
- [ ] `Controls.custom` — zero tests. Roundtrip, `controls` returns empty
  list, `fromType`/`toType` with `CustomValue` wrapper.
- [ ] `Component.example` — example frames with pinned models have no tests.
- [ ] `Component.doco` — documentation frames have no tests.

**Priority 2 — partial coverage gaps:**
- [ ] `Component.Application.update` — `UpdateSearch` message is untested.
  Search filtering exists in the UI but has no test coverage.
- [ ] `Component.Application.toUrl` — URL generation/parsing untested.
- [ ] `Component.Application.fromEffect` — effect conversion untested.
- [ ] `Component.Application.element` — `Browser.element` integration untested.
- [ ] `Component.toComponentUpdate` — effect wrapping untested.
- [ ] `Component.Ref.withNestedRef` and `fromNested` — untested variants.
- [ ] `Controls.stringEntry` — only tested indirectly through int/float.
- [ ] `Controls.addMapped` — only tested indirectly through comboElement.

**Priority 3 — edge cases:**
- [ ] Deeply nested builder compositions (builder within builder within list).
- [ ] State persistence across multiple page navigations.
- [ ] `listMapped` with `fromLookup` — storage ≠ output type roundtrip
  (originally planned in Layer 3 but not implemented).
- [ ] Adding/removing list items (increment/decrement length ref) — originally
  planned but not implemented.

### Structural note

`examples/src/Components.elm` is a standalone copy rather than a symlink to
`tests/Components.elm` as originally planned. The two files can drift
independently. Consider either establishing the symlink or deciding that
separate component definitions for tests vs examples is intentional.

## Open questions

- Do we want fuzz tests for roundtrips (random strings/ints through
  `toType`/`fromType`)? The elm-dev skill says prefer plain tests with
  examples, and for this domain specific examples are probably more
  informative. Could add fuzz later for confidence.
- Should `Controls.custom` tests use a concrete type (e.g. `Json.Encode.Value`)
  or a simple wrapper? A simple `type alias Wrapper = { x : Int }` with
  manual encode/decode would exercise the API without pulling in extra deps.
