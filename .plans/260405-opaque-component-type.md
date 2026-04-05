# Opaque Component type with smart constructors

**Item 2 from playground-polish.md**

## Goal

Make `Component` opaque with `component` / `componentWithPortals` constructors.
Remove `Component.view` helper — the common case (no portals) is handled by `component`.

## Changes

### src/Component.elm

- Change `Component` from type alias to opaque type: `type Component e t m msg = Component { ... }`
- Add `component : { id : String, name : String } -> Controls e t m -> (m -> (m -> msg) -> Html msg) -> Component e t m msg`
  — wraps Html into View internally (absorbs what `view` does now)
- Add `componentWithPortals : { id : String, name : String } -> Controls e t m -> (m -> (m -> msg) -> View msg) -> Component e t m msg`
  — for portal slots
- Remove `view` from exposed API
- Unwrap opaque type in `explore`, `example`, `toRef`, `makeComponentE`

### examples/src/Components.elm

Migrate from record literal + `Component.view` to:

```elm
textField =
    Component.component { id = "text-field", name = "Text field" }
        (Controls.builder TextFieldModel ...)
        (\model setter -> UI.textField { ... })
```

### tests/

Update any test code that constructs Component records directly.

## Notes

- Record-style `{ id, name }` for first arg, consistent with `playground`/`group`
- `View` type only matters if you use portals — most users won't need it
