
# Default descriptions for controls

Addresses item 1 from [playground-polish](.plans/260403-1300-playground-polish.md).

## Problem

When a primitive control (e.g. `Controls.int`) is used directly as a component's
`controls`, the label shown in the UI is the component name. This is wrong —
the component name is a section heading for grouped controls, not a field label.

```elm
-- Currently shows "My Counter" as the field label. Weird.
{ id = "counter", name = "My Counter", controls = Controls.int, ... }
```

Inside `Controls.builder`/`add`, the label from `add` is passed directly to the
inner control's `controls` function, so that path is unaffected.

## Solution

Add `description : Maybe String` to `ControlsI_`. This is read only in
`makeComponentE`, which uses it in place of the component name when set.

```elm
-- makeComponentE, before:
b.controls component.name b.default

-- after:
b.controls (b.description |> Maybe.withDefault component.name) b.default
```

Primitives carry type-specific descriptions. Combinators that describe a set of
values take a required `String` description parameter. Builder groups leave it
`Nothing` so the component name flows through as the section heading.

## Type changes

### `Component/Internal.elm` — `ControlsI_`

```elm
type alias ControlsI_ e t i r a =
    { fromType : r -> i -> Lookup t -> i
    , toType : r -> List ( Ref, Type t )
    , controls : String -> r -> List (Lookup t -> Html (List ( Ref, Type t )))
    , default : i
    , map : Lookup t -> i -> a
    , update : i -> i -> ( i, List e )
    , description : Maybe String   -- NEW
    }
```

## Changes to `Controls.elm`

| Site | Change |
|---|---|
| `string` | `description = Just "Text"` |
| `stringEntry` config | add required `description : String` field |
| `stringEntry` result | `description = Just c.description` |
| `int` | pass `description = "Integer"` to `stringEntry` |
| `float` | pass `description = "Float"` to `stringEntry` |
| `withPresets` | new first param `String`; sets `description = Just desc` |
| `fromLookup` | new first param `String`; sets `description = Just desc` |
| `bool` | `withPresets "Boolean" ( True, "True" ) [ ( False, "False" ) ]` |
| `identifier` | `description = Nothing` |
| `custom` | `description = Nothing` |
| `componentRef` | `description = Nothing` |
| `builder` | `description = Nothing` |
| `toControls` | `description = Nothing` |
| `list` / `listMapped` inner | `description = Nothing` |
| new: `withDescription` | `String -> Controls e t m -> Controls e t m` — sets `description = Just desc` |

Modifiers `hidden`, `withUpdate`, `withDefault`, `withDefaultMapped` pass
`description` through unchanged.

## New public API

```elm
withDescription : String -> Controls e t m -> Controls e t m

withPresets : String -> ( a, String ) -> List ( a, String ) -> Controls e t a

fromLookup : String -> ( String, a ) -> List ( String, a ) -> Internal.Controls e t String a

stringEntry :
    { toString : a -> String
    , toType : a -> Type t
    , fromString : String -> Maybe a
    , fromType : Type t -> Maybe a
    , default : a
    , onError : String -> String
    , description : String        -- NEW (was not present)
    }
    -> Controls e t a
```
