# Migration Guide — v0 → v1

This guide covers the breaking changes introduced in v1 and shows how to update
existing code.

---

## Overview

The v1 redesign replaces the pipeline-builder approach with a simpler model:

- **Components** are plain records instead of opaque pipeline values.
- **Controls** live in a dedicated `Controls` module, separated from `Component`.
- **Views** receive a record model and a setter instead of curried arguments.
- **Previews** become **Frames** (`explore`, `example`, `doco`) inside named
  **Playground pages**.
- The `group` function now requires an `{ id, name }` record, and a new
  `playground` constructor wraps frames into named pages.

---

## Quick Reference

| v0 | v1 |
|----|----|
| `Component.new (\a b -> ...) \|> Component.withControl ... \|> Component.toPreview { id, name }` | `{ id, name, controls = Controls.builder ... \|> Controls.toControls, view = Component.view (\model setter -> ...) }` |
| `Component.string`, `Component.int`, etc. | `Controls.string`, `Controls.int`, etc. |
| `Component.build \|> Component.addVia ... \|> Component.finish_` | `Controls.builder \|> Controls.add ... \|> Controls.toControls` |
| `Component.withState "Label" block default` | `Controls.add "Label" .field block` |
| `Component.withControl "Label" block default` | `Controls.add "Label" .field block` |
| `Component.withUnlabelled_ block` | `Controls.add "" .field (Controls.hidden block)` or `Controls.add "..." .field block` |
| `Component.withMsg f` | `Controls.withUpdate (\_ new -> (f new, []))` |
| `Component.withDefault m block` | `Controls.withDefault m block` (now in `Controls`) |
| `Component.group "Name" [previews]` | `Component.group { id, name } [Component.playground ... [frames]]` |
| `Component.Application.element [previews] Nothing` | `Component.Application.element [playgrounds] Nothing` |
| `Component.previewBlock` | `Controls.componentRef` (use with `Controls.addMapped`) |
| `Component.list Component.previewBlock` | `Controls.listMapped Controls.componentRef` |
| `Component.fromPreview preview` | `Component.toRef component` |
| `Component.withComponent_ "Label" block` | `Controls.addMapped "Label" block` |

---

## Step-by-Step

### 1. Add the `Controls` import

```elm
-- v0
import Component

-- v1
import Component
import Controls
```

---

### 2. Replace the pipeline builder with a record

**v0**

```elm
textFieldPreview : Component.Preview () ()
textFieldPreview =
    Component.new
        (\s msg l i err ->
            UI.textField { msg = msg, label = l, id = i, value = s, error = e }
        )
        |> Component.withState_ "Value" Component.string
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withUnlabelled_ Component.identifier
        |> Component.withControl "Error" Component.string ""
        |> Component.toPreview { id = "text-field", name = "Text field" }
```

**v1**

```elm
type alias TextFieldModel =
    { value : String
    , label : String
    , id : String
    , error : String
    }

textField : Component.Component () () TextFieldModel (Component.Update () ())
textField =
    { id = "text-field"
    , name = "Text field"
    , controls =
        Controls.builder TextFieldModel
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.add "Error" .error Controls.string
            |> Controls.toControls
    , view =
        Component.view <|
            \model setter ->
                UI.textField
                    { msg = \v -> setter { model | value = v }
                    , label = model.label
                    , id = model.id
                    , value = model.value
                    , error = if model.error == "" then Nothing else Just model.error
                    }
    }
```

Key changes:

- Define a record type for your model. Each field corresponds to one control.
- Replace `Component.new (\a b c -> ...)` with `Component.view (\model setter -> ...)`.
  The view now receives the whole model and a setter `(m -> msg)` rather than
  individual curried arguments.
- Replace `withState`/`withControl`/`withUnlabelled_` with `Controls.add "Label" .field block`.
  There is no distinction between "state" and "control" any more — every field
  is an `add` call.

---

### 3. Replace `build`/`addVia`/`finish_` with `Controls.builder`/`add`/`toControls`

**v0**

```elm
Component.build (\label value -> { label = label, value = value })
    |> Component.addVia .label "Label" Component.string
    |> Component.addVia .value "Value" Component.string
    |> Component.finish_
```

**v1**

```elm
Controls.builder (\label value -> { label = label, value = value })
    |> Controls.add "Label" .label Controls.string
    |> Controls.add "Value" .value Controls.string
    |> Controls.toControls
```

The argument order for `add` changed: `addVia getter "Label" block` →
`add "Label" getter block`.

---

### 4. Replace `withMsg` / `withUpdateF` with `Controls.withUpdate`

**v0** — attaching a message type to an individual control:

```elm
Component.new (\msg -> UI.button [ Html.Events.onClick (msg ()) ] [ Html.text "Click" ])
    |> Component.withMsg identity
    |> Component.toPreview { id = "btn", name = "Button" }
```

**v1** — use `Controls.withUpdate` on the whole controls block:

```elm
type alias ButtonModel =
    { clicked : Bool }

button : Component.Component () () ButtonModel (Component.Update () ())
button =
    { id = "btn"
    , name = "Button"
    , controls =
        Controls.builder ButtonModel
            |> Controls.add "Clicked" .clicked Controls.bool
            |> Controls.toControls
            |> Controls.withUpdate (\_ new -> ( new, [] ))
    , view =
        Component.view <|
            \model setter ->
                UI.button
                    [ Html.Events.onClick (setter { model | clicked = True }) ]
                    [ Html.text "Click" ]
    }
```

`Controls.withUpdate` receives `(oldModel -> newModel -> (newModel, List effect))`.
It fires whenever any control value changes.

---

### 5. Replace `withDefault` (moved to `Controls`)

**v0**

```elm
Component.withDefault "hello" Component.string
```

**v1**

```elm
Controls.withDefault "hello" Controls.string
```

`withDefault` is now in the `Controls` module.

---

### 6. Replace `withUnlabelled_` / `Component.hidden`

If a field should not appear in the controls panel (e.g. a stable ID), wrap it
with `Controls.hidden`:

**v0**

```elm
|> Component.withUnlabelled_ Component.identifier
```

**v1**

```elm
|> Controls.add "Id" .id (Controls.hidden Controls.identifier)
```

You still name the field and provide a getter; `Controls.hidden` removes it from
the UI while keeping it in serialised state.

---

### 7. Replace `toPreview` / `Component.Application.element`

**v0** — previews were top-level values fed directly to `element`:

```elm
main =
    Component.Application.element
        [ Component.group "Components"
            [ textFieldPreview
            , dropdownInputPreview
            ]
        ]
        Nothing
```

**v1** — components are wrapped in frames, and frames live inside named playground
pages:

```elm
main =
    Component.Application.element
        [ Component.group { id = "components", name = "Components" }
            [ Component.playground { id = "text-field", name = "Text field" }
                [ Component.explore textField ]
            , Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
                [ Component.explore dropdownInput ]
            ]
        ]
        Nothing
```

- `Component.explore component` — the main interactive frame (replaces `toPreview`).
- `Component.example "Variant name" initialModel component` — a frame with a
  pinned starting model (replaces `toPreview` with a fixed default).
- `Component.doco html` — a static documentation frame.
- `Component.playground { id, name } [frames]` — a named page containing frames.
- `Component.group { id, name } [playgrounds]` — a named group of pages.
  Now requires an `{ id, name }` record (v0 took a plain `String`).

---

### 8. `fromLookup` / `withPresets`

These are now in `Controls` instead of `Component`:

```elm
-- v0
Component.withPresets ( True, "True" ) [ ( False, "False" ) ]
Component.fromLookup ( "a", myValue ) rest

-- v1
Controls.withPresets ( True, "True" ) [ ( False, "False" ) ]
Controls.fromLookup ( "a", myValue ) rest
```

---

### 9. `list`

```elm
-- v0
Component.list Component.string

-- v1
Controls.list Controls.string
```

---

### 10. Replace `previewBlock` / combination elements

`Component.previewBlock` is now `Controls.componentRef`. Use
`Controls.addMapped` instead of `withComponent_` since the storage type
(`String` id) differs from the output type (`Html`).

**v0**

```elm
Component.new
    (\title inner innerList -> ...)
    |> Component.withControl "Title" Component.string "Title"
    |> Component.withControl_ "Element" Component.previewBlock
    |> Component.withControl "Element list"
        (Component.list Component.previewBlock)
        [ Component.fromPreview textFieldPreview ]
    |> Component.toPreview { id = "combo", name = "Combination Element" }
```

**v1**

```elm
type alias ComboModel =
    { title : String
    , inner : Html (Component.Update () ())
    , innerList : List (Html (Component.Update () ()))
    }

comboElement : Component.Component () () ComboModel (Component.Update () ())
comboElement =
    { id = "combo"
    , name = "Combination Element"
    , controls =
        Controls.builder ComboModel
            |> Controls.add "Title" .title (Controls.string |> Controls.withDefault "Title")
            |> Controls.addMapped "Element" Controls.componentRef
            |> Controls.addMapped "Elements"
                (Controls.listMapped Controls.componentRef
                    |> Controls.withDefaultMapped [ Component.toRef textField ]
                )
            |> Controls.toControls
    , view =
        Component.view <|
            \model _ ->
                Html.div [] (Html.text model.title :: model.inner :: model.innerList)
    }
```

Key changes:

- `Component.previewBlock` → `Controls.componentRef`
- `withControl_` / `withComponent_` → `Controls.addMapped` (no getter needed)
- `Component.list Component.previewBlock` → `Controls.listMapped Controls.componentRef`
- `Component.fromPreview preview` → `Component.toRef component`
- The model record has `Html` fields (the mapped output), not `String` fields

---

## Full Before / After Example

**v0**

```elm
module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI

dropdownInputPreview : Component.Preview () ()
dropdownInputPreview =
    Component.new
        (\label selected msg options i ->
            UI.select { id = i, label = label, options = options, value = selected, msg = msg }
        )
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withState "Value" Component.string "2"
        |> Component.withControl "Options"
            (Component.list
                (Component.build (\label value -> { label = label, value = value })
                    |> Component.addVia .label "Label" Component.string
                    |> Component.addVia .value "Value" Component.string
                    |> Component.finish_
                )
            )
            [ { label = "One", value = "1" }
            , { label = "Two", value = "2" }
            , { label = "Three", value = "3" }
            ]
        |> Component.withUnlabelled_ Component.identifier
        |> Component.toPreview { id = "dropdown-input", name = "Simple Dropdown Input" }

main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element
        [ Component.group "Components" [ dropdownInputPreview ] ]
        Nothing
```

**v1**

```elm
module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Controls

type alias DropdownModel =
    { label : String
    , value : String
    , options : List { label : String, value : String }
    , id : String
    }

dropdownInput : Component.Component () () DropdownModel (Component.Update () ())
dropdownInput =
    let
        optionControls =
            Controls.builder (\label value -> { label = label, value = value })
                |> Controls.add "Label" .label Controls.string
                |> Controls.add "Value" .value Controls.string
                |> Controls.toControls
    in
    { id = "dropdown-input"
    , name = "Simple Dropdown Input"
    , controls =
        Controls.builder DropdownModel
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Options" .options (Controls.list optionControls)
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.toControls
    , view =
        Component.view <|
            \model setter ->
                UI.select
                    { id = model.id
                    , label = model.label
                    , options = model.options
                    , value = model.value
                    , msg = \v -> setter { model | value = v }
                    }
    }

main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element
        [ Component.group { id = "components", name = "Components" }
            [ Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
                [ Component.explore dropdownInput ]
            ]
        ]
        Nothing
```
