
# Component Playground

An interactive component testing library for Elm. Define components with
controls and views, then assemble them into a browsable playground.

## Quick Start

```elm
import Component
import Component.Application
import Controls


-- 1. Define a model
type alias ButtonModel =
    { label : String
    , disabled : Bool
    }


-- 2. Define a component
button : Component.Component e t ButtonModel msg
button =
    { id = "button"
    , name = "Button"
    , controls =
        Controls.builder ButtonModel
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Disabled" .disabled Controls.bool
            |> Controls.toControls
            |> Controls.withDefault { label = "Click me", disabled = False }
    , view =
        Component.view <|
            \model _ ->
                Html.button
                    [ Html.Attributes.disabled model.disabled ]
                    [ Html.text model.label ]
    }


-- 3. Assemble into a playground
main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element
        [ Component.group { id = "components", name = "Components" }
            [ Component.playground { id = "button", name = "Button" }
                [ Component.explore button ]
            ]
        ]
        Nothing
```

## Concepts

### Components

A `Component` is a record with an id, name, controls, and a view function.
The `id` must be **unique across all components** in the playground — it's
used for URL routing and cross-component references.

```elm
type alias Component e t m msg =
    { id : String
    , name : String
    , controls : Controls e t m
    , view : m -> (m -> msg) -> View msg
    }
```

### Controls

Controls describe how a model value is stored, retrieved, and rendered as
interactive UI. Build controls for record types with the builder pipeline:

```elm
Controls.builder MyModel
    |> Controls.add "Label" .label Controls.string
    |> Controls.add "Count" .count Controls.int
    |> Controls.add "Enabled" .enabled Controls.bool
    |> Controls.toControls
```

Available primitives: `string`, `int`, `float`, `bool`, `identifier`.

Combinators: `list`, `withPresets`, `fromLookup`, `custom`.

Modifiers: `withDefault`, `withUpdate`, `hidden`.

### Frames

Frames are how components appear on a playground page:

- `Component.explore component` — fully interactive, controls shown alongside
- `Component.example "name" initialModel component` — interactive with a
  pinned starting state
- `Component.doco html` — static documentation

### Playground Structure

Pages and groups form a tree for the sidebar:

```elm
Component.group { id = "inputs", name = "Inputs" }
    [ Component.playground { id = "text", name = "Text Field" }
        [ Component.explore textField
        , Component.example "Empty" { value = "", label = "Name" } textField
        , Component.doco (Html.p [] [ Html.text "A basic text input." ])
        ]
    , Component.playground { id = "select", name = "Select" }
        [ Component.explore selectInput ]
    ]
```

### Embedding Components

Use `Controls.componentRef` to embed one component inside another. The
control stores a component id and renders a dropdown selector. The
referenced component is rendered dynamically with its own controls.

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
            |> Controls.addMapped "Elements" (Controls.listMapped Controls.componentRef)
            |> Controls.toControls
    , view =
        Component.view <|
            \model _ ->
                Html.div [] (Html.text model.title :: model.inner :: model.innerList)
    }
```

Use `Controls.addMapped` (instead of `Controls.add`) for controls where the
storage type differs from the output type. `componentRef` stores a `String`
id but outputs `Html`. Use `Controls.listMapped` and
`Controls.withDefaultMapped` for the list and default variants.

Set defaults with `Component.toRef`:

```elm
Controls.listMapped Controls.componentRef
    |> Controls.withDefaultMapped
        [ Component.toRef textField
        , Component.toRef selectInput
        ]
```

## Dev

Enter the default dev-shell with `nix develop`.

Start the vite dev server with `nix develop -c npm run dev`.

Install new packages by updating package.json, running `npm i
--package-lock-only` and re-entering the dev shell.

Run npm executables using `npx <name> <arg> ...`. Eg:
- `npx elm-format` to format elm.
- `npx elm-test` to run elm tests.
- `npx elm-review` to run elm-review (linting).
- `npx tsc` to run typescript type check.
- `npx tsx` to run typescript files.
