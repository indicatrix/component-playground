
# Component Playground

An interactive component testing library for Elm. Define components with
controls and views, then assemble them into a browsable playground.

## Quick Start

```elm
import Component
import Component.Application
import Component.Application.Theme as Theme
import Component.Control as Control
import Component.Playground as Playground


-- 1. Define a model
type alias ButtonModel =
    { label : String
    , disabled : Bool
    }


-- 2. Define a component
button : Component.Component e t ButtonModel msg
button =
    Component.component
        { id = "button"
        , name = "Button"
        , controls =
            Control.builder ButtonModel
                |> Control.add "Label" .label Control.string
                |> Control.add "Disabled" .disabled Control.bool
                |> Control.toControl
                |> Control.withDefault { label = "Click me", disabled = False }
        , view =
            \model setter ->
                Html.button
                    [ Html.Attributes.disabled model.disabled
                    , Html.Events.onClick (setter { model | label = "Clicked!" })
                    ]
                    [ Html.text model.label ]
        }


-- 3. Assemble into a playground
main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element Theme.default
        [ Playground.group { id = "components", name = "Components" }
            [ Playground.fromComponent { id = "button", name = "Button" } button
            ]
        ]
        Nothing
```

## Controls

Controls describe how a model value is stored, retrieved, and rendered as
interactive UI.

### Primitives

```elm
Control.string   -- text input
Control.int      -- validated int input
Control.float    -- validated float input
Control.bool     -- True/False dropdown
```

Use `Control.fromOptions` for a dropdown of named values:

```elm
Control.fromOptions "Size" ( "sm", "Small" ) [ ( "md", "Medium" ), ( "lg", "Large" ) ]
```

Use `Control.fromLookup` when your type contains functions (where `(==)` won't
work):

```elm
Control.fromLookup "Formatter"
    ( "default", defaultFormatter )
    [ ( "compact", compactFormatter ) ]
```

Other primitives: `Control.identifier` (stable unique string),
`Control.custom` (custom serialisation, no UI), `Control.componentRef`
(embed another component — see [Advanced](#advanced) section).

### Modifiers

```elm
Control.withDefault { label = "Hi" } myControl  -- override default value
Control.withDescription "Count" Control.int      -- override the label
Control.hidden Control.identifier                -- keep in state, hide from UI
Control.withUpdate (\old new -> ( clamp 0 100 new, [] )) myControl  -- post-update logic
```

### Building record controls

Use the builder pipeline to compose controls for record types. Field order
must match constructor argument order:

```elm
type alias TextFieldModel =
    { value : String, label : String, id : String }

textFieldControl : Control.Control e t TextFieldModel
textFieldControl =
    Control.builder TextFieldModel
        |> Control.add "Value" .value Control.string
        |> Control.add "Label" .label Control.string
        |> Control.add "Id" .id Control.identifier
        |> Control.toControl
        |> Control.withDefault { value = "", label = "Name", id = "unused" }
```

### Lists and Maybe

```elm
Control.list Control.string
-- Control_ e t (List String) (List String)

Control.maybe Control.int
-- Control_ e t { has : Bool, val : Int } (Maybe Int)
```

## Frames

Frames (`Component.Frame`) determine how a component appears on a playground
page:

- `Frame.fromComponent component` — fully interactive with controls panel
- `Frame.example "Empty state" initialStorage component` — interactive with a
  pinned starting storage value
- `Frame.gallery "Variants" component (\render -> ...)` — non-interactive
  multi-variant display
- `Frame.static html` — static HTML (documentation, Figma embeds, etc.)

Wrap any frame with chrome using `Frame.wrap`:

```elm
Frame.fromComponent myComponent
    |> Frame.wrap
        (\inner ->
            Html.div
                [ Html.Attributes.style "height" "300px" ]
                [ inner ]
        )
```

On interactive frames, `wrap` applies to the component view only — not to the
controls panel. It composes (`|> wrap |> wrap`) with the outermost call
producing the outermost layer in the DOM.

## Playground Structure

Pages and groups (`Component.Playground`) form a navigable sidebar tree.
`Playground.fromComponent` is sugar for a single-component page;
`Playground.fromFrames` takes an explicit list of frames.

```elm
import Component.Frame as Frame
import Component.Playground as Playground

Playground.group { id = "inputs", name = "Inputs" }
    [ Playground.fromFrames { id = "text", name = "Text Field" }
        [ Frame.fromComponent textField
        , Frame.example "Empty" { value = "", label = "Name", id = "" } textField
        , Frame.static (Html.p [] [ Html.text "A basic text input." ])
        ]
    , Playground.fromComponent { id = "select", name = "Select" } selectInput
    ]
```

## Advanced

### `Control_` and `Component_`

Most controls have the same storage and output type: `Control e t m` is an
alias for `Control_ e t m m`. But some controls store a different type than
they produce — `componentRef` stores a `ComponentRef` but outputs rendered
`Html`, and `fromLookup` stores a `String` key but outputs the looked-up
value.

When a component uses these mapped controls, use `Control_`, `add_`,
`toControl_`, and `component_`:

```elm
type alias ComboStorage =
    { title : String, inner : ComponentRef }

type alias ComboView =
    { title : String, inner : Html (Component.Update () ()) }

combo : Component.Component_ () () ComboStorage ComboView (Component.Update () ())
combo =
    Component.component_
        { id = "combo"
        , name = "Combo"
        , controls =
            Control.builder
                (\title element ->
                    { state = { title = title, inner = element.state }
                    , toValue = \s -> { title = s.title, inner = element.toValue s.inner }
                    }
                )
                |> Control.add "Title" .title (Control.string |> Control.withDefault "Title")
                |> Control.add_ "Element" .inner Control.componentRef
                |> Control.toControl_
        , view =
            \_ model _ ->
                Html.div []
                    [ Html.text model.title
                    , model.inner
                    ]
        }
```

The constructor passed to `Control.builder` returns a
`{ state, toValue }` record: the storage record and a mapping function from
storage to output. `add_` feeds a `{ state, toValue }` record per mapped
field to the constructor. `toControl_` finalises the split.

Set `componentRef` defaults with `Component.toRef`:

```elm
Control.componentRef
    |> Control.withDefault (Component.toRef myComponent)
```

### Sum types with `addWhen`

`Control.addWhen` conditionally shows a field's controls based on the current
state. Combined with `toControl_`, this supports sum types:

```elm
type ContentBlock
    = TextContent String
    | NumberContent Int

type alias ContentBlockStorage =
    { kind : String, text : String, number : Int }

contentBlockControl : Control.Control_ e t ContentBlockStorage ContentBlock
contentBlockControl =
    Control.builder
        (\kind text number ->
            { state = ContentBlockStorage kind text number
            , toValue = \s ->
                case s.kind of
                    "text" -> TextContent s.text
                    _      -> NumberContent s.number
            }
        )
        |> Control.add "Kind" .kind
            (Control.fromOptions "Kind"
                ( "text", "Text" )
                [ ( "number", "Number" ) ]
            )
        |> Control.addWhen (\s -> s.kind == "text") "Text" .text Control.string
        |> Control.addWhen (\s -> s.kind == "number") "Number" .number Control.int
        |> Control.toControl_
```

When the user switches "Kind", only the matching field's controls are shown.
All fields remain in state regardless of visibility.

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
