
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
- `Frame.presets component` — interactive with a preset tab bar across the
  top (each named preset becomes a tab; the component must be extended with
  `Component.withPresets`)
- `Frame.presetGallery component` — non-interactive, renders every preset
  side-by-side
- `Frame.gallery component (\render -> ...)` — non-interactive multi-variant
  display with an explicit layout
- `Frame.static html` — static HTML (documentation, Figma embeds, etc.)
- `Frame.subheading "Label"` — a labelled divider between frames on a page

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

## Presets

Attach named configurations to a component with `Component.withPresets` and
`Component.preset`. Presets become tabs on `Frame.presets`, are enumerated by
`Frame.presetGallery`, and appear as a picker on any embedded `componentRef`.

```elm
textField
    |> Component.withPresets
        [ Component.preset "Empty" { value = "", label = "Name", id = "tf-1", error = "" }
        , Component.preset "Filled" { value = "Hello", label = "Name", id = "tf-2", error = "" }
        , Component.preset "With error"
            { value = "!", label = "Name", id = "tf-3", error = "Invalid" }
        ]
    |> Frame.presets
```

## Playground Structure

Pages and groups (`Component.Playground`) form a navigable sidebar tree.
`Playground.fromComponent` is sugar for a single-component page;
`Playground.fromFrames` takes an explicit list of frames.

```elm
import Component.Frame as Frame
import Component.Playground as Playground

Playground.group { id = "inputs", name = "Inputs" }
    [ Playground.fromFrames { id = "text", name = "Text Field" }
        [ Frame.subheading "Interactive"
        , Frame.fromComponent textField
        , Frame.subheading "Notes"
        , Frame.static (Html.p [] [ Html.text "A basic text input." ])
        ]
    , Playground.fromComponent { id = "select", name = "Select" } selectInput
    ]
```

`Playground.sortByName` sorts every level of the tree by name, ignoring case:

```elm
Component.Application.element Theme.default (Playground.sortByName playgrounds)
```

## Embedding: the sidebar and preferences

A host embedding the shell with `initWith` / `update` / `view` supplies two
effects in `Config`; `update` returns them alongside the message's own effects.

- `revealNavRow : Maybe (NavReveal -> e)` — moving to another page, or clearing
  a search, opens the active page's sidebar sections and asks the host to
  scroll its row into view. The request carries the nav list's and row's DOM
  ids, an `Align` (`Start` or `Nearest`) and the list's top padding; fire it
  with `Browser.Dom`.
- `savePreferences : Maybe (Preferences -> e)` — the collapsed sections and the
  preview canvas's height and look, whenever one changes (once per message, and
  once per canvas height drag). Store `encodePreferences`'s JSON.

`initWith` takes where to start and the stored preferences, and returns
effects of its own:

```elm
initPlayground : Decode.Value -> Maybe Location -> ( Model t e, List e )
initPlayground storedPreferences link =
    Component.Application.initWith config
        theme
        { location =
            link
                |> Maybe.filter (\loc -> Component.Application.isPage loc.page playgrounds)
                |> Maybe.withDefault landingPage
        , preferences =
            Decode.decodeValue Component.Application.preferencesDecoder storedPreferences
                |> Result.withDefault Component.Application.defaultPreferences
        }
        playgrounds
```

It opens the sections holding the starting page (a link only ever opens
sections) and leaves the rest as stored. Its effects are any from picking the
location's preset, a `Start`-aligned `revealNavRow` request for the page's row,
and a `savePreferences` call when the stored preferences needed changing (a
section opened, a section id that no longer exists dropped). The row exists
only once the view has rendered, so fire the reveal after the first render.

`preferencesDecoder` never fails: a missing or malformed field, or a value that
isn't an object (`null` when nothing is stored yet), takes its default, so
`Result.withDefault` above never applies.

A set canvas height renders within 240px and 90% of the window, so a height
saved in a taller window fits a shorter one.

## Host styling

The shell renders its stylesheet (hover/focus states, Inspector chrome,
`box-sizing` reset under `.cp-root`) inside a cascade layer named
`component-playground`.

A host using cascade layers (e.g. Tailwind 4) must declare that layer in its own
`@layer` order statement, loaded before the shell renders:

```css
@layer theme, base, components, component-playground, utilities;
```

Here the host's utilities beat the shell's rules and the shell's rules beat the
host's base and component styles. An undeclared layer is appended after every
layer declared so far, so it would beat the host's utilities.

Unlayered host rules beat the shell's rules, so a host's global unlayered
resets (e.g. `button { … }`) also apply inside the shell.

The shell also sets many properties as inline `style` attributes. Those sit
outside any layer and beat all non-`!important` stylesheet rules.

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
