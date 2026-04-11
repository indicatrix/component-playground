
# Parameterise theme colors and fonts

Allow the playground chrome (sidebar, panels, typography, controls) to be
styled via a user-supplied `Theme` record, enabling dark mode, light mode,
blueprint schemes, etc.

---

## Literal inventory

All hardcoded color/font values currently in the codebase:

### `src/Component/Application.elm`

| Line | Value | Use |
|------|-------|-----|
| 396  | `#eee` | outer page background |
| 404  | `#fff` | sidebar panel background |
| 405  | `#aaa` | sidebar panel box-shadow color |
| 416  | `#fff` | content panel background |
| 417  | `#aaa` | content panel box-shadow color |
| 430  | `rgb(204, 204, 204)` | sidebar header bottom border |
| 492  | `#eee` | active page-link highlight background |
| 492  | `600`  | active page-link font-weight |

### `src/Component/UI.elm`

| Line | Value | Use |
|------|-------|-----|
| 31   | `Arial`  | font-family (text, heading, sub-heading, button, input) |
| 32   | `400`    | body font-weight |
| 33   | `14px`   | body font-size |
| 34   | `#222`   | body text color |
| 70   | `600`    | heading font-weight |
| 72   | `20px`   | heading font-size |
| 73   | `#222`   | heading text color |
| 79   | `Arial`  | (same font-family repeated) |
| 80   | `500`    | sub-heading font-weight |
| 81   | `16px`   | sub-heading font-size |
| 82   | `#222`   | sub-heading text color |
| 115  | `#ddd`   | input border color |
| 140  | `#f66`   | error input border color |
| 141  | `#f66`   | error message text color |
| 145  | `#ddd`   | normal input border color |

---

## Theme record design

New module `Component.Application.Theme` exposes:

```elm
type alias Theme =
    -- Chrome / layout
    { pageBackground      : String   -- #eee  outer wrapper
    , panelBackground     : String   -- #fff  sidebar + content panels
    , shadowColor         : String   -- #aaa  box-shadow color (panels)
    , sidebarDivider      : String   -- rgb(204,204,204)  sidebar header border
    , activeLinkBackground : String  -- #eee  selected nav link highlight

    -- Typography
    , fontFamily          : String   -- "Arial"
    , textColor           : String   -- #222
    , bodyFontSize        : String   -- 14px
    , bodyFontWeight      : String   -- 400
    , headingFontSize     : String   -- 20px
    , headingFontWeight   : String   -- 600
    , subHeadingFontSize  : String   -- 16px
    , subHeadingFontWeight : String  -- 500

    -- Controls
    , inputBorderColor    : String   -- #ddd
    , errorColor          : String   -- #f66
    }


default : Theme
default =
    { pageBackground       = "#eee"
    , panelBackground      = "#fff"
    , shadowColor          = "#aaa"
    , sidebarDivider       = "rgb(204, 204, 204)"
    , activeLinkBackground = "#eee"
    , fontFamily           = "Arial"
    , textColor            = "#222"
    , bodyFontSize         = "14px"
    , bodyFontWeight       = "400"
    , headingFontSize      = "20px"
    , headingFontWeight    = "600"
    , subHeadingFontSize   = "16px"
    , subHeadingFontWeight = "500"
    , inputBorderColor     = "#ddd"
    , errorColor           = "#f66"
    }
```

`shadowColor` is just the color part; `Application.elm` composes the full
`box-shadow` string (`theme.shadowColor ++ " 0px 2px 4px"`).

---

## Module location and exposure

- New file: `src/Component/Application/Theme.elm`
  → module `Component.Application.Theme`
- Add to `elm.json` `exposed-modules`
- No circular deps: `Theme.elm` imports nothing from the package; `UI.elm`
  imports `Theme`; `Application.elm` imports both.

---

## Thread-through strategy

### Option chosen: Theme lives in `Model`

Theme is set once at start-up and never changes during a session (no Msg for
it). Storing it in `Model` means `view : Model t e -> Html (Msg t e)` keeps
its current arity — no breaking change to the view signature.

This is preferable to `view : Theme -> Model t e -> Html (Msg t e)` which
would be a more disruptive change for embedders.

### `Component.Application.elm`

1. Add `theme : Theme` field to the `Model t e` type alias.
2. `init` gains a `Theme` first argument:
   ```elm
   init : Theme -> List (Playground e t) -> Maybe Url.Url -> Model t e
   ```
3. `element` gains a `Theme` first argument (or uses `Theme.default`):
   ```elm
   element : Theme -> List (Playground () t) -> Maybe Url.Url -> ComponentPlayground t ()
   ```
   For minimal friction we could offer `element` (uses `default`) and
   `elementWithTheme`. But a single `element theme ...` is cleaner and honest
   about the capability.
4. All private view helpers receive `Theme` as their first argument:
   `view`, `viewSidebarHeader`, `viewSearchBox`, `viewIndex`, `viewPageLink`,
   `viewFrame`, etc. Each reads only the fields it needs.
5. Replace every inline literal with the corresponding `theme.*` field.

### `Component.UI.elm`

The five style-list helpers and the two composite widgets all become
functions that take `Theme`:

```elm
textStyles       : Theme -> List (Attribute msg)
headingStyles    : Theme -> List (Attribute msg)
subHeadingStyles : Theme -> List (Attribute msg)
inputStyles      : Theme -> List (Attribute msg)

button    : Theme -> List (Attribute msg) -> List (Html msg) -> Html msg
textField : Theme -> { msg, id, label, value, error } -> Html msg
select    : Theme -> { id, options, label, value, msg }  -> Html msg
```

`fullHeight`, `hStack`, `vStack`, `style`, `onClick`, `disableAutocomplete`,
`controlWidth` are layout/utility helpers with no color/font content — they
stay as-is.

---

## Public API changes (semver impact)

| Symbol | Change |
|--------|--------|
| `Model t e` | gains `theme : Theme` field — technically breaking for record destructure |
| `init` | new first arg `Theme` — breaking |
| `element` | new first arg `Theme` — breaking |
| `Component.Application.Theme` | new module (additive) |

This is a **minor breaking change** requiring a major version bump. The
migration for consumers is mechanical: add `Theme.default` to each call-site.

---

## Implementation steps

- [ ] Create `src/Component/Application/Theme.elm` with `Theme` alias and
      `default` value
- [ ] Add `Component.Application.Theme` to `elm.json` exposed-modules
- [ ] Update `src/Component/UI.elm`: thread `Theme` through all affected
      functions
- [ ] Update `src/Component/Application.elm`: add `theme` to `Model`, thread
      through `init`, `element`, and all view helpers
- [ ] Update examples (`examples/src/`) to pass `Theme.default`
- [ ] Run `npx elm-test tests/` and `npx elm-review` — fix any fallout
- [ ] Run `npx elm-format --yes src/ tests/`

---

## Example themes

These ship inside `Component.Application.Theme`. Elm's dead code elimination
means unused themes add no bundle cost for consumers.

**Dark mode** — swap backgrounds and text:
```elm
dark : Theme
dark =
    { default
        | pageBackground       = "#1a1a1a"
        , panelBackground      = "#2a2a2a"
        , shadowColor          = "#000"
        , sidebarDivider       = "#444"
        , activeLinkBackground = "#333"
        , textColor            = "#eee"
        , inputBorderColor     = "#555"
    }
```

**Blueprint** — blue-tinted scheme:
```elm
blueprint : Theme
blueprint =
    { default
        | pageBackground       = "#0d1b2a"
        , panelBackground      = "#1b2e45"
        , shadowColor          = "#0a1520"
        , sidebarDivider       = "#2a4a6a"
        , activeLinkBackground = "#1e3a5f"
        , textColor            = "#c0d8f0"
        , inputBorderColor     = "#2a5080"
        , errorColor           = "#ff6b6b"
        , fontFamily           = "monospace"
    }
```

The module's `exposing` list: `Theme, default, dark, blueprint`.
