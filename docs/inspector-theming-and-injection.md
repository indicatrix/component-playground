# Inspector theming & control injection

The Inspector is "just another consumer" of the design system: the interface
that configures a component should look and behave like the components it
configures. This document describes how that is achieved without ever coupling
this generic library to any one host application's component code.

## Two layers, one rule

1. **Generic playground library (this repo).** App-agnostic control
   *primitives* (`Component.Ui.textField` / `select` / `button`). They are
   **themeable** and, in future, **injectable**. They must never import a host
   application's components.
2. **Host application (e.g. Planwisely / sage).** May supply its own production
   controls to the Inspector through an integration layer it owns.

**Hard rule:** dependencies only ever point host → library. `component-playground`
must not depend on `Planwisely`'s `UI.*` (or any host) modules. Solving the
"make the Inspector match production" problem by importing host components would
break the library's generic purpose and is not allowed.

## Where we are now (short-term: theming)

The Inspector chrome and controls are styled through the injected shell
stylesheet (`shellStylesheet` in `Component.Application`) plus the `ds*` design
constants. Control box chrome and every interaction state are token-driven via a
`.cp-control` ruleset (border, radius, fill, hover, brand-blue focus ring + glow,
disabled), so text fields and selects match the search field and the rest of the
Inspector. Typography stays `Theme`-driven.

A host aligns the look by:

- overriding the `Theme` fields it owns (`fontFamily`, `textColor`,
  `mutedTextColor`, font sizes/weights, `errorColor`, `backgroundColor`,
  `dividerColor`, sidebar slots), and
- (next step) the `ds*` constants being **promoted into the `Theme`** as proper
  control tokens — `accentColor`, `controlSurface`, `controlBorder`,
  `controlBorderHover`, `controlRadius`, `controlPadding`, `focusRing`,
  `controlDisabledSurface`, `controlHeight*` — so the box chrome is fully host-
  controlled rather than carried by in-library constants. This keeps the
  library generic while letting Planwisely map every value to its `--pw-*`
  tokens.

## Where we are going (longer-term: injection)

Theming makes the library's own primitives *look* like production. Injection
lets the host render the Inspector with its *actual* production components, so
behaviour, accessibility and states are literally the same code as ship to
users — true self-consumption — still with no reverse dependency.

### Recommended API shape

Add an optional control-renderer slot to the application config (carried on the
`Theme`, or a sibling `config` record). The library defines the *interface* and
ships a default implementation built from `Component.Ui`; the host may override.

```elm
type alias ControlRenderers msg =
    { textField : TextFieldConfig msg -> Html msg
    , select : SelectConfig msg -> Html msg
    , toggle : ToggleConfig msg -> Html msg
    , number : NumberConfig msg -> Html msg
    -- …segmented, dateField, timeField as they are needed
    }

-- Config records are plain data the library already has at each control site:
type alias SelectConfig msg =
    { id : String
    , label : String
    , value : String
    , options : List { label : String, value : String }
    , onChange : String -> msg
    , disabled : Bool
    }

-- The library's fallback — its own primitives, used when the host injects nothing:
defaultControlRenderers : Theme -> ControlRenderers msg
```

Control rendering then calls `renderers.select {...}` instead of `Ui.select`
directly, where `renderers` comes from the host config (falling back to
`defaultControlRenderers theme`). The data passed is exactly what the controls
already carry today (`Component.Control` already builds `id`/`label`/`value`/
`options`/`onChange`).

### Where Planwisely provides the controls

In the host app (`sage`, `js/src/ComponentPlayground/Main.elm`), where the
`Theme`/config is already constructed for `Component.Application.init`,
Planwisely passes a `ControlRenderers` whose functions wrap its production
components:

```elm
planwiselyControls : ControlRenderers msg
planwiselyControls =
    { select = \c -> UI.Dropdown.view { … from c … }
    , textField = \c -> UI.TextInput.field { … from c … }
    , toggle = \c -> UI.Switch.viewWithSize Switch.Medium { … from c … }
    , number = \c -> UI.IncrementalInput.view { … from c … }
    }
```

The library imports none of this; the host plugs its components in through the
interface the library exposes. The dependency arrow stays host → library, and
the Inspector becomes the largest integration-test surface for the real
controls.

## Adoption order

1. ✅ Theme the existing primitives so the Inspector is visually compliant
   (done — `.cp-control` + shell stylesheet).
2. Promote the `ds*` constants into `Theme` control tokens (fully host-driven
   look).
3. Introduce `ControlRenderers` with a `Component.Ui`-based default.
4. Planwisely supplies its production renderers from `ComponentPlayground.Main`.
5. Expand coverage in the priority order: dropdowns → multi-selects → text
   fields → text areas → toggles → checkboxes → radios → segmented → number →
   date → time.
