# Spec: stateful host-rendered Inspector controls

Status: **proposal / not implemented.** This note specs the follow-up to the
`ControlRenderers` proof (`docs/inspector-theming-and-injection.md`). It is the
plan for letting a host render Inspector controls that need their own UI state —
chiefly a custom dropdown with an open/closed menu — without breaking the
generic library or the standalone fallback.

## 1. What `ControlRenderers` supports today

`ControlRenderers msg` currently exposes one renderer:

```elm
type alias SelectConfig msg =
    { id : String
    , label : String
    , value : String
    , options : List { label : String, value : String }
    , onChange : String -> msg
    }

type alias ControlRenderers msg =
    { select : SelectConfig msg -> Html msg }
```

It is **stateless by construction**. The library owns the only state that
exists — the control's *value* — in its `Lookup` (keyed by the control's `Ref`).
On every render it reads the current value, hands the renderer pure data, and
the renderer's only output channel is `onChange : String -> msg`, which commits
a new value. The renderer holds nothing between renders. Injection is via the
`Library` (threaded at build) with `Application.initWith`; `default` provides a
fallback built from `Component.Ui.select`, so the library still works with no
host renderers.

## 2. Why native `<select>` is enough for the proof but not a real dropdown

The proof renders a native `<select>`. That works precisely *because the browser
owns all the interaction state*:

- **open/closed** menu — the OS opens and closes the popup;
- **active/highlighted option** during keyboard nav — the OS tracks it;
- **the menu surface itself** — drawn by the OS, outside the DOM.

So a native select needs no app state, and value-only `onChange` is sufficient.

A real Planwisely dropdown renders **its own menu surface in the DOM** so it can
carry the design system (tokens, spacing, check marks, group headers, the purple
focus glow, etc.). The moment the menu is app-drawn, the app must track:

- whether the menu is open,
- which option is keyboard-active,
- focus / typeahead.

None of that fits a value-only protocol. Consequences with today's API: the open
menu can't be themed, and the dropdown can't behave as a controlled component
(open, arrow-key through options, select, close on outside-click). This is the
core finding: **value updates alone are insufficient when the host control owns
internal UI state.**

## 3. What state the protocol must carry

A new, explicitly *UI* state — distinct from the control's stored value:

| State | Type | Why |
|---|---|---|
| open | `Bool` | menu visibility |
| active option | `Maybe Int` | keyboard highlight for arrow-key nav |
| (optional) typeahead | `String` | type-to-find buffer |

And the events to mutate it, alongside the existing value commit:

- `setOpen : Bool -> msg` (or `toggleOpen`)
- `setActive : Maybe Int -> msg`
- `onChange : String -> msg` — unchanged; commits the value (and typically closes)

Crucially, the setters must feed a **library-owned UI-state store**, not the
component's value. That requires the control-update channel to carry a *UI-state
update* kind in addition to the existing value updates — see §5.

Keyboard and outside-click handling: the library should own the *policy* (Esc
closes, Enter selects the active option, click-outside closes, only one menu open
at a time) over the store, while the host renderer owns the *visuals*. This keeps
behaviour consistent and prevents every host control re-implementing it.

## 4. Per-control scoping

The library already allocates a unique `Ref` per control. Scope UI state by that
`Ref`:

```elm
-- in the Application Model
controlUi : Dict Ref ControlUiState
```

Each renderer invocation receives **only its own slice** plus setters that are
pre-scoped to its `Ref`, so toggling one dropdown cannot affect another, and the
"single open at a time" policy is just a transform over this dict (close all
others when one opens). State is keyed by identity the library already owns, so
nothing new needs to be threaded through the control tree.

## 5. Doing it without breaking standalone fallback

Two compatibility rules:

1. **Additive, opt-in renderer.** Keep the existing stateless `select`. Add a
   *separate optional* stateful slot. A host that provides nothing, or only the
   stateless `select`, is unaffected; `default` stays stateless (native select).
   The library only allocates/threads `controlUi` for controls whose stateful
   renderer is actually present.

   ```elm
   type alias ControlRenderers msg =
       { select : SelectConfig msg -> Html msg                       -- today
       , selectStateful : Maybe (StatefulSelectConfig msg -> Html msg) -- opt-in
       }
   ```

2. **Extend the update channel, don't replace it.** The setters return the same
   `msg` the control system already dispatches, widened with a UI-state kind that
   `Application.update` routes to the per-`Ref` `controlUi` store (rather than to
   the value `Lookup`). Existing value updates are untouched, so stateless
   controls and the fallback render exactly as now.

## Smallest API sketch

```elm
-- Library-owned, per-control UI state (keyed by Ref in the Model).
type alias ControlUiState =
    { open : Bool, active : Maybe Int }

-- This control's slice + setters, pre-scoped to its Ref. The setters' msgs
-- flow to the library's controlUi store, not the value Lookup.
type alias ControlUiEnv msg =
    { state : ControlUiState
    , setOpen : Bool -> msg
    , setActive : Maybe Int -> msg
    }

type alias StatefulSelectConfig msg =
    { id : String
    , label : String
    , value : String
    , options : List { label : String, value : String }
    , onChange : String -> msg   -- commit value (+ library closes the menu)
    , ui : ControlUiEnv msg      -- NEW: open/active + scoped setters
    }
```

The library: keeps `controlUi : Dict Ref ControlUiState`; when a control has a
`selectStateful` renderer, looks up its slice, builds a `ControlUiEnv` with
setters bound to that `Ref`, and renders via the host function; `Application.update`
applies UI-state msgs to the dict (and enforces single-open / Esc / outside-click
policy). No stateful renderer → unchanged stateless path.

## 6. Migration path

- **Phase 0 — done.** Stateless `ControlRenderers` proof: host-rendered select,
  library fallback preserved.
- **Phase 1 — protocol.** Add `controlUi : Dict Ref ControlUiState`, the UI-state
  update kind in `Application.update`, the `ControlUiEnv`, and `selectStateful`.
  Library owns open/active + the close policies. Fallback stays stateless.
- **Phase 2 — real dropdown.** Planwisely supplies `selectStateful` backed by its
  production `UI.Dropdown` (styled open menu). Validate rest / hover / focus /
  **open** / active / selected / keyboard.
- **Phase 3 — generalise.** Reuse the per-`Ref` UI-state pattern for the other
  *stateful* controls (multi-select, palette, date/time pickers). Stateless
  controls (text field, toggle, checkbox, number) stay on the simple renderer —
  no UI state, no extra cost.
- **Phase 4 — default.** Once parity and host coverage are broad, make the host
  renderers the default path and retire native fallbacks where a host always
  provides them; keep `default` for standalone use of the library.

### Out of scope / explicitly deferred

Control radius (now 4px, `radiusSm`) is a separate, already-decided token change
and is not part of this protocol work.
