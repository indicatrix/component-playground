
# UI refinements: muted text, preset Custom, sub-component indent

**Date:** 260421-0725
**Status:** Proposed. Clarifications resolved — scope narrowed.

---

## Goals

Four unrelated UX fixes on top of the application-UI redesign and control
presets work:

1. Sidebar items lean on muted text so the active page stands out.
2. The in-controls preset picker gains a **Custom** entry; underlying
   controls render only while Custom is selected.
3. Sub-component controls (embedded via `componentRef`) indent like
   builder-group fields so the boundary is visible.
4. Control labels and preset-tab labels use `mutedTextColor` so the
   control *values* (textfield content, active tab) visually dominate.

Breaking API changes are acceptable but these are visual tweaks and
none should be needed.

---

## 1. Sidebar: de-emphasise to emphasise

**Current** (`Application.elm:708-741`): `viewPageLink` colours every
page link with `theme.textColor`; the active link is distinguished only
by `theme.headingFontWeight` and `theme.activeLinkBackground`. Group
headers (`Application.elm:680`) also use `theme.textColor`.

**Proposed:**

- Inactive page link → `theme.mutedTextColor`, `bodyFontWeight`.
- Active page link → `theme.textColor`, `headingFontWeight`
  (unchanged), plus existing `activeLinkBackground` highlight.
- Group-heading label (`Application.elm:673-683`) → `theme.mutedTextColor`.
- Sidebar branding header stays on `theme.textColor` (branding, not
  navigation).

---

## 2. Preset picker (dropdown): Custom option, gate controls on Custom

**Current** (`Frame.elm:454-476` `makePicker`): the dropdown lists only
named presets; the component's inner controls render unconditionally
underneath — so a page with a list of preset-bearing sub-components
produces "control spam" even when every sub-component is pinned to a
named preset.

**Proposed:** the in-controls dropdown picker (the one emitted by
`makePicker`, which appears either at the top of a `fromComponent`
frame's controls when the component has presets, or underneath a
`componentRef` select when the embedded component has presets) gets a
`Custom` entry. Selecting Custom dispatches a pick that clears the
preset slot; the existing `PresetsInfo.current` reader already maps
absent/empty/stale to `Nothing`.

Controls gating: in `makeComponentE.controls`, when a named preset is
active (`info.current lookup == Just _`), render only the picker. When
Custom (`Nothing`), render picker + `innerControls` as today.

**`Frame.presets` tab bar: unchanged.** No Custom tab, no controls
gating — current behaviour is correct. The tab bar is explicit UX and
the settings-2 toggle already gates the controls pane at the frame
level.

**Gallery dropdowns unaffected.** `Control.fromOptions` already shows
a "Custom" option *only when the stored value isn't one of the listed
options* (`Control.elm:576-582`). That's a different behaviour — don't
conflate. The preset picker should *always* offer Custom as a
deliberate choice; `fromOptions` keeps its opportunistic rendering.

---

## 3. Sub-component controls: name label + indentation

**Current** (`Control.elm:791-803` in `componentRef.controls`):
embedded controls are rendered directly beneath the component-picker
`select` in a plain `Ui.vStack` with 8px gap — no name, no indent, no
separator.

**Proposed:** match the treatment `toControl` uses for nested builder
fields (`Control.elm:181-191`) — the embedded component's `name` as a
label above an indented vStack of its controls:

```elm
Ui.vStack [ Ui.style "gap" "8px" ]
    (Ui.select theme { ...componentPicker... }
        :: (if List.isEmpty embeddedControls then
                []
            else
                [ Ui.text theme [] [ Html.text embeddedComponentName ]
                , Ui.vStack
                    [ Ui.style "gap" "8px"
                    , Ui.style "padding-left" "16px"
                    , Ui.style "border-left" ("1px solid " ++ theme.dividerColor)
                    ]
                    embeddedControls
                ]
           )
    )
```

Two new requirements flow from adding the name:

- `Library_.lookupDef` returns the factory but not the component's
  display name. Extend `Library_.index` lookup (it already carries
  `{ id, name }`) — resolve the current `currentId` against
  `lib.index` to find the name. No `Internal` changes needed.
- The label reads from the currently-selected component ref (i.e. it
  updates as the picker dropdown changes), so put it inside the
  `controls theme label default lookup` closure, not hoisted.

The border-left is a small addition on top of the `toControl`
pattern — it gives the nested group a visible bracket so it's obvious
where the sub-component's controls start/stop. Applying the same
border-left to `toControl` / `toControl_` / `list` builder groups is
a consistency win and costs nothing — do it at the same time.

---

## 4. Muted labels on controls and tabs

**Current:** `Ui.textStyles` (`Ui.elm:33-39`) uses `theme.textColor`,
which flows into:

- `Ui.text` (used for builder-group labels, `Control.elm:183, 242`).
- `Ui.textField` label (`Ui.elm:137-141`).
- `Ui.select` label (`Ui.elm:199-203`).

Preset tab-bar labels (`Application.elm:958`) also use `theme.textColor`
for both active and inactive tabs.

**Proposed:**

- Introduce `Ui.labelStyles : Theme -> List (Attribute msg)` — same
  as `textStyles` but `color = theme.mutedTextColor`. Apply it to
  the `<label>` elements in `textField` and `select`, and to the
  group label `Ui.text` call inside `toControl` / `toControl_`.
- Preset tabs: inactive tab → `mutedTextColor`; active tab →
  `textColor` (unchanged). Underline indicator unchanged. This
  mirrors the sidebar treatment in §1.

The input *values* (textfield contents, selected option text) stay on
`theme.textColor`. The muted treatment is strictly for labels.

---

## Out of scope

- Restyling `activeLinkBackground` or the underline indicator.
- Changing which elements are selectable in the sidebar.
- Any revisit of the sidebar header/footer slots.
- Muted-text treatment on input values or headings — only labels
  and unselected nav/tab items.

---

## Public API impact

| Symbol | Change |
|--------|--------|
| `Ui.labelStyles` | **New** — muted variant of `textStyles`. |
| `Ui.textField` / `Ui.select` | Labels switch to `labelStyles`. Behavioural only. |
| Everything else | Internal adjustments in `Control.elm` and `Application.elm`. |

No changes to `Internal.elm`, `Frame.elm`, or the public constructor
signatures.

---

## Implementation steps

- [ ] Add `Ui.labelStyles` and wire it into `Ui.textField`,
      `Ui.select`, and the group-label `Ui.text` calls in
      `Control.toControl` / `Control.toControl_` / `Control.listHelper`.
      (§4)
- [ ] `viewPageLink` — inactive uses `mutedTextColor`; active stays
      on `textColor`. Group-heading label uses `mutedTextColor`. (§1)
- [ ] Preset tab bar — inactive tab uses `mutedTextColor`. (§4)
- [ ] `makePicker` — append a Custom option; gate `innerControls`
      in `makeComponentE.controls` on `info.current lookup == Nothing`.
      (§2)
- [ ] `componentRef.controls` — look up the current component's name
      from `lib.index`, render it as a label via `Ui.text`, wrap
      embedded controls in an indented `Ui.vStack` with `padding-left:
      16px` and a `border-left` using `theme.dividerColor`. (§3)
- [ ] Apply the same `border-left` to the nested builder-group
      indents in `Control.toControl` / `Control.toControl_` /
      `Control.listHelper` for consistency. (§3 follow-on)
- [ ] Compile examples (`cd examples && npx elm make src/Index.elm`),
      run `npx elm-test tests/`, format, `elm-review`.

---

## Resolved clarifications

1. **§1** — Sidebar branding header stays on `textColor` (no mute).
2. **§2** — `Frame.presets` tab bar unchanged. Gating applies only
   to the in-controls dropdown picker. Motivating case: a page with
   many preset-bearing sub-components stays clean when each is pinned
   to a named preset.
3. **§3** — Render the embedded component's `name` as a label above
   the indented block.
4. **§4** — "tabs for components" = preset tab bar. Inactive tab
   labels go muted.
