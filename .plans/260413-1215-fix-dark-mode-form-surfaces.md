
# Fix dark-mode bleed-through on playground form controls

On Safari + macOS dark mode, `textField` and `select` widgets render with
black backgrounds in some contexts (notably `exploreFrame (with wrapper)`
and `Combination Element` in the examples app). The partial fix in
`Ui.elm` (replacing `background-color: inherit` with `theme.panelBackground`
on the `<input>` / `<select>` elements) leaves the surrounding widget chrome
transparent, so dark OS defaults still bleed through around the input and
behind the label.

---

## Root cause

Two things compound:

1. **Tailwind v4 preflight** sets `background-color: transparent` on
   `input`, `select`, `button`, `textarea`, and does **not** set
   `color-scheme` anywhere. They tried, then reverted — the `color-scheme:
   light` default on `<html>` broke too many v3→v4 upgrades (see
   [tailwindlabs/tailwindcss#15036](https://github.com/tailwindlabs/tailwindcss/pull/15036)).

2. **Safari in macOS dark mode** without a `color-scheme` declaration uses
   system dark colours for transparent surfaces where an ancestor isn't
   opaque. Per the
   [WebKit blog on dark-mode support](https://webkit.org/blog/8840/dark-mode-support-in-webkit/),
   `color-scheme` is the documented mechanism to pin this, and it can be
   scoped to individual elements.

The current `Ui` widgets leave most of their chrome (outer `vStack`,
`hStack`, `label`) transparent, so:

- **`exploreFrame (with wrapper)`** — the user's `#1a1a2e` wrapper shows
  through the widget chrome. Label text (dark) on dark wrapper is
  unreadable.
- **`Combination Element`** — no explicit dark wrapper, but Safari's dark
  system colours leak through the transparent chrome.

---

## Approach — minimal surface painting

The playground widgets should bring their own opaque surface. Instead of
adding a `Theme.colorScheme` field and threading a `color-scheme` CSS
property through the root view, we take the smaller, more localised fix:
set `background-color: theme.panelBackground` on the outer `vStack` of
`Ui.textField` and `Ui.select`.

Why this is enough:

- Every playground widget that currently breaks (`textField`, `select`) is
  the thing painting the dark bar. Making each widget a self-contained
  light card fixes both failure modes without touching the root view or
  adding new theme fields.
- In the `exploreFrame (with wrapper)` case this is also the *right*
  visual outcome: the widget renders as a card inside the user's dark
  wrapper, the wrapper frames it.
- Consumers who want different widget surfaces already have
  `theme.panelBackground` to tune, no new API.
- No breaking change.

`Ui.button` and `Ui.text` stay transparent — they're used as inline text
/ nav links, not surface-bearing widgets.

---

## Implementation steps

- [ ] In `src/Component/Ui.elm`, add
      `style "background-color" theme.panelBackground` to the outer
      `vStack` of `textField` (around line 170).
- [ ] Same change to `select`'s outer `vStack`.
- [ ] Compile (`npx elm make src/Component.elm src/Component/Application.elm
      src/Component/Control.elm`) and the examples
      (`cd examples && npx elm make src/Index.elm`).
- [ ] Manually verify in Safari + macOS dark mode:
      - `Combination Element` — textFields render as light cards.
      - `exploreFrame (with wrapper)` — textField renders as a light
        card inside the dark wrapper.
      - Regular `Text field` playground — unchanged.

---

## Sources

- [Dark Mode Support in WebKit](https://webkit.org/blog/8840/dark-mode-support-in-webkit/)
  — `color-scheme` is the official mechanism; can be scoped to specific
  elements; Safari/WebKit will not auto-darken content.
- [Tailwind v4 form-controls preflight PR (reverted)](https://github.com/tailwindlabs/tailwindcss/pull/15036)
  — confirms v4 does not ship `color-scheme: light` and form elements
  get `background-color: transparent`.
- [Tailwind color-scheme docs](https://tailwindcss.com/docs/color-scheme)
  — utility equivalents if we ever need to revisit `color-scheme`.

---

## Non-goals / deferred

- **Threading `color-scheme` through the `Theme`.** If dark/blueprint
  preset themes end up needing browser form-control colours to match,
  we can add `Theme.colorScheme` then. The minimal fix doesn't need it.
- **Fixing `Ui.button`'s transparent background.** Used as nav link —
  needs to stay transparent to pick up hover/active background from the
  theme.
- **Full dark-mode polish for preset themes.** Out of scope; the goal
  here is that the light theme renders correctly under OS dark mode.
