# AI Inspector

Status: **plan / not yet implemented.** Front-end + interaction states only
(this pass). Backend task API and the real Claude Code file-editing bridge are
deferred (see Deferred). The approved mockup (`sage/design/design-system.html`,
states 1–6) is the visual source of truth; do not redesign.

## Objective

Give the Playground a sticky **AI Inspector** pinned to the bottom of the
existing right-side Inspector panel. It lets a user pick one element in the live
component preview, then either ask an agent to make a targeted change (Agent
chat) or edit the element's design tokens (Token editor), and track that work
through history and change-details views. This pass delivers the full
front-end and all six interaction states driven by an **in-memory, mocked**
work store; no source files are edited and no backend is called yet.

## Coarse Appetite

Weeks. Multi-PR even at "front-end only" scope. This document covers the
front-end pass; the backend + bridge is a separate future plan.

## Where this lives

The Inspector panel, live preview canvas, side nav and shell all live in **this
repo** (`indicatrix/component-playground`), which `sage` consumes as a flake
input pinned to `richard/inspector-selection-link`. So the feature is built
here and `sage` bumps its flake pin to consume it. The only `sage`-side changes
are: (a) installing one new custom element in
`js/src/ComponentPlayground/index.js`, and (b) the flake pin bump.

**Hard rule (unchanged):** dependencies point host → library. This feature must
not import any `sage`/Planwisely `UI.*` module. It uses `Component.Ui`
primitives, `Theme` tokens, and plain CSS-class strings only.

## Scope

### Shell integration — HIGH
- [ ] AI Inspector is a **shell-level** region owned by `Component.Application`
      (not a per-component `withInspectorBinding`). It renders for every
      component page, pinned to the bottom of `viewInspectorPanel`, visually
      separated from the scrollable inspector body above it, unaffected by
      inspector scrolling. The existing inspector content scrolls above it.
- [ ] New module `Component/Application/AiInspector.elm` holds the model, msgs,
      update, and view for the feature to keep `Application.elm` manageable;
      `Application` owns a single `aiInspector : AiInspector.Model` field and
      routes a single `AiInspectorMsg` case.

### Selection engine (live preview only) — HIGH
- [ ] Selection mode is entered by `Inspect` (default state) or `Start new
      selection`. It applies **only inside the live component preview** — never
      to playground chrome, side nav, breadcrumbs, toolbar, or the inspector.
- [ ] Hover highlights the hovered inspectable element; click selects it; Esc
      cancels. One selection at a time; a new selection replaces the previous.
      Selection mode ends once an element is selected. Focus moves to the
      selected-element card after selection; focus returns sensibly on cancel.
- [ ] DOM geometry / computed styles / data-attributes / selector path are
      JS concerns. A **custom element** `<cp-ai-selection>` wraps the preview:
      when `active`, it paints the hover/selected highlight overlay and, on
      click, emits a `cp-select` `CustomEvent` whose `detail` is the captured
      `SelectedElement` JSON. The library renders the wrapper and decodes the
      event into `AiInspectorMsg`. The custom element's JS ships in this repo
      (`public/` or an examples helper) and is installed by `sage`'s
      `index.js` alongside the existing `pw-*` custom elements.
- [ ] Stable targeting via explicit `data-*` on preview components rather than
      brittle DOM indexes: `data-ai-inspectable`, `data-component`,
      `data-element`, `data-token-*`, `data-source-file`, `data-source-symbol`.
      Playground frames get `data-ai-inspectable` + `data-component` at the
      callout wrapper; richer attributes are opt-in per component.

### Default state — MED
- [ ] Header: sparkle icon, `AI INSPECTOR`, info icon, collapse/expand chevron.
- [ ] Body: primary blue `Inspect` button (`square-dashed-circle-plus`) + helper
      `Select an element in the preview to inspect and edit with AI.`
- [ ] Footer action `Start new selection` (same icon) + helper
      `Click to enter selection mode in the preview.`

### Selected state (tabbed) — HIGH
- [ ] On selection the AI Inspector expands into a tabbed UI: `Agent chat` /
      `Token editor`, active tab using the blue-underline treatment
      (`Component.Ui`/themed). Remember the active tab across history /
      change-details navigation; Back returns to it (default `Agent chat`).
- [ ] **Selected element card** (both tabs): category icon, title
      (`Title (h1)`), subtitle (`Review account requirements`), close icon.
      Close clears selection → back to default.

### Agent chat tab — MED
- [ ] Selected element card + a plain chat input, placeholder
      `Describe what to build or change...`. No VSCode-derived UI (no Plan-agent
      helper text, no file chip, no tool row, no model/plus/code/sliders/Auto).
- [ ] Submit creates a work item whose payload bundles: instruction, selected
      element metadata, current component metadata, route, active tab, source
      file hints, active tokens, surrounding component context.

### Token editor tab — HIGH
- [ ] Selected element card + `TOKENS APPLIED` + only the token rows relevant to
      the element + primary blue `Apply changes`.
- [ ] Each row: category icon (purple), category label, **searchable** dropdown,
      current value. Dropdown lists **only** its category's tokens (typography,
      text-colour, radius, elevation, spacing, …). Irrelevant categories hidden.
- [ ] The searchable dropdown is **bespoke to the AI Inspector model** (open /
      query / active-index held in `AiInspector.Model`, keyed by row) — it does
      **not** use the still-unimplemented `selectStateful` control-renderer
      protocol. Token catalogues come from a static token map (categorised
      `(label, value)` lists) defined in the module.
- [ ] `Apply changes` creates a work item: selected element metadata, original
      token values, new token values, component/source metadata, intended files.

### Title-bar icon behaviour — MED
- [ ] No history + no active work → no history icon. History exists for current
      component → history icon. Work active → spinner replaces history icon
      (spinner always wins). Start-new-selection icon + close icon shown when
      relevant. Start-new-selection clears selection and re-enters selection
      mode; Close ends selection mode, clears selection, returns to default.

### Work history page — MED
- [ ] Opens in-panel replacing the tabs. Header: back button + `AI Inspector —
      Work history`. Back returns to previously active tab (default Agent chat).
      Sections: `CURRENTLY WORKING ON` (active items) then `RECENT HISTORY`
      (finished, newest first). No Current/History tabs. Rows: status icon,
      title, time, component/page, status label, chevron. Statuses
      `In progress` / `Completed` / `Failed`. Row click → Change details.

### Change details page — MED
- [ ] Opens in-panel. Header: back button + `Change details`; Back → Work
      history. Completed: status icon, title, status, completion time, summary,
      files changed, tokens updated (if any), elements affected. In-progress:
      status icon/spinner, title, status, start time, summary, files being
      changed, activity timeline (`Completed`/`In progress`/`Pending`/`Failed`).
      No Undo anywhere.

### Component-scoped work store (mocked) — HIGH
- [ ] `AiInspector.Model` holds `Dict componentId (List AiWorkItem)`. Work is
      scoped to the current component/page. History icon + Work history reflect
      the current component only.
- [ ] With no backend this pass, submitting a prompt or applying tokens creates
      a `queued` item that advances through `Analyzing request` → `Planning
      changes` → `Applying changes` → `Updating preview` → `completed` via timed
      effects (a `Tick`/delay effect), so all live/working/completed/failed
      states are demonstrable. A designated demo path yields `failed` to
      exercise the error UI.

### Side-nav spinner — MED
- [ ] While a component has active work, show a spinner beside its side-nav
      item. Browsing continues; navigating away keeps the spinner on the
      affected component and the title-bar spinner/history reflects the current
      component only.

### Icons (FontAwesome) — MED
- [ ] Add a tiny library icon helper emitting `Html.i [ class "fa-regular
      fa-<name>" ] []` (weight variants as needed). Plain class strings, no host
      dependency. Inspect / start-new-selection = `square-dashed-circle-plus`.
      Others: sparkle, `circle-info`, `xmark`, chevrons, history
      (`clock-rotate-left`), `spinner` (spin), `circle-check`, `circle-xmark`,
      typography/text glyph. Renders wherever a FA kit is loaded (sage: yes;
      standalone examples need a kit added to `examples/index.html`).

### Accessibility — MED
- [ ] Inspect + Start-new-selection keyboard focusable; close buttons labelled;
      tabs use tab semantics; work-history rows focusable; dropdowns labelled;
      status icons have accessible text; Esc cancels selection; focus returns
      sensibly on close; focus moves to the selected-element card on selection
      and to the page heading when opening Work history / Change details.

## Risks & Mitigations
- **Editing the heavily-changed `Application.elm`.** The branch already
  reworked it (+1980 lines). Mitigation: isolate the feature in
  `AiInspector.elm`; touch `Application.elm` only to add one model field, one
  msg case, the sticky render slot in `viewInspectorPanel`, the preview wrapper,
  and the side-nav spinner hook.
- **FA not loaded standalone.** Icons are invisible in the library's own
  examples unless a kit is added. Mitigation: add a kit to
  `examples/index.html`; document the dependency.
- **Selection metadata is DOM-bound.** Must live in JS. Mitigation: confine it
  to the `<cp-ai-selection>` custom element with a typed `cp-select` event
  contract decoded on the Elm side; start with reliably-available fields and
  degrade gracefully.
- **Cross-repo consumption.** sage must install the custom element and bump the
  flake pin. Mitigation: keep the sage-side surface to those two changes;
  document them.

## Deferred
- Backend task API (`POST/GET /api/ai-inspector/tasks`) and its Haskell types.
- Real Claude Code bridge that inspects/edits repo files and reports live status.
- SSE/websocket/polling transport (mock lifecycle stands in for now).
- Real preview hot-reload on completion (mock refreshes in-memory state only).
- Global (cross-component) history.

## Approach

Shell-level Elm feature in `Component.Application`, with the bulk isolated in a
new exposed-internal `Component/Application/AiInspector.elm` (model, msg,
update, view, token map, mock lifecycle). `Application` gains one
`aiInspector` field, threads one `AiInspectorMsg`, renders the sticky region as
the last child of `viewInspectorPanel` (flex layout so it pins to the bottom
and the body scrolls above), and wraps the live preview in `<cp-ai-selection>`.
Selection metadata is captured in JS by that custom element and decoded from a
`cp-select` event. Icons use a FontAwesome class-string helper. The searchable
token dropdown is bespoke to the AI Inspector model. Work items live in a
component-scoped in-memory dict; a timed effect simulates the task lifecycle so
every state in the mockup is reachable without a backend.

## Steps

Front-end pass — **done** (all slices compile; `elm-test` green, 16 AI Inspector
tests; `elm-review` clean for the new module).

- [x] **Type skeleton** — `Component/Application/AiInspector.elm` (exposed
      package module) with the full type set + `Application` wiring
      (`aiInspector` field, `AiInspectorMsg`, `subscriptions`, sticky dock).
- [x] **Slice 1** Default state (header / Inspect / footer).
- [x] **Slice 2** Selected + tabs (`Agent chat` / `Token editor`), selected
      card, blue underline.
- [x] **Slice 3** Work history + Change details (newest-first, working/finished
      split, activity timeline / files / tokens / elements).
- [x] **Slice 4** Title-bar spinner-vs-history (mutually exclusive, spinner
      wins).
- [x] **Slice 5** Mock lifecycle via `Tick` (animation-frame clock; 4 phases →
      Completed / Failed demo hook).
- [x] **Slice 6** Selection engine: `selectedDecoder` + `<cp-ai-selection>`
      wrapper in `Application` + `examples/src/cp-ai-selection.js` (hover
      overlay, capture-phase click, Esc-cancel, metadata extraction).
- [x] **Slice 7** Token editor + bespoke searchable, category-filtered dropdown
      + token catalogue (`tokenCatalogue`).
- [x] **Slice 8** Work creation from prompts (`SubmitPrompt`) and token applies
      (`ApplyChanges`) → in-memory items driven by the mock lifecycle.
- [x] **Slice 9** Side-nav spinner (`navTrailing`) for components with active
      work, so progress is visible while browsing elsewhere.
- [x] **Slice 10** Accessibility: tab roles + `aria-selected`, labelled icon
      buttons, `listbox`/`option` + labelled search (autofocus on open),
      status icons as labelled `img`, focus-target heading/card, Esc-cancel.

### Follow-ups (deferred)

- **Focus management on navigation.** Moving focus to the page heading (Work
  history / Change details) and to the selected-element card after selection
  needs `Browser.Dom.focus`, i.e. a `Cmd`. The library's `update` returns
  `(Model, List e)` with no `Cmd`, so this must be driven host-side (a port /
  effect) or by extending the update signature. Focus *targets* (`tabindex=-1`
  heading + card) are already in place.
- **sage integration.** Install `cp-ai-selection` in
  `js/src/ComponentPlayground/index.js`, wire `Component.Application.subscriptions`
  into sage's playground program, add the new `Theme` status tokens
  (`success`/`successBg`/`danger`/`dangerBg`/`tokenIcon`) to sage's theme, bump
  the flake pin, and fix any Elm compile errors from the widened API.
- **Backend + real Claude Code bridge** (see Deferred) replaces the mock
  lifecycle. (Work history is built only from real user actions — prompts and
  token applies; there is no seeded/fabricated history.)

## Verification
`elm-test` (126 pass incl. 17 AI Inspector), `elm-format`, `elm-review` (0
errors in the new module; 7 pre-existing repo errors untouched), examples
compile.

### Browser verification (done)

Ran the examples via `vite` and drove them with Playwright. Added an **AI
Inspector Demo** component (`examples/src/Index.elm`) whose markup carries the
`data-ai-inspectable` / `data-component` / `data-token-*` / `data-source-*`
hooks, so the selection engine has real metadata to capture (also serves as
living documentation of the annotation pattern).

Verified in-browser: default state; selecting mode (crosshair + `active`);
hover overlay; click capture; metadata extraction (tokens, source file, bounds,
selector); selected card; both tabs (blue underline); searchable
category-filtered token dropdown; Apply changes → work item; mock lifecycle
Analyzing→…→Completed; title-bar spinner→history (mutually exclusive);
work history (working/finished split, newest-first); completed **and**
in-progress change details; Back navigation with remembered tab; Esc
cancellation; side-nav spinner persisting across navigation; layout (docked
panel, independently scrolling body, pinned dock, no clipping); no console/page
errors.

**Two bugs found and fixed during verification:**

1. Navigating to another component left the *previous* component's selected
   element in view. Fixed with `resetForNavigation` (called from
   `Application.update`'s `ViewPage`): per-component view state resets on
   navigation; work/history persist.
2. The title-bar spinner used global `hasActiveWork`, so it showed on unrelated
   components. Fixed to be component-scoped (`isRunning` over `historyFor`);
   the work indicator now also appears in the default state when the current
   component has history/active work (empty for a fresh component, matching the
   mockup).

**Caveats:** icons are blank in the examples (FontAwesome *Free* lacks most
`fa-regular` glyphs — sage's Pro kit renders them); the bottom-right "0" badge in
dev is the `vite-plugin-elm` debugger, not part of the feature; focus-move-on-
navigation remains a host-side follow-up (see below).

In sage (later): `dev-check`, elm compile, and a manual walk-through of all six
mockup states in `/dev/components`.
