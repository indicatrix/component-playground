
# Component.Application UI redesign

**Date:** 260418-2212
**Status:** Proposed.

---

## Goal

Redesign the runner UI to match the Figma at
`https://www.figma.com/design/4ZzWENF4jwtrfKTC5o1Qns/Component-Playground?node-id=15-2`.

Key visual changes:
- Sidebar: branded header (icon + title), dedicated search band, component
  index, optional footer. Header and footer configurable by the caller.
- Page: large page title at the top, then a vertical stack of frames.
- Frames: optional inline subheadings between frames (new frame type).
  Interactive/example frames get a settings-2 toggle that hides their
  controls pane; controls hidden by default.
- Icons: use inline lucide SVGs (`blocks`, `search`, `settings-2`) from
  `Component.Ui`.

Breaking API changes are acceptable.

---

## Changes

### 1. New `SubheadingFrame`

`Component.Internal`:

```elm
type Frame e t
    = InteractiveFrame { id : String, name : String } (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | ExampleFrame { id : String, name : String } String (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | StaticFrame (Html (Update t))
    | GalleryFrame String (Library e t -> State Ref (Html (Update t)))
    | SubheadingFrame String
```

`Component.Frame`:

```elm
subheading : String -> Frame e t
subheading label =
    Internal.SubheadingFrame label
```

Re-export from `Component.Playground` alongside the other frame
constructors.

`Frame.wrap` is a no-op on `SubheadingFrame` (same as it would be on
`StaticFrame` if there were no HTML — subheading carries no wrappable
content).

**Removing the per-frame inline subhead:** `ExampleFrame` currently renders
its display `name` as a subhead at the top of `viewInteractiveFrame`. That
rendering goes away — callers who want a subhead add `Frame.subheading`
before their frame. `ExampleFrame`'s `name` argument is dropped from the
constructor (was only used for this rendering). `GalleryFrame` similarly
loses its rendered name: drop the `String` argument, use
`Frame.subheading` before the gallery if a label is wanted.

Updated `Internal.Frame`:

```elm
type Frame e t
    = InteractiveFrame { id : String, name : String } (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | ExampleFrame { id : String, name : String } (Library e t -> State Ref (ComponentE e t)) (Html (Update t) -> Html (Update t))
    | StaticFrame (Html (Update t))
    | GalleryFrame (Library e t -> State Ref (Html (Update t)))
    | SubheadingFrame String
```

`Frame.example` loses its display-name argument; `Frame.gallery` loses
its name argument.

### 2. Lucide icon helpers in `Component.Ui`

Add inline SVG helpers (24x24, stroke-based, matching lucide source):

```elm
lucideBlocks : Html msg
lucideSearch : Html msg
lucideSettings2 : Html msg
```

Stroke color inherits from `currentColor` so theme text color applies.

### 3. Theme: configurable sidebar header + footer

`Component.Application.Theme`:

```elm
type alias Theme =
    { ...existing fields...
    , sidebarHeader : Html Never
    , sidebarFooter : Html Never
    }
```

Default values on the provided themes:

```elm
sidebarHeader =
    Html.div [ Ui.style "display" "flex", Ui.style "gap" "12px", ... ]
        [ Ui.lucideBlocks
        , Html.span [ ... ] [ Html.text "Component Playground" ]
        ]

sidebarFooter =
    Html.text ""
```

Callers override by record update on the theme.

### 4. Sidebar layout restructure

`Component.Application.viewSidebar` rebuilt as four stacked bands:

1. **Header band** — renders `theme.sidebarHeader |> Html.map never`.
   Bottom border `theme.sidebarDivider`.
2. **Search band** — `lucideSearch` icon + the existing search input,
   inline. Bottom border. (Replaces the search-inside-header layout.)
3. **Component index** — existing `viewIndex` output, scrollable if tall.
4. **Footer band** — renders `theme.sidebarFooter |> Html.map never`.
   Pinned to the bottom (sidebar becomes a flex column; index takes
   `flex-grow: 1`).

Remove the hardcoded "Library" heading.

### 5. Page layout: page title + stacked frames

`Component.Application.viewPage`:

1. **Page header band** — large heading with the current page's name
   (from `Playground.Page.name`). Bottom border for visual separation.
2. **Frame stack** — existing vertical stack of `viewFrame` outputs.

`viewFrame` dispatches on the new `SubheadingFrame` variant, rendering
`Html.div (Ui.subHeadingStyles theme) [ Html.text label ]` with the
same padding/border treatment as the Figma's "Page Subhead".

### 6. Controls toggle

**Model:**

```elm
type alias Model t e =
    { ...existing fields...
    , shownControls : Set String
    }
```

Empty on init — controls hidden by default on every interactive/example
frame.

**Msg:**

```elm
type Msg t e
    = ...
    | ToggleFrameControls String
```

Toggle membership in `shownControls` on that frame's id.

**View:**

`viewInteractiveFrame` takes the frame's id. Renders the component
view with a `settings-2` icon absolute-positioned in its top-right
corner; clicking dispatches `ToggleFrameControls id`. When the id is in
`shownControls`, also render the controls pane to the right
(existing layout); otherwise the component view spans full width.

Example frames use the same viewer (they always had — `viewFrame`
passes both through `viewInteractiveFrame`). Toggle applies to both.

State is in-memory only: no URL param, resets on reload.

### 7. Example updates

`examples/src/` needs updates for the signature changes:
- `Frame.example` call sites drop the display-name argument (replace with
  a preceding `Frame.subheading` if the label was wanted).
- `Frame.gallery` call sites drop the name argument (same treatment).

The existing "example with wrap" and "fromComponent with wrap" examples
become good showcases for the subheading frame by pairing each with a
`Frame.subheading` above it.

Add or adapt an example that shows a page with: page title → static intro
→ subheading → gallery → subheading → interactive frame (mirrors the
Figma layout).

### 8. Tests

- Compile the examples (`cd examples && npx elm make src/Index.elm`).
- Run `npx elm-test tests/`. Any tests that construct `ExampleFrame`
  / `GalleryFrame` directly or through the public API will need their
  arguments updated.
- `elm-review` and `elm-format` on `src/` and `tests/`.

---

## Out of scope

- Portals-count indicator (not part of this redesign; the "0" badge in
  the reference screenshot came from example code).
- URL persistence of the controls toggle state.
- Sidebar collapse / responsive layout.
- New theme tokens beyond header/footer — reuse existing divider,
  heading, subheading, background, and text tokens.

---

## Order of work

1. Add lucide icon helpers in `Component.Ui`.
2. Extend `Theme` with header/footer; update default themes.
3. Add `SubheadingFrame` + `Frame.subheading`; drop the name arguments
   from `ExampleFrame`/`GalleryFrame`; re-export.
4. Restructure `viewSidebar` (header/search/index/footer bands).
5. Add page-header band and wire `SubheadingFrame` into `viewFrame`.
6. Add `shownControls` / `ToggleFrameControls`; add settings-2 toggle
   icon and conditional controls rendering in `viewInteractiveFrame`.
7. Update examples + tests until everything compiles, tests pass,
   elm-review is clean.
