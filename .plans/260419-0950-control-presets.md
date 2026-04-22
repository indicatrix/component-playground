
# Control presets

**Date:** 260419-0950
**Status:** Implemented (commits `5829879`, `21f0004`). Revised during
implementation — `ExampleFrame` removed (subsumed by `PresetsFrame`);
`Frame.presetGallery` built on top of `Frame.gallery` rather than being its
own variant; per-preset wrap added.

---

## Objective

Let a component declare a list of **named presets** — canonical
configurations of its storage state. Consumers pick a preset to replace
the whole state at once (a chart exposes "Bar", "Line", "Pie" rather
than requiring callers to configure chart type, axes, and data
individually). **Custom** is always available as an escape back to
free-form editing.

The Dashboard embedding case — a dashboard embeds a chart via
`componentRef` and wants its users to pick between chart presets
without seeing every chart field — is the load-bearing motivation. The
preset picker must therefore **travel with the component** when
embedded, which means the picker state must live in refs, not in
`Application.Model` UI state.

Two first-class frame treatments layer on top:

1. **`Frame.presets`** — tab bar across the top. Suppresses the
   in-controls picker (the tab bar replaces it). Controls panel always
   shows the underlying component controls.
2. **`Frame.presetGallery`** — renders one non-interactive variant per
   preset, side-by-side, labelled. Implemented as a convenience over
   `Frame.gallery`, so users needing custom layouts can drop down to
   `Frame.gallery` directly.

Breaking API changes are acceptable.

---

## Where state lives

Two pieces of state:

- **Preset slot.** One new `Ref` allocated per preset-using component
  instance. Stores `Type.StringValue name` for a named preset or
  `StringValue ""` / absent for Custom. Encoded as `Maybe String` in
  Elm (`Nothing` ≡ Custom).
- **Component's existing refs.** Unchanged. When a named preset is
  picked, its `i` is pushed through `inner.toType` and written wholesale.

Because the preset slot is a ref, `componentRef` picks it up for free:
embedding a preset-bearing component renders its picker inline with its
controls, exactly like any other control.

---

## `Preset` record and per-preset wrap

Each preset carries a name, value, and a wrap function for custom
chrome (height/width/background containers, chart-type-specific sizing,
etc.):

```elm
type alias Preset t i =
    { name : String
    , value : i
    , wrap : Html (Update t) -> Html (Update t)
    }


Component.preset : String -> i -> Preset t i
Component.preset name value =
    { name = name, value = value, wrap = identity }


Component.withPresets
    : List (Preset t i)
    -> Component_ e t i m (Update t)
    -> Component_ e t i m (Update t)
```

Call-site ergonomics:

```elm
chart
    |> Component.withPresets
        [ Component.preset "Bar" barConfig
        , Component.preset "Line" lineConfig
        , { name = "Sparkline"
          , value = sparklineConfig
          , wrap = Html.div [ Ui.style "height" "40px" ] << List.singleton
          }
        ]
```

The wrap applies whenever the component is rendered *as that preset*:

- **`Frame.presets` interactive**: active named tab → wrap the view;
  Custom tab → no wrap.
- **`Frame.presetGallery`**: each preset's view gets its wrap.
- **`Frame.fromComponent` (no preset tab UI)**: no wrap. The in-controls
  picker changes storage but the rendered view isn't known to be "at a
  preset" — current state may or may not match one.

(Open question: should `Frame.fromComponent` also apply the wrap when
the preset slot reads as a named preset? Symmetric with `Frame.presets`
but mildly magical. Default: no, leave `Frame.fromComponent` unwrapped.)

---

## Rename: current `withPresets` → `fromOptions`

Unchanged from earlier revisions.

| Current | New |
|---------|-----|
| `Control.withPresets : String -> (a, String) -> List (a, String) -> Control e t a` | `Control.fromOptions : String -> (a, String) -> List (a, String) -> Control e t a` |

Call-sites: `Control.bool`, `tests/ControlTests.elm`,
`tests/ControlBuilderTests.elm`, `examples/src/Components.elm`
(`contentBlock`), docstrings.

---

## `Internal.ComponentE` shape

```elm
type alias ComponentE e t =
    { render : Lookup t -> View (Update t)
    , controls : Theme -> Lookup t -> List (Html (Update t))
    , innerControls : Theme -> Lookup t -> List (Html (Update t))
    , update : Lookup t -> Lookup t -> ( List ( Ref, Type t ), List e )
    , presets : Maybe (PresetsInfo t)
    }


type alias PresetsInfo t =
    { names : List String
    , current : Lookup t -> Maybe String
    , pick : Maybe String -> List ( Ref, Type t )
    , renderAt : String -> Lookup t -> Html (Update t)
    , wrapAt : String -> Html (Update t) -> Html (Update t)
    }
```

- `names` — preset names in declaration order. Custom is appended in
  the UI layer.
- `current` — reads the preset slot, returns `Just name` if a known
  preset, `Nothing` otherwise (Custom, absent, or stale).
- `pick` — change list to dispatch for a preset choice. `pick (Just
  "Bar")` = `(slotRef, StringValue "Bar") :: inner.toType barValue`;
  `pick Nothing` = `[(slotRef, StringValue "")]`.
- `renderAt name lookup` — non-interactive render of the component as
  if that preset were active, using a lookup overlay. Used by
  `Frame.presetGallery`. Messages target a sentinel instance.
- `wrapAt name` — returns the named preset's wrap function, or
  `identity` if the name isn't found.
- `controls` = picker :: `innerControls` when presets exist, else
  equal.

Components without presets: `presets = Nothing`, `controls ==
innerControls`. Zero behavioural cost.

---

## `Internal.Component_` record

```elm
type Component_ e t i m msg
    = Component_
        { id : String
        , name : String
        , controls : Control e t i m
        , view : i -> m -> (i -> msg) -> View msg
        , presets : List (Preset t i)
        }
```

All existing constructors default `presets = []`. `Component.withPresets`
replaces it. Opaque constructor, so no downstream break.

Caveat: the `t` parameter in `Preset t i` is the same `t` the component
already has, so threading the new field through is straightforward.

---

## Frame variants — fewer than the last draft

`ExampleFrame` is removed. `PresetsFrame` subsumes it: any frame that
wants a pinned initial state uses `Component.withPresets` + one
preset. Migration:

```elm
-- before
Frame.example initial myComponent

-- after
myComponent
    |> Component.withPresets [ Component.preset "Example" initial ]
    |> Frame.presets
```

This does change the UX slightly (a tab bar appears where none did
before), but the concept is the same and the tab name gives the former
frame a clearer label.

`PresetGalleryFrame` is also removed. `Frame.presetGallery` is a thin
helper over `Frame.gallery`:

```elm
presetGallery : Component_ e t i m (Update t) -> Frame e t
presetGallery ((Component_ c) as component) =
    Frame.gallery component
        (\render ->
            Html.div [ flexWrapStyling ]
                (List.map
                    (\p ->
                        Html.div []
                            [ Html.div (Ui.subHeadingStyles ...) [ Html.text p.name ]
                            , p.wrap (render p.value)
                            ]
                    )
                    c.presets
                )
        )
```

Users wanting a different layout skip `presetGallery` and call
`Frame.gallery` with their own assembler. The preset list is reachable
via pattern-matching `Component_ c` on the public component — same
pattern the helper uses internally.

Net change to `Internal.Frame`: **one new variant (`PresetsFrame`), one
deletion (`ExampleFrame`)**:

```elm
type Frame e t
    = InteractiveFrame ...
    | StaticFrame ...
    | GalleryFrame ...
    | SubheadingFrame ...
    | PresetsFrame { id : String, name : String }
        (Library e t -> State Ref (ComponentE e t))
        (Html (Update t) -> Html (Update t))
```

`PresetsFrame` mirrors `InteractiveFrame`'s shape so
`Application.extractDefs` can index it for `componentRef` lookups.
`Frame.wrap` on `PresetsFrame` wraps the component view (same as
`InteractiveFrame`).

---

## `Frame.presets` — layout and behaviour

```elm
Frame.presets : Component_ e t i m (Update t) -> Frame e t
```

1. **Tab bar** — `presets.names` + trailing Custom. Active tab
   highlight reads `presets.current lookup`. Clicks dispatch `Update
   instance (presets.pick choice)`. Because this goes through the
   normal `ComponentUpdate` message path, `withUpdate` callbacks see
   the transition — important for maintaining component-internal
   invariants on state change (confirmed).
2. **Component view** — `componentE.render lookup`, then
   `presets.wrapAt name` if a named tab is active (Custom = identity).
3. **Controls panel** — `componentE.innerControls` (picker suppressed).
   Settings-2 toggle still applies.

Editing a control after picking "Bar" leaves the slot at "Bar" — tab
stays highlighted while state diverges. Clicking any tab (including
Custom) resolves it.

---

## `Frame.presetGallery`

```elm
Frame.presetGallery : Component_ e t i m (Update t) -> Frame e t
```

Default layout: a `flex`-wrap row where each item is `<subheading with
preset name>` over `<preset.wrap (render preset.value)>`. Built on
`Frame.gallery`, which already uses the sentinel instance for
non-interactive rendering and already handles `Frame.wrap` correctly.

No Custom entry.

---

## `Application` changes

`ProcessedFrame` gains one new variant, `ProcessedPresets` (mirroring
`ProcessedInteractive`), with a dedicated `viewPresetsFrame`.
`ProcessedExample` is deleted. `Msg`/`Model` don't change — preset
state is in refs, gallery rendering goes through the existing gallery
path.

Case-analysis sites: `Frame.wrap`, `processFrame`, `extractDefs`,
`viewFrame` each lose one case (`ExampleFrame`) and gain one
(`PresetsFrame`). Net wash.

---

## Public API summary

| Symbol | Change |
|--------|--------|
| `Control.withPresets` | Renamed to `Control.fromOptions`. |
| `Component.Preset` | **New** type alias. |
| `Component.preset` | **New** helper (default-wrap constructor). |
| `Component.withPresets` | **New** modifier. |
| `Component.Frame.example` | **Removed**. Use `Component.withPresets` + `Frame.presets`. |
| `Component.Frame.presets` | **New**: tab-bar frame. |
| `Component.Frame.presetGallery` | **New**: helper over `Frame.gallery`. |
| `Component.Playground.presets` / `presetGallery` | Re-exports. |
| `Internal.Frame.ExampleFrame` | **Removed**. |
| `Internal.Frame.PresetsFrame` | **New** variant. |
| `Internal.Component_.presets` | **New** field. |
| `Internal.ComponentE` | New `presets`, `innerControls` fields. |

---

## Implementation steps

- [ ] Rename `Control.withPresets` → `Control.fromOptions`; update
      every call site, test, and docstring.
- [ ] Add `Preset` type alias and `Component.preset` helper.
- [ ] Add `presets : List (Preset t i)` to `Internal.Component_`;
      default `[]` in all `Component.component*` constructors.
- [ ] Add `Component.withPresets`.
- [ ] Extend `Internal.ComponentE` with `presets : Maybe PresetsInfo`
      and `innerControls`; define `PresetsInfo`.
- [ ] In `Frame.makeComponentE`: when `comp.presets` is non-empty,
      allocate a preset slot ref, precompute each preset's `toType`,
      build `pick` / `current` / `renderAt` / `wrapAt`, prepend the
      picker to `controls`. First preset becomes `b.default`.
- [ ] Remove `ExampleFrame` from `Internal.Frame` and `Frame.example`
      from `Component.Frame`. Remove `ProcessedExample` from
      `Application`.
- [ ] Add `PresetsFrame` variant; implement `Frame.presets`; handle in
      `Frame.wrap`, `processFrame`, `extractDefs`, `viewFrame`.
- [ ] Implement `Frame.presetGallery` as a helper calling
      `Frame.gallery` with a per-preset-subheading flex-wrap
      assembler that applies each preset's `wrap`.
- [ ] Add `viewPresetsFrame` — tab bar, component view with
      `wrapAt`, controls pane using `innerControls`, settings-2
      toggle.
- [ ] Re-export `presets` and `presetGallery` from `Component.Playground`.
- [ ] Update examples: migrate any `Frame.example` call-sites; add a
      chart-like example component with three presets including one
      with a custom `wrap`; show it via both `Frame.presets` and
      `Frame.presetGallery`; add a dashboard component that embeds it
      via `componentRef` to demonstrate the picker travelling into
      embedded controls.
- [ ] Tests: presets thread into `ComponentE`; first preset is the
      default; `pick` round-trips; in-controls picker and tab bar
      produce identical change lists; `renderAt` overlays correctly;
      `wrapAt` returns identity for unknown names; `presetGallery`
      renders all presets.
- [ ] `npx elm-format --yes src/ tests/`, `npx elm-test tests/`, `npx
      elm-review`, compile `examples/`.

---

## Resolved open questions

1. **Preset picks fire `withUpdate`.** Tab-bar and in-controls picker
   both dispatch through `Update instance` / `componentE.update`, so
   `withUpdate` callbacks run and can maintain component-internal
   invariants.
2. **Gallery wrap.** `Frame.wrap` on `presetGallery` matches
   `GalleryFrame` (whole-gallery), because `presetGallery` *is* a
   `GalleryFrame` underneath.
3. **Stale preset names.** `current` returns `Nothing` — UI treats as
   Custom. Picking any tab rewrites.
4. **Custom layouts.** `Frame.gallery` is the escape hatch. Per-preset
   wrap on the `Preset` record covers the common case (sizing chrome
   per variant) without requiring a full custom assembler.

## Remaining open question

- **Wrap in `Frame.fromComponent`.** Should the active preset's wrap
  apply when the component is rendered via `Frame.fromComponent` (no
  tab UI) if the preset slot happens to hold a named preset? Plan says
  no — `fromComponent` stays unwrapped; if you want preset-aware
  rendering, use `Frame.presets`. Flag if you'd rather have it
  symmetric.
