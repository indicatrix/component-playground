
# Fix exploreFrame wrapper leaking into componentRef lookups

`Component.exploreFrame` registers its wrapped component under the same id
as the underlying component. When another component embeds the same
component via `Control.componentRef`, the library lookup returns the
wrapped version — so the `exploreFrame`'s chrome (background, padding,
border-radius) bleeds into every embedded usage elsewhere in the
playground.

## Repro

`examples/src/Index.elm` has:

- `Combination Element` page — uses `Component.explore Components.comboElement`,
  which embeds `Components.textField` via `componentRef`.
- `exploreFrame (with wrapper)` page — uses
  `Component.exploreFrame (\inner -> Html.div [bg: #1a1a2e, padding: 32px, radius: 8px] [inner]) Components.textField`.

Both frames register under the same id `"text-field"`. The `exploreFrame`
page comes second in the tree, so `Dict.fromList` in `extractLibrary`
keeps *its* wrapped def. Combination Element's embedded text fields
render with the `exploreFrame` wrapper applied — dark bg, oversized
padding, rounded corners — even though Combination Element has nothing to
do with that wrapper.

## Root cause

[src/Component/Application.elm:130-131](../src/Component/Application.elm#L130-L131):

```elm
defDict =
    Dict.fromList (List.map (\d -> ( d.id, d.def )) defs)
```

Combined with [src/Component.elm](../src/Component.elm):

```elm
exploreFrame wrapper (Component_ c) =
    InteractiveFrame { id = c.id, name = c.name }
        (\lib -> ... { base | render = \lookup -> ( wrapper html, portals ) } ...)
```

The frame stores one function that, when run, produces a `ComponentE`
whose `render` is *already wrapped*. `defDict` uses that same function
for `componentRef` lookups, so wrapped render leaks everywhere the id is
referenced.

## Principle

> The component is the component. A frame is how it's displayed here.

A frame should be allowed to decorate its own presentation without
mutating the identity of the component it's presenting. Other frames or
components looking up the id should see the undecorated component.

## Fix — split the wrapper out of the stored `ComponentE`

Change `InteractiveFrame` (and `ExampleFrame`, which has the same shape)
to carry the wrapper as a separate, opt-in field. Library lookup
(`lookupDef`) returns the *unwrapped* `ComponentE`. Frame rendering
(`viewInteractiveFrame`) applies the wrapper after calling
`componentE.render`.

### Type changes — `src/Component/Internal.elm`

```elm
type Frame e t
    = InteractiveFrame
        { id : String, name : String }
        (Library e t -> State Ref (ComponentE e t))
        (Html (Update t e) -> Html (Update t e))   -- wrapper; identity for plain explore
    | ExampleFrame
        { id : String, name : String }
        String
        (Library e t -> State Ref (ComponentE e t))
        (Html (Update t e) -> Html (Update t e))   -- wrapper; identity for plain example
    | StaticFrame (Html (List e))
    | GalleryFrame String (Library e t -> State Ref (Html (List e)))
```

`Frame` is an internal type — `Component.Internal` is not exposed — so
changing its constructors is not a public API break.

### `src/Component.elm`

- `explore` → constructs `InteractiveFrame` with `identity` as the wrapper.
- `exploreFrame` → stops touching `base.render`; passes the user's
  `wrapper` as the third field. The def stored in the frame is the
  unwrapped `makeComponentE c b`.
- `example` → `ExampleFrame` with `identity` wrapper.
- (No `exampleFrame` exists yet; if/when added, same shape.)

### `src/Component/Application.elm`

- `extractDefs` continues to key defs by id — now keying to the
  *unwrapped* def (what's stored inside `InteractiveFrame`). `componentRef`
  lookups therefore return the plain component.
- `processFrame` / `viewFrame` → after calling the frame's def to get the
  `ComponentE`, apply the frame's wrapper to the `render` output before
  passing to `viewInteractiveFrame`. Equivalently, thread the wrapper
  through `ProcessedInteractive` / `ProcessedExample` so
  `viewInteractiveFrame` can apply it at render time.

The cleanest landing point is inside `viewInteractiveFrame` at the place
that currently does:

```elm
, Html.div []
    [ internals.render lookup
        |> Tuple.first
        |> Html.map ComponentUpdate
    ]
```

becoming something like:

```elm
, Html.div []
    [ internals.render lookup
        |> Tuple.first
        |> wrapper
        |> Html.map ComponentUpdate
    ]
```

where `wrapper` is the per-frame field threaded through.

### `componentRef` stays unchanged

`Control.componentRef` already looks up via `lib.lookupDef`; once
`lookupDef` returns the unwrapped def, no change needed there.

## Implementation steps

- [ ] `src/Component/Internal.elm`: add wrapper field to `InteractiveFrame`
      and `ExampleFrame` constructors.
- [ ] `src/Component.elm`:
      - `explore`: pass `identity` wrapper.
      - `exploreFrame`: stop wrapping `base.render`; pass `wrapper` as
        third constructor arg.
      - `example`: pass `identity` wrapper.
- [ ] `src/Component/Application.elm`:
      - Update pattern matches on `InteractiveFrame` / `ExampleFrame` to
        destructure the new wrapper field.
      - Thread wrapper through to `viewInteractiveFrame` (via
        `ProcessedInteractive` / `ProcessedExample` payload or a model
        lookup).
      - Apply wrapper in `viewInteractiveFrame` at the render-output site.
- [ ] Compile library + examples.
- [ ] Manually verify:
      - `Combination Element` — embedded text fields render as plain
        text fields (no dark bg, no 32px padding, no rounded corners).
      - `exploreFrame (with wrapper)` — text field still renders
        wrapped with `#1a1a2e` / padding / radius.
      - Other pages unchanged.

## Non-goals

- **Warn on duplicate ids.** Out of scope here — we're fixing the
  semantic bug, not the UX around accidentally shadowing an id.
- **Revisiting the `defDict` dedup strategy.** Same id still collapses
  via `Dict.fromList`; the point is that all frames for the same
  component *should* register the same underlying def, so collapsing
  is fine once wrappers are out of the stored def.
- **A `componentWithFrame` / `withExample` registration-only API** for
  components that have no playground page. Could come later; current
  registration-via-frame keeps working.
