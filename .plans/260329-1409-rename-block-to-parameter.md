# Rename Block → Parameter

## Decision

Rename the `Block` concept to `Parameter`, and introduce a `Parameter` module as the public API for building parameters.

## Rationale

"Block" is implementation-flavored. "Parameter" better describes what these values represent: the inputs a component accepts, which the playground exposes as interactive controls at runtime.

## API Shape

```elm
Component.new MyComponent
    |> Component.withParameter "Opacity" Parameter.float
    |> Component.withInternalParameter Parameter.float
    |> Component.withPreviewParameter "Inner" (Component.withLib Parameter.preview)
```

- `Component.withParameter` takes a label and a parameter — the label belongs at the call site because it only matters when there's a UI control to display it on
- `Component.withInternalParameter` takes no label — hidden parameters have no UI, so no label is needed
- `Parameter.hidden` was considered but rejected: with the label at the call site, there's nowhere natural for it to go on a hidden parameter
- `withInternalParameter` is cleaner than `withHiddenParameter` — "internal" describes what the parameter *is*, not just that it's hidden

## Preview parameters (embedded components)

`Parameter.preview` needs `Library` to render a component selector. Options explored:

- **`list2`-style** — keep `Library -> Parameter` as a special shape, use `list2` combinator to thread it. Non-uniform.
- **Library on all parameters** — every parameter becomes `Library -> Parameter`. Uniform but noisy for the 95% case.
- **`Component.withLib` wrapper** — `Parameter.preview` is a plain value; callers wrap with `Component.withLib` to resolve the `Library` dependency at the `withParameter` call site. `list2` goes away, combinators stay uniform.

`Component.withLib` is the preferred direction but needs an implementation spike to validate.

## Entry / registration (replaces toPreview)

Components are defined without meta; meta is added at registration in `Playground.elm`:

```elm
-- Button.elm
component : Component.Basic e t
component =
    Component.new view
        |> Component.withParameter "Opacity" Parameter.float

-- Playground.elm
entries =
    [ Entry.entry { id = "button", name = "Button" } Button.component
    , Entry.entry { id = "card", name = "Card" } Card.component
    ]
```

- `toPreview` and `toPortalPreview` are replaced by `Entry.entry` and `Entry.portal`
- `Component.Basic` and `Component.WithPortals` are type aliases that produce helpful type errors

## Parameter builders

Mirror the existing block builders, namespaced under `Parameter`:

- `Parameter.float`
- `Parameter.string`
- `Parameter.int`
- `Parameter.bool`
- `Parameter.oneOf`
- `Parameter.list`
- `Parameter.custom`
- `Parameter.preview` (needs `Library` — see open questions)
- etc.

## Docs note

Worth adding a short explainer in the readme or module docs:

> A `Parameter` defines an interactive control — a value the playground lets you tweak at runtime. A `Component` wires those parameters together with a view function.

## Open questions

1. **`Component.withLib` shape** — what does the type actually look like? Does it wrap `Library -> Parameter` into a plain `Parameter`, or does `withParameter` grow an overload? Needs an implementation spike before committing to `BlockI` changes.

2. **`Parameter.list` + preview** — can `Parameter.list (Component.withLib Parameter.preview)` work without `list2`? The spike should try this combination explicitly.

3. **`Component.withLib` naming** — `withLib` is a bit terse. `Parameter.withLibrary`? `Parameter.usingLibrary`? Decide once the shape is clearer.

4. **`Entry` module location** — top-level `Entry` module, or `Component.Entry`? Depends on whether `Entry` needs access to internals.

## Spike goal

Before doing the full rename, write a small proof-of-concept in a scratch file that shows:
- `withParameter` / `withInternalParameter` working with plain parameters
- `Component.withLib` (or equivalent) making `Parameter.preview` composable with `Parameter.list`
- No changes to `BlockI` internals required

## Scope (after spike)

- Rename internal `Block`/`BlockI` types in `Component.Internal`
- Update `Component.elm` public API (`withControl` → `withParameter`, block builders → `Parameter.*`)
- Create `src/Parameter.elm` (or expose via `Component.Parameter`)
- Add `Component.Basic` and `Component.WithPortals` type aliases
- Replace `toPreview`/`toPortalPreview` with `Entry.entry`/`Entry.portal`
- Update `Component.Application` if it references block types directly
- Update elm.json exposed modules
