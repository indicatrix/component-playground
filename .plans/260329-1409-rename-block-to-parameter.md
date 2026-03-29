# Rename Block → Model

## Decision

Rename the `Block` concept to `Model`, and introduce a `Model` module as the public API for building models.

## Rationale

"Block" is implementation-flavored. "Model" reflects the standard Elm MVU framing: a component playground entry is defined by a **Model**, a **View**, and **Stories**. The playground exposes model fields as interactive controls at runtime — the same values an Elm programmer already manages in their `Model` type.

## API Shape

```elm
Component.new MyComponent
    |> Component.withModel "Opacity" Model.float
    |> Component.withInternalModel Model.float
    |> Component.withPreviewModel "Inner" Model.preview
```

- `Component.withModel` takes a label and a model — the label belongs at the call site because it only matters when there's a UI control to display it on
- `Component.withInternalModel` takes no label — hidden models have no UI, so no label is needed
- `withInternalModel` is cleaner than `withHiddenModel` — "internal" describes what the model *is*, not just that it's hidden

## Preview models (embedded components)

`Model.preview` needs `Library` to render a component selector. Options explored:

- **`list2`-style** — keep `Library -> Model` as a special shape, use `list2` combinator to thread it. Non-uniform.
- **Library on all models** — every model becomes `Library -> Model`. Uniform but noisy for the 95% case.
- **`Component.withLib` wrapper** — `Model.preview` is a plain value; callers wrap with `Component.withLib` to resolve the `Library` dependency at the `withModel` call site. `list2` goes away, combinators stay uniform.
- **Library in BlockI_ thunks** — push Library into the internals so `Model.preview` resolves it transparently. `Model.list Model.preview` works with no call-site wrapper at all.

**Library-in-thunks is the preferred direction** and the prerequisite spike before collapsing entities. See spike goal below.

## Entry / registration (replaces toPreview)

Components are defined as Model + View + Stories; meta is added at registration in `Playground.elm`:

```elm
-- Button.elm
model : Model e t { opacity : Float }
model =
    Model.record { opacity = 1.0 }
        |> Model.field "Opacity" .opacity Model.float

stories : List ( String, { opacity : Float } )
stories =
    [ ( "Default", { opacity = 1.0 } )
    , ( "Faded", { opacity = 0.3 } )
    ]

-- Playground.elm
entries =
    [ Entry.entry { id = "button", name = "Button" } Button.view Button.model Button.stories
    , Entry.entry { id = "card", name = "Card" } Card.view Card.model Card.stories
    ]
```

- `toPreview` and `toPortalPreview` are replaced by `Entry.entry` and `Entry.portal`
- Stories are `List ( String, m )` — named initial states, always interactive via the model's update loop
- `Component.Basic` and `Component.WithPortals` are type aliases that produce helpful type errors

## Stories

Stories are plain named initial states, not snapshots. They are always interactive: the model's update loop (if any) applies to all stories automatically. There is no distinction between "snapshot" and "interactive" stories — a story without an update loop simply doesn't respond to messages.

## Model builders

Mirror the existing block builders, namespaced under `Model`:

- `Model.float`
- `Model.string`
- `Model.int`
- `Model.bool`
- `Model.oneOf`
- `Model.list`
- `Model.custom`
- `Model.preview` (library-dependent — resolved via Library thunks in internals)
- `Model.withUpdate` — attaches an update loop to the model

## Docs note

Worth adding a short explainer in the readme or module docs:

> A `Model` defines interactive controls — the values the playground lets you tweak at runtime, along with an optional update loop. An `Entry` wires a Model together with a view function and named stories.

## Open questions

1. **Library-in-thunks shape** — what does the `BlockI_` change look like? The second constructor stores a `Library -> BlockI_` thunk. The spike must validate that `Model.list Model.preview` resolves Library correctly through `list`'s internal `State Ref` traversal, and that no call-site wrapper is needed.

2. **`Model.withUpdate` type** — the update loop introduces a `msg` type variable. How is this surfaced (or hidden) in `Model e t m`? Can `withUpdate` keep `msg` existential, or does it need to be part of the type?

3. **`Model` namespace collision** — users will have their own `Model` types. The module should be used qualified (`Model.float`, `Model.string` etc.). Document this clearly.

4. **`Entry` module location** — top-level `Entry` module, or `Component.Entry`? Depends on whether `Entry` needs access to internals.

## Spike goal (prerequisite)

Before full rename and entity collapse, spike Library-in-thunks:

- Add a second constructor to `BlockI_` for library-dependent blocks: `LibraryBlock (Library -> BlockI_ ...)`
- Implement `Model.preview` using this constructor
- Validate `Model.list Model.preview` resolves Library correctly with no call-site wrapper
- Validate that plain model combinators (`Model.string`, `Model.float`, etc.) are unaffected

## Scope (after spike)

- Rename internal `Block`/`BlockI` types in `Component.Internal`
- Update `Component.elm` public API (`withControl` → `withModel`, block builders → `Model.*`)
- Create `src/Model.elm` (or expose via `Component.Model`)
- Add `Component.Basic` and `Component.WithPortals` type aliases
- Replace `toPreview`/`toPortalPreview` with `Entry.entry`/`Entry.portal`
- Update `Component.Application` if it references block types directly
- Update elm.json exposed modules
