# Plan: Fix nested groups in sidebar — render the static tree structure

**Date:** 260413-1404
**Status:** Proposed.

---

## Summary

The sidebar nav renders nested `Playground.group`s incorrectly:

1. Groups and pages within a parent are alphabetically re-ordered, so the
   on-screen order doesn't match the source order in `previews`.
2. Nested groups render at the same horizontal level as their parent, so the
   tree shape isn't visually conveyed — a sub-group heading looks like a peer
   of the top-level group it belongs to.

Both issues are localised to `viewIndex` in
[src/Component/Application.elm:466-490](src/Component/Application.elm#L466-L490).

The fix:

- Drop the alphabetical sort. Render in source order — that's how the user
  wrote the tree, and that's the order they should see.
- Indent children of groups so the tree shape is visible.

---

## Reproducer

[examples/src/Index.elm:65-124](examples/src/Index.elm#L65-L124) defines:

```
Frame Types (group)
  ├─ fromComponent with wrap   (page)
  ├─ example with wrap         (page)
  └─ Gallery                   (group)
      ├─ Text field variants     (page)
      └─ Content block variants  (page)
```

Expected: the sidebar shows that exact tree, in that exact order, with the
two `Gallery` children visually nested under `Gallery`.

Observed (current screenshot):

```
Frame Types
Gallery
  Content block variants
  Text field variants

example with wrap
fromComponent with wrap
```

Two failures here:

- **Order is alphabetised by name.** `'G' < 'e' < 'f'` in ASCII, so `Gallery`
  floats to the top of `Frame Types`'s children, then `example with wrap`,
  then `fromComponent with wrap`. The internal Gallery children
  (`Content block variants`, `Text field variants`) are also alphabetised
  rather than source-ordered.
- **`Gallery` renders flush-left**, indistinguishable from `Frame Types`
  itself. There's no visual cue that it's nested inside `Frame Types`.

---

## Root cause

`viewIndex` at [src/Component/Application.elm:478-490](src/Component/Application.elm#L478-L490):

```elm
let
    filteredChildren =
        List.filter (indexHasMatch model.search) item.children
            |> List.sortBy (\(Index child) -> child.name)
in
if List.isEmpty filteredChildren then
    Html.text ""

else
    Ui.vStack [ Ui.style "margin-bottom" "0.5em" ]
        (Html.span (Ui.subHeadingStyles model.theme) [ Html.text item.name ]
            :: List.map (viewIndex model) filteredChildren
        )
```

Two problems:

- The `List.sortBy` is the source-order bug. There's no comment explaining
  why it's there; `git blame` will confirm whether it was deliberate or
  carried over from a flat-only era. Either way, it's wrong now that nested
  groups are first-class.
- The `Ui.vStack` containing the heading + children has no left-padding /
  margin / indentation applied to the children. The recursion just stacks
  the next heading flush-left.

---

## Fix

### 1. Drop the alphabetical sort

Change [src/Component/Application.elm:478-481](src/Component/Application.elm#L478-L481) to:

```elm
let
    filteredChildren =
        List.filter (indexHasMatch model.search) item.children
in
```

Source order in `previews` becomes the rendered order. This is what users
expect from "a static tree structure" — they wrote the tree, the sidebar
mirrors it.

### 2. Indent group children

Wrap the recursive children in a left-padded container so the tree shape is
visible. Replace the `else` branch with:

```elm
else
    Ui.vStack [ Ui.style "margin-bottom" "0.5em" ]
        [ Html.span (Ui.subHeadingStyles model.theme) [ Html.text item.name ]
        , Ui.vStack
            [ Ui.style "padding-left" "12px" ]
            (List.map (viewIndex model) filteredChildren)
        ]
```

`12px` matches the existing sidebar paddings (the sidebar content uses
`padding "12px 24px"` at [src/Component/Application.elm:424](src/Component/Application.elm#L424)). Each level of
nesting compounds, so a doubly-nested group naturally shifts further right
without needing depth-aware rendering — just structural recursion.

### 3. (Optional, defer) Depth-aware heading style

After §1+§2 the tree is correct and readable. A nice-to-have is making
deeper sub-group headings visually subordinate (smaller / lower-weight)
rather than every group using `subHeadingStyles`. That requires either:

- Threading a depth counter through `viewIndex`, or
- Adding a `subSubHeadingStyles` token to `Theme`.

Both are larger changes than the current bug warrants. Defer until there's
a real call for three-level-deep nesting in a published example. Note it
here so we don't forget the option exists.

---

## Files touched

| File | Change |
|------|--------|
| `src/Component/Application.elm` | Drop sort, indent children in `viewIndex` |

No new files. No type changes. No public API changes.

---

## Verification

After applying the change:

- `cd examples && npx elm make src/Index.elm --output=/dev/null` — must
  compile.
- Open the example and confirm the `Frame Types` group renders as:

  ```
  Frame Types
    fromComponent with wrap
    example with wrap
    Gallery
      Text field variants
      Content block variants
  ```

  with a visible left-indent on each nested level.
- Confirm `Components` (a single-level group) still renders correctly with
  its 8 pages indented under the heading.
- Confirm search filtering still works: typing into the search box should
  hide non-matching pages and collapse empty groups (the existing
  `indexHasMatch` / `List.isEmpty filteredChildren` logic is unchanged).
- `npx elm-test tests/` — no test should regress (no tests target
  `viewIndex` directly today, but run the suite to be safe).
- `npx elm-format --yes src/` and `npx elm-review`.

---

## Not changing

- `extractDefs`, `extractGroup`, `toIndex`, `flattenIndex` — all correctly
  preserve source order already; they're not the bug.
- `indexHasMatch` and the search-filter behaviour.
- `Theme` tokens — no new tokens needed for this fix.
- `Component.Playground` API — `group` is fine; the tree it produces is
  already correct, the renderer was just collapsing it.

---

## Risks / things to watch

- **Alphabetical order was possibly intentional for the flat case.** The
  top-level `Components` group has 8 unsorted pages; today they happen to
  be in alphabetical order in the screenshot, which suggests either the
  source already lists them alphabetically or the sort is masking source
  order there too. Check [examples/src/Index.elm:55-64](examples/src/Index.elm#L55-L64) — if the source
  order differs from the rendered order, removing the sort will visibly
  re-order that group. That's the correct behaviour ("static tree
  structure"), but worth flagging in the commit message so it isn't
  surprising.
- **Indentation interacts with the sidebar's fixed `300px` width**
  ([src/Component/Application.elm:415](src/Component/Application.elm#L415)). At 12px per level, a tree four
  levels deep eats 48px of horizontal space — page link text could start
  to wrap. Fine for the current example tree (max depth 2), but if
  consumers nest deeply we may want to revisit either the indent unit or
  the sidebar width. Not blocking.
