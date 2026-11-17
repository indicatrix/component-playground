module Component.Playground exposing
    ( Playground
    , Component_, Frame, Preset, Update
    , fromComponent, fromFrames, group
    , sortByName
    )

{-| Playground constructors.

A playground is a recursive tree of named pages and groups, producing the
navigable sidebar of the application. Pass a list of playgrounds to
`Component.Application.element` to run them.


# Type

@docs Playground


# Re-exported type aliases

@docs Component_, Frame, Preset, Update


# Constructors

@docs fromComponent, fromFrames, group


# Ordering

@docs sortByName

-}

import Component.Frame as Frame
import Component.Internal as Internal
    exposing
        ( Playground(..)
        )



-- TYPE RE-EXPORT


{-| A playground is a recursive tree of named pages and groups. Create with
`fromComponent`, `fromFrames`, or `group`.
-}
type alias Playground e t =
    Internal.Playground e t


{-| Re-export of `Component.Component_`. A component with potentially distinct
storage and output types. Accepted by `fromComponent`.
-}
type alias Component_ e t i m msg =
    Internal.Component_ e t i m msg


{-| Re-export of `Component.Frame.Frame`. Produced by `fromFrames` callers.
-}
type alias Frame e t =
    Internal.Frame e t


{-| Re-export of `Component.Preset`. A named preset configuration attached
to a component via `Component.withPresets`.
-}
type alias Preset t i =
    Internal.Preset t i


{-| Re-export of `Component.Update`. The message type produced by interactive
frames.
-}
type alias Update t =
    Internal.Update t



-- CONSTRUCTORS


{-| Sugar for a single-component page named after the component. Equivalent to:

    fromFrames { name = componentName } [ Frame.fromComponent component ]

-}
fromComponent : Component_ e t i m (Update t) -> Playground e t
fromComponent ((Internal.Component_ c) as component) =
    fromFrames { name = c.name } [ Frame.fromComponent component ]


{-| A named playground page containing a list of frames. The page's URL
segment is its name, slugged: lowercase, spaces to dashes, other punctuation
dropped (`Progress & Effects` → `progress-effects`).
-}
fromFrames : { name : String } -> List (Frame e t) -> Playground e t
fromFrames meta frames =
    Page meta frames


{-| A named group of playground pages or sub-groups. The group's slugged name
prefixes its children's URL paths (`forms/button`). The uuid identifies the
group to `Control.componentRefWithin`; write it once in source and never
change it.

    Playground.group { uuid = "3f6c1e2a-…", name = "Forms" } [ … ]

-}
group : { uuid : String, name : String } -> List (Playground e t) -> Playground e t
group meta children =
    Group meta children



-- ORDERING


{-| Sort every level of the tree, groups and pages alike, by name, ignoring
case. Names that compare equal keep their source order.

    Component.Application.element theme (Playground.sortByName playgrounds)

-}
sortByName : List (Playground e t) -> List (Playground e t)
sortByName playgrounds =
    playgrounds
        |> List.indexedMap
            (\index pg ->
                case pg of
                    Page meta _ ->
                        ( ( String.toLower meta.name, index ), pg )

                    Group meta children ->
                        ( ( String.toLower meta.name, index ), Group meta (sortByName children) )
            )
        |> List.sortBy Tuple.first
        |> List.map Tuple.second
