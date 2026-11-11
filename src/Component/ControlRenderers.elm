module Component.ControlRenderers exposing (ControlRenderers, SelectConfig, default)

{-| Host-injectable renderers for the Inspector's controls.

The playground is "just another consumer" of a design system: the controls that
configure a component should be able to be the host's own production controls.
This module defines the generic interface for that injection — the library
provides a fallback built from its own primitives, and a consuming application
(e.g. Planwisely) may supply its own renderers that adapt the control data into
its real components, **without the library depending on the host**.

This is a deliberately narrow proof: only the `select` control is injectable so
far. The data each renderer receives is exactly what the library already has at
the control site, so a host renderer is a pure adapter.

@docs ControlRenderers, SelectConfig, default

-}

import Component.Application.Theme exposing (Theme)
import Component.Ui as Ui
import Html exposing (Html)


{-| The configuration the library hands a `select` renderer: the field id and
label, the current value, the available options, and the change handler. `msg`
is the control-update message — the renderer is fully polymorphic in it, so a
host renderer simply threads `onChange` into its own component.
-}
type alias SelectConfig msg =
    { id : String
    , label : String
    , value : String
    , options : List { label : String, value : String }
    , onChange : String -> msg
    }


{-| The set of host-injectable control renderers. Narrow by design — extend with
`textField`, `toggle`, … as the proof graduates to a migration.
-}
type alias ControlRenderers msg =
    { select : SelectConfig msg -> Html msg
    }


{-| The library's fallback renderers, built from its own `Component.Ui`
primitives. Used when a host supplies none, so the library still works
standalone.
-}
default : Theme -> ControlRenderers msg
default theme =
    { select =
        \c ->
            Ui.select theme
                { id = c.id
                , options = c.options
                , label = c.label
                , value = c.value
                , msg = c.onChange
                }
    }
