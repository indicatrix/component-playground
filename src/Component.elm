module Component exposing
    ( Component, Component_, ComponentRef, Control, Control_, Frame, Playground
    , Update, View
    , component, component_, componentWithPortals, componentWithPortals_
    , explore, example, static
    , playground, group
    , toRef
    )

{-| Component Playground — an interactive component testing library for Elm.

Build interactive playgrounds for your UI components in three steps:

1.  **Components** define _what_ to render: a set of controls and a view
    function. Controls describe how to store, edit, and display each parameter
    your component accepts. The view receives the current parameter values and
    renders the component.

2.  **Frames** define _how_ to present a component on a page. `explore` gives
    an interactive frame with a live controls panel. `example` pins a specific
    starting state. `static` inserts static HTML for documentation.

3.  **Playgrounds** organise frames into named pages and groups, producing a
    navigable sidebar. Pass the playground tree to
    `Component.Application.element` to run it.


# Core Types

@docs Component, Component_, ComponentRef, Control, Control_, Frame, Playground


# Supporting Types

@docs Update, View


# Component Constructors

@docs component, component_, componentWithPortals, componentWithPortals_


# Frame Constructors

@docs explore, example, static


# Playground Constructors

@docs playground, group


# References

@docs toRef

-}

import Component.Internal as Internal
    exposing
        ( ComponentE
        , ComponentRef(..)
        , Control(..)
        , Frame(..)
        , Playground(..)
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type exposing (Type)
import Dict
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State



-- TYPE RE-EXPORTS


{-| Alias for the control type used in `Component` records. This is the same
type as `Control.Control` — re-exported here so users can annotate component
definitions without importing the `Component.Control` module.
-}
type alias Control e t m =
    Internal.Control e t m m


{-| General control type where storage type `i` may differ from output `m`.
-}
type alias Control_ e t i m =
    Internal.Control e t i m


{-| A component where storage and output types are the same.
Create with `component` or `componentWithPortals`.
-}
type alias Component e t m msg =
    Component_ e t m m msg


{-| A component where storage type `i` may differ from output type `m`.
Create with `component_` or `componentWithPortals_`.
-}
type Component_ e t i m msg
    = Component_
        { id : String
        , name : String
        , controls : Control_ e t i m
        , view : i -> m -> (i -> msg) -> View msg
        }


{-| A frame within a playground page. Create frames with `explore`, `example`,
or `static`.
-}
type alias Frame e t msg =
    Internal.Frame e t msg


{-| A playground is a recursive tree of named pages and groups. Create with
`playground` and `group`.
-}
type alias Playground e t msg =
    Internal.Playground e t msg


{-| Opaque reference to a component. Use `toRef` to create and pass to
`Control.componentRef` defaults.
-}
type alias ComponentRef =
    Internal.ComponentRef


{-| Update type for component state changes and effects.
-}
type alias Update t e =
    Internal.Update t e


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    Internal.View msg



-- COMPONENT CONSTRUCTORS


{-| Create a component from a plain `Html` view (no portals). This is the
common case — use `componentWithPortals` if you need named portal slots.

    myButton =
        Component.component
            { id = "button"
            , name = "Button"
            , controls =
                Control.builder ButtonModel
                    |> Control.add "Label" .label Control.string
                    |> Control.toControl
            , view =
                \model setter ->
                    Html.button [ Html.Events.onClick (setter { model | clicked = True }) ]
                        [ Html.text model.label ]
            }

-}
component :
    { id : String
    , name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> Html msg
    }
    -> Component e t m msg
component c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \_ m setter -> ( c.view m setter, Dict.empty )
        }


{-| Create a component whose view returns named portal slots alongside the
main HTML. Use `component` instead if you don't need portals.
-}
componentWithPortals :
    { id : String
    , name : String
    , controls : Control e t m
    , view : m -> (m -> msg) -> View msg
    }
    -> Component e t m msg
componentWithPortals c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \_ m setter -> c.view m setter
        }


{-| Create a component where storage type `i` differs from output type `m`.
The view receives both the storage record and the mapped output.
-}
component_ :
    { id : String
    , name : String
    , controls : Control_ e t i m
    , view : i -> m -> (i -> msg) -> Html msg
    }
    -> Component_ e t i m msg
component_ c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = \i m setter -> ( c.view i m setter, Dict.empty )
        }


{-| Like `component_`, but the view returns named portal slots.
-}
componentWithPortals_ :
    { id : String
    , name : String
    , controls : Control_ e t i m
    , view : i -> m -> (i -> msg) -> View msg
    }
    -> Component_ e t i m msg
componentWithPortals_ c =
    Component_
        { id = c.id
        , name = c.name
        , controls = c.controls
        , view = c.view
        }



-- FRAME CONSTRUCTORS


{-| Create an interactive explore frame from a component. Works with both
simple (`Component`) and mapped (`Component_`) components.
-}
explore : Component_ e t i m (Update t e) -> Frame e t (Update t e)
explore (Component_ c) =
    InteractiveFrame { id = c.id, name = c.name }
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.nested (controlsF lib |> State.map (makeComponentE c))
        )


{-| Create an interactive example frame with a pinned initial model value. The
controls are still shown and the frame is fully interactive; `initialModel` is
used as the starting state instead of the controls' own default.
-}
example : String -> m -> Component e t m (Update t e) -> Frame e t (Update t e)
example name initialModel (Component_ c) =
    ExampleFrame { id = c.id, name = c.name }
        name
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.nested
                (controlsF lib
                    |> State.map (\b -> makeComponentE c { b | default = initialModel })
                )
        )


{-| Create a static frame from HTML. Use for documentation, embedded Figma
designs, or any non-interactive content.
-}
static : Html msg -> Frame e t msg
static html =
    StaticFrame html



-- PLAYGROUND CONSTRUCTORS


{-| Create a named playground page containing a list of frames.
-}
playground : { id : String, name : String } -> List (Frame e t msg) -> Playground e t msg
playground meta frames =
    Page meta frames


{-| Create a named group of playground pages or sub-groups.
-}
group : { id : String, name : String } -> List (Playground e t msg) -> Playground e t msg
group meta children =
    Group meta children



-- REFERENCES


{-| Extract an opaque component reference. Use this to provide default
values for `Control.componentRef` controls.

    Control.componentRef
        |> Control.withDefault (Component.toRef myComponent)

-}
toRef : Component_ e t i m msg -> ComponentRef
toRef (Component_ c) =
    ComponentRef c.id



-- INTERNAL HELPERS


makeComponentE :
    { a | name : String, view : i -> m -> (i -> Update t e) -> View (Update t e) }
    -> Internal.ControlI_ e t i i m
    -> ComponentE e t
makeComponentE comp b =
    { render =
        \lookup ->
            let
                i =
                    b.fromType b.default b.default lookup

                m =
                    b.map lookup i

                setter newI =
                    Update (b.toType newI) []
            in
            comp.view i m setter
    , controls =
        \lookup ->
            let
                currentState =
                    b.fromType b.default b.default lookup
            in
            b.controls b.description currentState
                |> List.map (wrapControl b)
                |> List.map
                    (\ctrl ->
                        ctrl lookup
                            |> Html.map (\( state, effects ) -> Update state effects)
                    )
    }


{-| Wrap a control to call the update function after state changes.
-}
wrapControl :
    Internal.ControlI_ e t i i a
    -> (Internal.Lookup t -> Html (List ( Ref, Type t )))
    -> (Internal.Lookup t -> Html ( List ( Ref, Type t ), List e ))
wrapControl b ctrl lookup =
    ctrl lookup
        |> Html.map
            (\rawChanges ->
                let
                    patchedLookup ref =
                        List.find (\( r, _ ) -> r == ref) rawChanges
                            |> Maybe.map Tuple.second
                            |> Maybe.orElseLazy (\() -> lookup ref)

                    oldI =
                        b.fromType b.default b.default lookup

                    i =
                        b.fromType b.default b.default patchedLookup

                    ( i2, effects ) =
                        b.update oldI i

                    ownedChanges =
                        b.toType i2

                    ownedRefs =
                        List.map Tuple.first ownedChanges

                    foreignChanges =
                        List.filter (\( r, _ ) -> not (List.member r ownedRefs)) rawChanges
                in
                ( ownedChanges ++ foreignChanges, effects )
            )
