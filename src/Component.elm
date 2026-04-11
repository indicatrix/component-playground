module Component exposing
    ( Component, Component_, ComponentRef, Control, Control_, Frame, Playground
    , Update, View
    , component, component_, componentWithPortals, componentWithPortals_
    , explore, exploreFrame, example, static, galleryFrame, galleryFrame_
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

@docs explore, exploreFrame, example, static, galleryFrame, galleryFrame_


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
type alias Control e t state =
    Internal.Control e t state state


{-| General control type where storage type `state` may differ from output
`value`.
-}
type alias Control_ e t state value =
    Internal.Control e t state value


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
type alias Frame e t =
    Internal.Frame e t


{-| A playground is a recursive tree of named pages and groups. Create with
`playground` and `group`.
-}
type alias Playground e t =
    Internal.Playground e t


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
explore : Component_ e t i m (Update t e) -> Frame e t
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
example : String -> m -> Component e t m (Update t e) -> Frame e t
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


{-| Like `explore`, but wraps the rendered component HTML before display.
Use this to add chrome around the component — for example a fixed-height
container, background colour, or padding — without changing the component
itself.

    Component.exploreFrame
        (\inner ->
            Html.div
                [ Html.Attributes.style "height" "300px"
                , Html.Attributes.style "overflow" "hidden"
                ]
                [ inner ]
        )
        myComponent

-}
exploreFrame : (Html (Update t e) -> Html (Update t e)) -> Component_ e t i m (Update t e) -> Frame e t
exploreFrame wrapper (Component_ c) =
    InteractiveFrame { id = c.id, name = c.name }
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.nested
                (controlsF lib
                    |> State.map
                        (\b ->
                            let
                                base =
                                    makeComponentE c b
                            in
                            { base
                                | render =
                                    \lookup ->
                                        let
                                            ( html, portals ) =
                                                base.render lookup
                                        in
                                        ( wrapper html, portals )
                            }
                        )
                )
        )


{-| Create a static frame from HTML. Use for documentation, embedded Figma
designs, or any non-interactive content.
-}
static : Html (List e) -> Frame e t
static html =
    StaticFrame html


{-| Create a non-interactive gallery frame that renders multiple model values
using a component's view function. Use this to enumerate variants or states
side by side without controls.

The third argument is a callback that receives a `render` function — call it
with any number of model values to produce individual `Html` nodes, then
assemble them into whatever layout you need:

    Component.galleryFrame "Button variants"
        Components.button
        (\render ->
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "gap" "16px"
                ]
                [ render { label = "Primary", variant = Primary }
                , render { label = "Secondary", variant = Secondary }
                , render { label = "Danger", variant = Danger }
                ]
        )

The rendered HTML can fire effects (`List e`) but produces no state changes.

-}
galleryFrame : String -> Component e t m (Update t e) -> ((m -> Html (List e)) -> Html (List e)) -> Frame e t
galleryFrame name (Component_ c) assemble =
    let
        render : m -> Html (List e)
        render m =
            c.view m m (\_ -> Update [] [])
                |> Tuple.first
                |> Html.map (\(Update _ effects) -> effects)
    in
    GalleryFrame name (\_ -> State.state (assemble render))


{-| Like `galleryFrame`, but works with mapped components (`Component_`) where
the storage type `i` differs from the output type `m`. The render callback
receives `i` (storage) values; the frame derives `m` from the controls'
mapping function internally.

    Component.galleryFrame_ "Content block variants"
        Components.contentBlock
        (\render ->
            Html.div [ Html.Attributes.style "display" "flex", Html.Attributes.style "gap" "16px" ]
                [ render { kind = "text",   text = "Hello", number = 0, toggle = False }
                , render { kind = "number", text = "",      number = 42, toggle = False }
                , render { kind = "toggle", text = "",      number = 0,  toggle = True }
                ]
        )

Note: if the component uses `componentRef` controls in its mapping, those
referenced components will render as empty in the gallery (no playground state
is available to resolve them).

-}
galleryFrame_ : String -> Component_ e t i m (Update t e) -> ((i -> Html (List e)) -> Html (List e)) -> Frame e t
galleryFrame_ name (Component_ c) assemble =
    GalleryFrame name
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.nested
                (controlsF lib
                    |> State.map
                        (\b ->
                            let
                                render : i -> Html (List e)
                                render i =
                                    let
                                        m =
                                            b.map (always Nothing) i
                                    in
                                    c.view i m (\_ -> Update [] [])
                                        |> Tuple.first
                                        |> Html.map (\(Update _ effects) -> effects)
                            in
                            assemble render
                        )
                )
        )



-- PLAYGROUND CONSTRUCTORS


{-| Create a named playground page containing a list of frames.
-}
playground : { id : String, name : String } -> List (Frame e t) -> Playground e t
playground meta frames =
    Page meta frames


{-| Create a named group of playground pages or sub-groups.
-}
group : { id : String, name : String } -> List (Playground e t) -> Playground e t
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
    { a | view : state -> value -> (state -> Update t e) -> View (Update t e) }
    -> Internal.ControlI_ e t state state value
    -> ComponentE e t
makeComponentE comp b =
    let
        render : Internal.Lookup t -> View (Update t e)
        render lookup =
            let
                currentState =
                    b.fromType b.default b.default lookup

                currentValue =
                    b.map lookup currentState

                setter : state -> Update t e
                setter newState =
                    Update (b.toType newState) []
            in
            comp.view currentState currentValue setter
    in
    { render = render
    , controls =
        \theme lookup ->
            let
                currentState =
                    b.fromType b.default b.default lookup
            in
            b.controls theme b.description currentState
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
    Internal.ControlI_ e t state state value
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
