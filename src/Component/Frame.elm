module Component.Frame exposing
    ( Frame
    , Component_, Update
    , fromComponent, example, gallery, static
    , wrap
    )

{-| Frame constructors and combinators.

A frame describes how a component (or static content) appears on a playground
page. Frames are combined into pages via `Component.Playground.fromFrames`.


# Type

@docs Frame


# Re-exported type aliases

@docs Component_, Update


# Constructors

@docs fromComponent, example, gallery, static


# Modifiers

@docs wrap

-}

import Component.Internal as Internal
    exposing
        ( ComponentE
        , ComponentInstance(..)
        , ComponentRef(..)
        , Component_(..)
        , Control(..)
        , Frame(..)
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type exposing (Type)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State



-- TYPE RE-EXPORT


{-| A frame within a playground page. Produced by `fromComponent`, `example`,
`gallery`, or `static`, and optionally modified with `wrap`.
-}
type alias Frame e t =
    Internal.Frame e t


{-| Re-export of `Component.Component_`. A component with potentially distinct
storage and output types. Accepted by `fromComponent`, `example`, and
`gallery`.
-}
type alias Component_ e t i m msg =
    Internal.Component_ e t i m msg


{-| Re-export of `Component.Update`. The message type produced by interactive
frames — a combination of state changes and effects.
-}
type alias Update t e =
    Internal.Update t e



-- CONSTRUCTORS


{-| Turn a component into an interactive frame with a live controls panel.
Works with both plain (`Component`) and mapped (`Component_`) components.
-}
fromComponent : Component_ e t i m (Update t e) -> Frame e t
fromComponent (Component_ c) =
    InteractiveFrame { id = c.id, name = c.name }
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.take
                |> State.andThen
                    (\ref ->
                        let
                            instance =
                                ComponentInstance (ComponentRef c.id) ref
                        in
                        State.state (Ref.from ref (controlsF lib |> State.map (makeComponentE instance c)))
                    )
        )
        identity


{-| Like `fromComponent`, but with a pinned initial storage state and a
per-frame display name. The controls are still shown and the frame remains
fully interactive; `initial` replaces the controls' own default.

For a plain `Component e t m` (where `i == m`), `initial` is the model value.
For `Component_ e t i m`, `initial` is the storage-shape value.

-}
example : String -> i -> Component_ e t i m (Update t e) -> Frame e t
example name initial (Component_ c) =
    ExampleFrame { id = c.id, name = c.name }
        name
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.take
                |> State.andThen
                    (\ref ->
                        let
                            instance =
                                ComponentInstance (ComponentRef c.id) ref
                        in
                        State.state
                            (Ref.from ref
                                (controlsF lib
                                    |> State.map (\b -> makeComponentE instance c { b | default = initial })
                                )
                            )
                    )
        )
        identity


{-| A non-interactive gallery frame that renders multiple storage values using
a component's view function. Use this to enumerate variants or states side by
side without controls.

The third argument receives a `render` function — call it as many times as you
like and assemble the results into whatever layout you need:

    Frame.gallery "Button variants"
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
For `componentRef`-based mappings the referenced component renders at its
default state.

-}
gallery : String -> Component_ e t i m (Update t e) -> ((i -> Html (List e)) -> Html (List e)) -> Frame e t
gallery name (Component_ c) assemble =
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
                                |> Html.map (\effects -> Update [] effects)
                        )
                )
        )


{-| A static frame from HTML. Use for documentation, embedded Figma designs,
or any non-interactive content. The HTML can fire effects (`List e`) but
produces no state changes.
-}
static : Html (List e) -> Frame e t
static html =
    StaticFrame (Html.map (\effects -> Update [] effects) html)



-- MODIFIERS


{-| Wrap the rendered HTML of a frame. Use this to add chrome around a frame's
output — a fixed-height container, background colour, padding — without
changing the underlying component or content.

    Frame.fromComponent myComponent
        |> Frame.wrap
            (\inner ->
                Html.div
                    [ Html.Attributes.style "height" "300px"
                    , Html.Attributes.style "overflow" "hidden"
                    ]
                    [ inner ]
            )

Applies uniformly across all frame variants. Composes: the outer-most `wrap`
is the outer-most layer in the DOM.

For interactive frames (`fromComponent`, `example`), the wrapper is applied to
the component's rendered view only — not to the controls panel.

-}
wrap : (Html (Update t e) -> Html (Update t e)) -> Frame e t -> Frame e t
wrap f frame =
    case frame of
        InteractiveFrame meta build w ->
            InteractiveFrame meta build (f << w)

        ExampleFrame meta name build w ->
            ExampleFrame meta name build (f << w)

        StaticFrame html ->
            StaticFrame (f html)

        GalleryFrame name build ->
            GalleryFrame name (build >> State.map f)



-- INTERNAL HELPERS


makeComponentE :
    Internal.ComponentInstance
    -> { a | view : state -> value -> (state -> Update t e) -> Internal.View (Update t e) }
    -> Internal.ControlI_ e t state state value
    -> ComponentE e t
makeComponentE instance comp b =
    let
        render : Internal.Lookup t -> Internal.View (Update t e)
        render lookup =
            let
                currentState =
                    b.fromType b.default b.default lookup

                currentValue =
                    b.map lookup currentState

                setter : state -> Update t e
                setter newState =
                    let
                        ( finalState, effects ) =
                            b.update instance currentState newState
                    in
                    Update (b.toType finalState) effects
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
                |> List.map (wrapControl instance b)
                |> List.map
                    (\ctrl ->
                        ctrl lookup
                            |> Html.map (\( state, effects ) -> Update state effects)
                    )
    }


{-| Wrap a control to call the update function after state changes.
-}
wrapControl :
    Internal.ComponentInstance
    -> Internal.ControlI_ e t state state value
    -> (Internal.Lookup t -> Html (List ( Ref, Type t )))
    -> (Internal.Lookup t -> Html ( List ( Ref, Type t ), List e ))
wrapControl instance b ctrl lookup =
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
                        b.update instance oldI i

                    ownedChanges =
                        b.toType i2

                    ownedRefs =
                        List.map Tuple.first ownedChanges

                    foreignChanges =
                        List.filter (\( r, _ ) -> not (List.member r ownedRefs)) rawChanges
                in
                ( ownedChanges ++ foreignChanges, effects )
            )
