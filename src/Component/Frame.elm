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


{-| Re-export of `Component.Update`. State changes from an interactive
component, tagged with a ComponentInstance.
-}
type alias Update t =
    Internal.Update t



-- CONSTRUCTORS


{-| Turn a component into an interactive frame with a live controls panel.
Works with both plain (`Component`) and mapped (`Component_`) components.
-}
fromComponent : Component_ e t i m (Update t) -> Frame e t
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
example : String -> i -> Component_ e t i m (Update t) -> Frame e t
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

The rendered HTML can include event handlers, but they are dispatched
against a sentinel ComponentInstance that produces no state changes or
effects. For genuine interactivity, use `example` or `fromComponent`.

-}
gallery : String -> Component_ e t i m (Update t) -> ((i -> Html (Update t)) -> Html (Update t)) -> Frame e t
gallery name (Component_ c) assemble =
    GalleryFrame name
        (\lib ->
            let
                (Control controlsF) =
                    c.controls
            in
            Ref.take
                |> State.andThen
                    (\ref ->
                        let
                            sentinelInstance =
                                ComponentInstance (ComponentRef c.id) ref
                        in
                        State.state
                            (Ref.from ref
                                (controlsF lib
                                    |> State.map
                                        (\b ->
                                            let
                                                render : i -> Html (Update t)
                                                render i =
                                                    let
                                                        m =
                                                            b.map (always Nothing) i
                                                    in
                                                    c.view i m (\_ -> Update sentinelInstance [])
                                                        |> Tuple.first
                                            in
                                            assemble render
                                        )
                                )
                            )
                    )
        )


{-| A static frame from HTML. Use for documentation, embedded Figma designs,
or any non-interactive content. Must be truly static (`Html Never`) — use
native HTML elements like links and iframes for interactivity.
-}
static : Html Never -> Frame e t
static html =
    StaticFrame (Html.map never html)



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
wrap : (Html (Update t) -> Html (Update t)) -> Frame e t -> Frame e t
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
    -> { a | view : state -> value -> (state -> Update t) -> Internal.View (Update t) }
    -> Internal.ControlI_ e t state state value
    -> ComponentE e t
makeComponentE instance comp b =
    let
        render : Internal.Lookup t -> Internal.View (Update t)
        render lookup =
            let
                currentState =
                    b.fromType b.default b.default lookup

                currentValue =
                    b.map lookup currentState

                setter : state -> Update t
                setter newState =
                    Update instance (b.toType newState)
            in
            comp.view currentState currentValue setter

        update : Internal.Lookup t -> Internal.Lookup t -> ( List ( Ref, Type t ), List e )
        update oldLookup newLookup =
            let
                oldState =
                    b.fromType b.default b.default oldLookup

                newState =
                    b.fromType b.default b.default newLookup

                ( finalState, effects ) =
                    b.update instance oldState newState
            in
            ( b.toType finalState, effects )
    in
    { render = render
    , controls =
        \theme lookup ->
            let
                currentState =
                    b.fromType b.default b.default lookup
            in
            b.controls theme b.description currentState
                |> List.map
                    (\ctrl ->
                        ctrl lookup
                            |> Html.map (\changes -> Update instance changes)
                    )
    , update = update
    }
