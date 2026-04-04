module Component exposing
    ( Component, Controls, Frame, Playground
    , Update, View
    , explore, example, doco
    , playground, group
    , view
    , toRef
    , toComponentUpdate
    )

{-| Component Playground - an interactive component testing library for Elm.

Define self-contained components with controls and views, then assemble them
into a playground for interactive testing.


# Core Types

@docs Component, Controls, Frame, Playground


# Supporting Types

@docs Update, View


# Frame Constructors

@docs explore, example, doco


# Playground Constructors

@docs playground, group


# Component Helpers

@docs view


# References

@docs toRef


# Updates

@docs toComponentUpdate

-}

import Component.Internal as Internal
    exposing
        ( ComponentE
        , Controls(..)
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


{-| Alias for the controls type used in `Component` records. This is the same
type as `Controls.Controls` — re-exported here so users can annotate component
definitions without importing the `Controls` module.
-}
type alias Controls e t m =
    Internal.Controls e t m m


{-| A self-contained component definition. Compose this with `explore` or
`example` to create frames for a playground page.

  - `id` — stable identifier (used for URL routing and component references).
    **Must be unique** across all components in the playground.
  - `name` — display name shown in the playground UI.
  - `controls` — how the model is stored and rendered as interactive controls.
    Build with `Controls.builder`/`Controls.add`/`Controls.toControls` or use
    a primitive from the `Controls` module directly.
  - `view` — renders the component given the current model and a setter
    callback. Use `Component.view` to lift a plain `Html` view.

-}
type alias Component e t m msg =
    { id : String
    , name : String
    , controls : Controls e t m
    , view : m -> (m -> msg) -> View msg
    }


{-| A frame within a playground page. Create frames with `explore`, `example`,
or `doco`.
-}
type alias Frame e t msg =
    Internal.Frame e t msg


{-| A playground is a recursive tree of named pages and groups. Create with
`playground` and `group`.
-}
type alias Playground e t msg =
    Internal.Playground e t msg


{-| Update type for component state changes and effects.
-}
type alias Update t e =
    Internal.Update t e


{-| A view is the main HTML plus optional named portal slots.
-}
type alias View msg =
    Internal.View msg



-- FRAME CONSTRUCTORS


{-| Create an interactive explore frame from a component. The controls are
shown alongside the component view, driven by the component's `controls`.
-}
explore : Component e t m (Update t e) -> Frame e t (Update t e)
explore component =
    InteractiveFrame { id = component.id, name = component.name }
        (\lib ->
            let
                (Controls controlsF) =
                    component.controls
            in
            Ref.nested (controlsF lib |> State.map (makeComponentE component))
        )


{-| Create an interactive example frame with a pinned initial model value. The
controls are still shown and the frame is fully interactive; `initialModel` is
used as the starting state instead of the controls' own default.
-}
example : String -> m -> Component e t m (Update t e) -> Frame e t (Update t e)
example name initialModel component =
    ExampleFrame { id = component.id, name = component.name }
        name
        (\lib ->
            let
                (Controls controlsF) =
                    component.controls
            in
            Ref.nested
                (controlsF lib
                    |> State.map (\b -> makeComponentE component { b | default = initialModel })
                )
        )


{-| Create a documentation frame from static HTML.
-}
doco : Html msg -> Frame e t msg
doco html =
    DocoFrame html



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



-- COMPONENT HELPERS


{-| Lift a plain `Html msg` view (no portals) into the `View msg` type
expected by `Component`. Use this when your component doesn't need portal
slots.

    view =
        Component.view
            (\model setter ->
                Html.button [ Html.Events.onClick (setter { model | count = model.count + 1 }) ]
                    [ Html.text (String.fromInt model.count) ]
            )

-}
view : (m -> (m -> msg) -> Html msg) -> (m -> (m -> msg) -> View msg)
view f m setter =
    ( f m setter, Dict.empty )



-- REFERENCES


{-| Extract a component's id as a string reference. Use this to provide
default values for `Controls.componentRef` controls.

    Controls.componentRef
        |> Controls.withDefault (Component.toRef myComponent)

-}
toRef : Component e t m msg -> String
toRef component =
    component.id



-- UPDATES


{-| Wrap an effect as an `Update`. Use this when a component produces an
effect that should be handled by the host application.
-}
toComponentUpdate : e -> Update t e
toComponentUpdate effect =
    Update [] [ effect ]



-- INTERNAL HELPERS


makeComponentE :
    { a | name : String, view : m -> (m -> Update t e) -> View (Update t e) }
    -> Internal.ControlsI_ e t m m m
    -> ComponentE e t
makeComponentE component b =
    { render =
        \lookup ->
            let
                m =
                    b.fromType b.default b.default lookup

                setter newM =
                    Update (b.toType newM) []
            in
            component.view m setter
    , controls =
        \lookup ->
            b.controls b.description b.default
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
    Internal.ControlsI_ e t i i a
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
