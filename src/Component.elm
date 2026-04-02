module Component exposing
    ( Component, Frame, Playground
    , Update, View
    , explore, example, doco
    , playground, group
    , view
    , toComponentUpdate
    , withDefault
    )

{-| Component Playground - an interactive component testing library for Elm.

Define self-contained components with controls and views, then assemble them
into a playground for interactive testing.


# Core Types

@docs Component, Frame, Playground


# Supporting Types

@docs Update, View


# Frame Constructors

@docs explore, example, doco


# Playground Constructors

@docs playground, group


# Component Helpers

@docs view


# Updates

@docs toComponentUpdate


# Advanced

@docs withDefault

-}

import Component.Internal as Internal
    exposing
        ( Controls(..)
        , Frame(..)
        , FrameInternals
        , Library(..)
        , Playground(..)
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type exposing (Type)
import Component.UI as UI
import Dict
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)



-- TYPE RE-EXPORTS


{-| A self-contained component definition. Compose this with `explore` or
`example` to create frames for a playground page.

  - `id` — stable identifier (used for URL routing).
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
    , controls : Internal.Controls e t m m
    , view : m -> (m -> msg) -> View msg
    }


{-| A frame within a playground page. Create frames with `explore`, `example`,
or `doco`.
-}
type alias Frame e t =
    Internal.Frame e t


{-| A playground is a recursive tree of named pages and groups. Create with
`playground` and `group`.
-}
type alias Playground e t =
    Internal.Playground e t


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
explore : Component e t m (Update t e) -> Frame e t
explore component =
    let
        dummyLib : Internal.Library e t
        dummyLib =
            Library "" { index = [], groups = [] }

        (Block blockF) =
            component.controls
    in
    InteractiveFrame
        (Ref.nested
            (blockF dummyLib
                |> State.map (makeFrameInternals component.name component.view)
            )
        )


{-| Create an interactive example frame with a pinned initial model value. The
controls are still shown and the frame is fully interactive; `initialModel` is
used as the starting state instead of the controls' own default.
-}
example : String -> m -> Component e t m (Update t e) -> Frame e t
example name initialModel component =
    let
        controls =
            withDefault initialModel component.controls

        dummyLib : Internal.Library e t
        dummyLib =
            Library "" { index = [], groups = [] }

        (Block blockF) =
            controls
    in
    ExampleFrame name
        (Ref.nested
            (blockF dummyLib
                |> State.map (makeFrameInternals component.name component.view)
            )
        )


{-| Create a documentation frame from static HTML. The HTML type is
`Html (Update t e)` to align with other frame constructors; in practice use
`Html.map Component.toComponentUpdate` if you have no updates.
-}
doco : Html (Update t e) -> Frame e t
doco html =
    DocoFrame html



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



-- UPDATES


{-| Wrap an effect as an `Update`. Use this when a component produces an
effect that should be handled by the host application.
-}
toComponentUpdate : e -> Update t e
toComponentUpdate effect =
    WithEffect [] [ effect ]



-- ADVANCED


{-| Override the default value for controls. Use this to set a stable initial
state for an `example` frame.

    Component.example "Disabled"
        (Component.withDefault { label = "Submit", disabled = True } button.controls)
        button

-}
withDefault : m -> Internal.Controls e t m m -> Internal.Controls e t m m
withDefault m (Block f) =
    Block <| \lib -> State.map (\b -> { b | default = m }) (f lib)



-- INTERNAL HELPERS


makeFrameInternals :
    String
    -> (m -> (m -> Update t e) -> View (Update t e))
    -> Internal.ControlsI_ e t m m m
    -> FrameInternals e t
makeFrameInternals label viewFn b =
    { render =
        \lookup ->
            let
                m =
                    b.fromType b.default b.default lookup

                setter newM =
                    Update (b.toType newM) []
            in
            viewFn m setter
    , controls =
        \lookup ->
            b.controls label b.default
                |> List.map (wrapControl b)
                |> List.map
                    (\ctrl ->
                        ctrl lookup
                            |> Html.map (\( state, effects ) -> Update state effects)
                    )
    }


{-| Wrap a block control to call the update function after state changes.
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
