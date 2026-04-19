module Component.Frame exposing
    ( Frame
    , Component_, Preset, Update
    , fromComponent, gallery, static, subheading, presets, presetGallery
    , wrap
    )

{-| Frame constructors and combinators.

A frame describes how a component (or static content) appears on a playground
page. Frames are combined into pages via `Component.Playground.fromFrames`.


# Type

@docs Frame


# Re-exported type aliases

@docs Component_, Preset, Update


# Constructors

@docs fromComponent, gallery, static, subheading, presets, presetGallery


# Modifiers

@docs wrap

-}

import Component.Application.Theme exposing (Theme)
import Component.Internal as Internal
    exposing
        ( ComponentE
        , ComponentInstance(..)
        , ComponentRef(..)
        , Component_(..)
        , Control(..)
        , Frame(..)
        , PresetsInfo
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type as Type exposing (Type)
import Component.Ui as Ui
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import State exposing (State)



-- TYPE RE-EXPORT


{-| A frame within a playground page. Produced by `fromComponent`, `presets`,
`presetGallery`, `gallery`, `static`, or `subheading`, and optionally
modified with `wrap`.
-}
type alias Frame e t =
    Internal.Frame e t


{-| Re-export of `Component.Component_`. A component with potentially distinct
storage and output types. Accepted by `fromComponent`, `presets`, `gallery`,
and `presetGallery`.
-}
type alias Component_ e t i m msg =
    Internal.Component_ e t i m msg


{-| Re-export of `Component.Preset`. A named configuration used by
`Component.withPresets`, `Frame.presets`, and `Frame.presetGallery`.
-}
type alias Preset t i =
    Internal.Preset t i


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
    InteractiveFrame { id = c.id, name = c.name } (makeFactory c) identity


{-| Turn a component into an interactive frame with a preset tab bar across
the top. Each named preset is a tab; clicking a tab replaces the component's
state with the preset's value. The controls panel stays editable — opening
it while a named tab is active shows exactly what that preset sets.

The component must have been extended with `Component.withPresets`;
otherwise the tab bar has no content.

-}
presets : Component_ e t i m (Update t) -> Frame e t
presets (Component_ c) =
    PresetsFrame { id = c.id, name = c.name } (makeFactory c) identity


{-| A non-interactive gallery frame that renders multiple storage values using
a component's view function. Use this to enumerate variants or states side by
side without controls.

The second argument receives a `render` function — call it as many times as
you like and assemble the results into whatever layout you need:

    Frame.gallery Components.button
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
effects. For genuine interactivity, use `fromComponent` or `presets`.

-}
gallery : Component_ e t i m (Update t) -> ((i -> Html (Update t)) -> Html (Update t)) -> Frame e t
gallery (Component_ c) assemble =
    GalleryFrame
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


{-| A gallery showing every preset of the component, side-by-side. Each
preset is rendered with its `wrap` function applied, and labelled with its
name. The component must have been extended with `Component.withPresets`.

Implemented on top of `Frame.gallery` — drop down to `Frame.gallery`
directly if you need a different layout or ordering.

-}
presetGallery : Component_ e t i m (Update t) -> Frame e t
presetGallery ((Component_ c) as component) =
    gallery component
        (\render ->
            Html.div
                [ Ui.style "display" "flex"
                , Ui.style "flex-wrap" "wrap"
                , Ui.style "gap" "24px"
                ]
                (List.map
                    (\p ->
                        Html.div
                            [ Ui.style "display" "flex"
                            , Ui.style "flex-direction" "column"
                            , Ui.style "gap" "8px"
                            ]
                            [ Html.div
                                [ Ui.style "font-weight" "500" ]
                                [ Html.text p.name ]
                            , p.wrap (render p.value)
                            ]
                    )
                    c.presets
                )
        )


{-| A static frame from HTML. Use for documentation, embedded Figma designs,
or any non-interactive content. Must be truly static (`Html Never`) — use
native HTML elements like links and iframes for interactivity.
-}
static : Html Never -> Frame e t
static html =
    StaticFrame (Html.map never html)


{-| A frame that renders as a subheading between other frames. Useful for
grouping interactive, gallery, and static frames under labelled sections on
a page.
-}
subheading : String -> Frame e t
subheading label =
    SubheadingFrame label



-- MODIFIERS


{-| Wrap the rendered HTML of a frame. Use this to add chrome around a frame's
output — a fixed-height container, background colour, padding — without
changing the underlying component or content.

Applies uniformly across all frame variants. Composes: the outer-most `wrap`
is the outer-most layer in the DOM.

For interactive frames (`fromComponent`, `presets`), the wrapper is applied
to the component's rendered view only — not to the controls panel.

-}
wrap : (Html (Update t) -> Html (Update t)) -> Frame e t -> Frame e t
wrap f frame =
    case frame of
        InteractiveFrame meta build w ->
            InteractiveFrame meta build (f << w)

        PresetsFrame meta build w ->
            PresetsFrame meta build (f << w)

        StaticFrame html ->
            StaticFrame (f html)

        GalleryFrame build ->
            GalleryFrame (build >> State.map f)

        SubheadingFrame label ->
            SubheadingFrame label



-- INTERNAL HELPERS


makeFactory :
    { a
        | id : String
        , controls : Control e t state value
        , view : state -> value -> (state -> Update t) -> Internal.View (Update t)
        , presets : List (Internal.Preset t state)
    }
    -> Internal.Library e t
    -> State Ref (ComponentE e t)
makeFactory c lib =
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
                State.state (Ref.from ref (buildComponentE instance c controlsF lib))
            )


buildComponentE :
    ComponentInstance
    -> { a | view : state -> value -> (state -> Update t) -> Internal.View (Update t), presets : List (Internal.Preset t state) }
    -> (Internal.Library e t -> State Ref (Internal.ControlI_ e t state state value))
    -> Internal.Library e t
    -> State Ref (ComponentE e t)
buildComponentE instance c controlsF lib =
    case c.presets of
        [] ->
            controlsF lib
                |> State.map (\b -> makeComponentE instance c.view [] Nothing b)

        _ ->
            controlsF lib
                |> State.andThen
                    (\b ->
                        Ref.take
                            |> State.map
                                (\presetRef ->
                                    makeComponentE instance c.view c.presets (Just presetRef) b
                                )
                    )


makeComponentE :
    Internal.ComponentInstance
    -> (state -> value -> (state -> Update t) -> Internal.View (Update t))
    -> List (Internal.Preset t state)
    -> Maybe Ref
    -> Internal.ControlI_ e t state state value
    -> ComponentE e t
makeComponentE instance componentView presetList maybePresetRef rawB =
    let
        b =
            case presetList of
                [] ->
                    rawB

                first :: _ ->
                    { rawB | default = first.value }

        currentState lookup =
            b.fromType b.default b.default lookup

        updateSetter : state -> Update t
        updateSetter newState =
            Update instance (b.toType newState)

        render : Internal.Lookup t -> Internal.View (Update t)
        render lookup =
            componentView (currentState lookup) (b.map lookup (currentState lookup)) updateSetter

        update : Internal.Lookup t -> Internal.Lookup t -> ( List ( Ref, Type t ), List e )
        update oldLookup newLookup =
            let
                ( finalState, effects ) =
                    b.update instance updateSetter (currentState oldLookup) (currentState newLookup)
            in
            ( b.toType finalState, effects )

        innerControls theme lookup =
            b.controls theme b.description (currentState lookup)
                |> List.map
                    (\ctrl ->
                        ctrl lookup
                            |> Html.map (\changes -> Update instance changes)
                    )

        maybeInfo : Maybe (PresetsInfo t)
        maybeInfo =
            Maybe.map (buildPresetsInfo instance componentView b presetList) maybePresetRef

        picker : Maybe (Theme -> Internal.Lookup t -> Html (Update t))
        picker =
            Maybe.map2 (makePicker instance) maybePresetRef maybeInfo

        controls theme lookup =
            case picker of
                Just p ->
                    p theme lookup :: innerControls theme lookup

                Nothing ->
                    innerControls theme lookup
    in
    { render = render
    , controls = controls
    , innerControls = innerControls
    , update = update
    , presets = maybeInfo
    }


buildPresetsInfo :
    Internal.ComponentInstance
    -> (state -> value -> (state -> Update t) -> Internal.View (Update t))
    -> Internal.ControlI_ e t state state value
    -> List (Internal.Preset t state)
    -> Ref
    -> PresetsInfo t
buildPresetsInfo instance componentView b presetList presetRef =
    let
        names =
            List.map .name presetList

        findPreset name =
            List.find (\p -> p.name == name) presetList

        current lookup =
            case lookup presetRef |> Maybe.andThen Type.stringValue of
                Nothing ->
                    List.head names

                Just name ->
                    if List.member name names then
                        Just name

                    else
                        Nothing

        pick name =
            Update instance <|
                case findPreset name of
                    Just p ->
                        ( presetRef, Type.StringValue name ) :: b.toType p.value

                    Nothing ->
                        []

        updateSetter newState =
            Update instance (b.toType newState)

        renderAt name lookup =
            Maybe.map
                (\p ->
                    let
                        overlayUpdates =
                            b.toType p.value

                        overlayLookup ref =
                            List.find (\( r, _ ) -> r == ref) overlayUpdates
                                |> Maybe.map Tuple.second
                                |> Maybe.orElseLazy (\() -> lookup ref)
                    in
                    componentView p.value (b.map overlayLookup p.value) updateSetter
                )
                (findPreset name)

        wrapAt name =
            findPreset name
                |> Maybe.map .wrap
                |> Maybe.withDefault identity
    in
    { names = names
    , current = current
    , pick = pick
    , renderAt = renderAt
    , wrapAt = wrapAt
    }


makePicker :
    Internal.ComponentInstance
    -> Ref
    -> PresetsInfo t
    -> Theme
    -> Internal.Lookup t
    -> Html (Update t)
makePicker _ presetRef info theme lookup =
    let
        currentValue =
            info.current lookup
                |> Maybe.withDefault (List.head info.names |> Maybe.withDefault "")

        options =
            List.map (\name -> { label = name, value = name }) info.names
    in
    Ui.select theme
        { msg = info.pick
        , id = Ref.toString presetRef
        , label = "Preset"
        , value = currentValue
        , options = options
        }
