module Component.Application exposing
    ( Msg, Model, ProcessedFrame, ComponentPlayground
    , ComponentInstance, ComponentUpdate, Index, Library_, Playground, Ref, Type
    , element, init, update, view, toUrl
    , fromUpdate, renderPortal
    )

{-| Application runner for the Component Playground.


# Types

@docs Msg, Model, ProcessedFrame, ComponentPlayground


# Re-exported Aliases

@docs ComponentInstance, ComponentUpdate, Index, Library_, Playground, Ref, Type


# Running the Playground

The playground can be run as a standalone `element`, or wired into a larger
application using `init`, `update`, and `view`.

@docs element, init, update, view, toUrl


# Portals and Effect Dispatch

@docs fromUpdate, renderPortal

-}

import Browser
import Component.Application.Theme exposing (Theme)
import Component.Internal as Internal
    exposing
        ( ComponentE
        , ComponentInstance(..)
        , Frame(..)
        , Index(..)
        , Library(..)
        , Library_
        , Playground(..)
        , Update
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type
import Component.Ui as Ui
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Set exposing (Set)
import State exposing (State)
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query



-- PROCESSED TYPES
-- These are internal to Application; users interact with Frame/Playground.


type ProcessedFrame e t
    = ProcessedInteractive { id : String, name : String } (Html (Update t) -> Html (Update t)) (ComponentE e t)
    | ProcessedPresets { id : String, name : String } (Html (Update t) -> Html (Update t)) (ComponentE e t)
    | ProcessedStatic (Html (Update t))
    | ProcessedGallery (Html (Update t))
    | ProcessedSubheading String



-- MSG AND MODEL


type Msg t e
    = NoOp
    | ComponentUpdate (Internal.Update t)
    | ViewPage String
    | UpdateSearch String
    | ToggleInspector
    | SelectInspector String
    | ToggleGroup String
    | ToggleTokenGroup String


type alias Model t e =
    { state : Dict String (Type t)
    , pages : Dict String (List (ProcessedFrame e t))
    , library : Library_ e t
    , index : List Index
    , currentPage : String
    , search : String
    , inspectorOpen : Bool
    , activeInspector : Maybe String
    , collapsedGroups : Set String
    , expandedTokens : Set String
    , theme : Theme
    }


type alias ComponentPlayground t e =
    Program () (Model t e) (Msg t e)



-- RE-EXPORTED ALIASES


type alias ComponentInstance =
    Internal.ComponentInstance


type alias ComponentUpdate t =
    Internal.Update t


{-| Sidebar index tree. Re-exported from `Component.Internal`.
-}
type alias Index =
    Internal.Index


{-| Library navigation metadata. Re-exported from `Component.Internal`.
Used in the `library` field of `Model`.
-}
type alias Library_ e t =
    Internal.Library_ e t


{-| A playground is a recursive tree of named pages and groups. Re-exported
from `Component.Internal`.
-}
type alias Playground e t =
    Internal.Playground e t


type alias Ref =
    Ref.Ref


type alias Type t =
    Component.Type.Type t



-- PROCESSING


extractLibrary : List (Playground e t) -> Internal.Library_ e t
extractLibrary playgrounds =
    let
        defs =
            extractDefs playgrounds

        defDict =
            Dict.fromList (List.map (\d -> ( d.id, d.def )) defs)
    in
    { index = List.map (\d -> { id = d.id, name = d.name }) defs
    , groups = List.filterMap extractGroup playgrounds
    , lookupDef = \id -> Dict.get id defDict
    }


{-| Walk the Playground tree and collect all InteractiveFrame/PresetsFrame
definitions, keyed by component id. Component ids must be unique across all
components in the playground.
-}
extractDefs :
    List (Playground e t)
    ->
        List
            { id : String
            , name : String
            , def : Library e t -> State Ref (ComponentE e t)
            }
extractDefs playgrounds =
    List.concatMap
        (\pg ->
            case pg of
                Page _ frames ->
                    List.filterMap
                        (\frame ->
                            case frame of
                                InteractiveFrame meta f _ ->
                                    Just { id = meta.id, name = meta.name, def = f }

                                PresetsFrame meta f _ ->
                                    Just { id = meta.id, name = meta.name, def = f }

                                StaticFrame _ ->
                                    Nothing

                                GalleryFrame _ ->
                                    Nothing

                                SubheadingFrame _ ->
                                    Nothing
                        )
                        frames

                Group _ children ->
                    extractDefs children
        )
        playgrounds


toIndex : Maybe String -> List (Playground e t) -> List Index
toIndex prefix =
    List.map
        (\pg ->
            case pg of
                Page meta _ ->
                    Index { id = concatPrefix prefix meta.id, name = meta.name, children = [] }

                Group meta children ->
                    let
                        prefix_ =
                            concatPrefix prefix meta.id
                    in
                    Index { id = prefix_, name = meta.name, children = toIndex (Just prefix_) children }
        )


flattenIndex : List Index -> List { id : String, name : String }
flattenIndex =
    List.concatMap
        (\(Index item) ->
            if List.isEmpty item.children then
                [ { id = item.id, name = item.name } ]

            else
                flattenIndex item.children
        )


extractGroup : Playground e t -> Maybe { name : String, pages : List { id : String, name : String } }
extractGroup pg =
    case pg of
        Page _ _ ->
            Nothing

        Group meta children ->
            Just { name = meta.name, pages = List.concatMap extractFlatIndex children }


extractFlatIndex : Playground e t -> List { id : String, name : String }
extractFlatIndex pg =
    case pg of
        Page meta _ ->
            [ { id = meta.id, name = meta.name } ]

        Group _ children ->
            List.concatMap extractFlatIndex children


processPlayground :
    Library_ e t
    -> Maybe String
    -> Playground e t
    -> State Ref (List ( String, List (ProcessedFrame e t) ))
processPlayground library prefix pg =
    case pg of
        Page meta frames ->
            let
                lib =
                    Library meta.id library
            in
            State.traverse (processFrame lib) frames
                |> State.map
                    (\processedFrames ->
                        [ ( concatPrefix prefix meta.id, processedFrames ) ]
                    )

        Group meta children ->
            let
                prefix_ =
                    concatPrefix prefix meta.id
            in
            State.traverse (processPlayground library (Just prefix_)) children
                |> State.map List.concat


concatPrefix : Maybe String -> String -> String
concatPrefix prefix string =
    case prefix of
        Nothing ->
            string

        Just prefix_ ->
            prefix_ ++ "/" ++ string


processFrame : Library e t -> Frame e t -> State Ref (ProcessedFrame e t)
processFrame lib frame =
    case frame of
        InteractiveFrame meta f wrapper ->
            State.map (ProcessedInteractive { id = meta.id, name = meta.name } wrapper) (f lib)

        PresetsFrame meta f wrapper ->
            State.map (ProcessedPresets { id = meta.id, name = meta.name } wrapper) (f lib)

        StaticFrame html ->
            State.state (ProcessedStatic html)

        GalleryFrame f ->
            State.map ProcessedGallery (f lib)

        SubheadingFrame label ->
            State.state (ProcessedSubheading label)



-- PUBLIC API


element :
    Theme
    -> List (Playground () t)
    -> Maybe Url.Url
    -> ComponentPlayground t ()
element theme playgrounds url =
    Browser.element
        { init = \() -> ( init theme playgrounds url, Cmd.none )
        , update = \msg model -> ( update msg model |> Tuple.first, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : Theme -> List (Playground e t) -> Maybe Url.Url -> Model t e
init theme playgrounds url =
    let
        library =
            extractLibrary playgrounds

        idx =
            toIndex Nothing playgrounds

        pages =
            State.traverse (processPlayground library Nothing) playgrounds
                |> Ref.fromTop
                |> List.concat
                |> Dict.fromList

        flatPages =
            flattenIndex idx

        currentPage =
            Maybe.andThen urlToPage url
                |> Maybe.withDefault
                    (List.head flatPages
                        |> Maybe.map .id
                        |> Maybe.withDefault ""
                    )
    in
    { state = Dict.empty
    , pages = pages
    , library = library
    , index = idx
    , currentPage = currentPage
    , search = ""
    , inspectorOpen = True
    , activeInspector = Nothing
    , collapsedGroups = Set.empty

    -- Design-token groups start collapsed: the set tracks which categories the
    -- reader has explicitly opened, so an empty set means every group is closed
    -- by default (and it copes with each component exposing a different set of
    -- categories).
    , expandedTokens = Set.empty
    , theme = theme
    }


urlToPage : Url.Url -> Maybe String
urlToPage url =
    let
        parser =
            Url.Parser.query (Url.Parser.Query.string "component")
    in
    -- see https://github.com/elm/url/issues/17
    Url.Parser.parse parser { url | path = "" }
        |> Maybe.withDefault Nothing


toUrl : String -> Model t e -> String
toUrl path model =
    Url.Builder.relative [ path ] [ Url.Builder.string "component" model.currentPage ]


update : Msg t e -> Model t e -> ( Model t e, List e )
update msg model =
    case msg of
        NoOp ->
            ( model, [] )

        ComponentUpdate (Internal.Update (Internal.ComponentInstance (Internal.ComponentRef componentId) ref) updates) ->
            let
                modelWithUpdates =
                    applyUpdates updates model
            in
            case model.library.lookupDef componentId of
                Just factory ->
                    let
                        componentE =
                            -- Same pattern as renderPortal: run the factory
                            -- starting from the instance's ref without nesting.
                            State.finalValue ref (factory (Library componentId model.library))

                        ( additionalUpdates, effects ) =
                            componentE.update
                                (lookupCurrent model)
                                (lookupCurrent modelWithUpdates)
                    in
                    ( applyUpdates additionalUpdates modelWithUpdates, effects )

                Nothing ->
                    ( modelWithUpdates, [] )

        ViewPage pageId ->
            -- Reset the inspector target when navigating: the new page's first
            -- inspectable frame becomes active (resolved lazily in the view).
            ( { model | currentPage = pageId, activeInspector = Nothing }, [] )

        UpdateSearch newSearch ->
            ( { model | search = newSearch }, [] )

        ToggleInspector ->
            ( { model | inspectorOpen = not model.inspectorOpen }, [] )

        SelectInspector frameId ->
            ( { model | activeInspector = Just frameId, inspectorOpen = True }, [] )

        ToggleGroup groupId ->
            ( { model | collapsedGroups = toggleMember groupId model.collapsedGroups }, [] )

        ToggleTokenGroup groupName ->
            ( { model | expandedTokens = toggleMember groupName model.expandedTokens }, [] )


toggleMember : comparable -> Set comparable -> Set comparable
toggleMember key set =
    if Set.member key set then
        Set.remove key set

    else
        Set.insert key set


lookupCurrent : Model t e -> Ref -> Maybe (Type t)
lookupCurrent model ref =
    Dict.get (Ref.toString ref) model.state


applyUpdates : List ( Ref, Type t ) -> Model t e -> Model t e
applyUpdates updates model =
    { model
        | state =
            List.foldl
                (\( ref, t ) ->
                    Dict.insert (Ref.toString ref) t
                )
                model.state
                updates
    }



-- PORTAL RENDERING


{-| Render a named portal for a component instance. Returns `Nothing` if
the component definition or portal name is not found.

Use this inside `Control.withUpdate` content closures to produce lazy
portal HTML:

    \(PlaygroundModel model) ->
        Component.Application.renderPortal model instance "dropdown-menu"

-}
renderPortal : Model t e -> ComponentInstance -> String -> Maybe (Html (Msg t e))
renderPortal model (ComponentInstance (Internal.ComponentRef componentId) ref) portalName =
    model.library.lookupDef componentId
        |> Maybe.andThen
            (\factory ->
                let
                    lib =
                        Library componentId model.library

                    componentE =
                        -- Run the factory starting from ref directly, without
                        -- nesting. The factory already contains its own nesting
                        -- (Ref.take + Ref.from inside Frame.fromComponent), so
                        -- using Ref.from here would double-nest the Refs.
                        State.finalValue ref (factory lib)

                    ( _, portals ) =
                        componentE.render (lookupCurrent model)
                in
                Dict.get portalName portals
                    |> Maybe.map (Html.map ComponentUpdate)
            )


{-| Wrap an `Update` into a `Msg`. Useful for dispatching state changes
produced by a `withUpdate` setter through effect callbacks (e.g. a
popover's `onClick` handler).
-}
fromUpdate : ComponentUpdate t -> Msg t e
fromUpdate =
    ComponentUpdate



-- VIEW
-- The playground shell: a left navigation sidebar, a top ribbon (breadcrumbs +
-- Inspector trigger), the documentation column with a dedicated Playground card,
-- and a full-height right-side Inspector that pushes the content in (Figma-style)
-- rather than floating over it. Chrome is styled with the design-system token
-- values (surface / line / ink / radius / elevation / spacing) so the shell is
-- itself an example of correct design-system usage.


view : Model t e -> Html (Msg t e)
view model =
    let
        theme =
            model.theme

        inspectables =
            pageInspectables model

        showInspector =
            model.inspectorOpen && not (List.isEmpty inspectables)
    in
    Html.div
        [ Html.Attributes.class "cp-root"
        , Ui.style "display" "flex"
        , Ui.style "flex-direction" "row"
        , Ui.style "height" "100vh"
        , Ui.style "background-color" theme.backgroundColor
        , Ui.style "color" theme.textColor
        ]
        [ shellStylesheet
        , viewSidebar model
        , viewMainColumn model inspectables
        , if showInspector then
            viewInspectorPanel model inspectables

          else
            Html.text ""
        ]


{-| Interactive (Interactive / Presets) frames on the current page, paired with
their ready-to-render control lists. These are the page's "live components"; the
Inspector targets one at a time and shows tabs when there is more than one.
-}
type alias Inspectable t e =
    { id : String
    , name : String
    , controls : List (Html (Msg t e))
    , tokens : List Internal.TokenGroup
    }


pageInspectables : Model t e -> List (Inspectable t e)
pageInspectables model =
    let
        theme =
            model.theme

        lookup =
            lookupCurrent model
    in
    Dict.get model.currentPage model.pages
        |> Maybe.withDefault []
        |> List.filterMap
            (\frame ->
                case frame of
                    ProcessedInteractive meta _ internals ->
                        Just
                            { id = meta.id
                            , name = meta.name
                            , controls = List.map (Html.map ComponentUpdate) (internals.controls theme lookup)
                            , tokens = internals.tokens
                            }

                    ProcessedPresets meta _ internals ->
                        Just
                            { id = meta.id
                            , name = meta.name
                            , controls = List.map (Html.map ComponentUpdate) (internals.innerControls theme lookup)
                            , tokens = internals.tokens
                            }

                    _ ->
                        Nothing
            )


{-| The Inspector's current target: the explicitly selected frame if it still
exists on the page, otherwise the first inspectable frame.
-}
activeInspectable : Model t e -> List (Inspectable t e) -> Maybe (Inspectable t e)
activeInspectable model inspectables =
    case model.activeInspector of
        Just id ->
            case List.filter (\i -> i.id == id) inspectables of
                first :: _ ->
                    Just first

                [] ->
                    List.head inspectables

        Nothing ->
            List.head inspectables



-- SHELL STYLESHEET
-- Hover / focus / active states, the open-Inspector transition, and the
-- narrow-viewport overlay behaviour. Layout properties that the media query
-- must override (the Inspector's position) live here in classes rather than
-- inline, so the breakpoint can win.


shellStylesheet : Html msg
shellStylesheet =
    Html.node "style"
        []
        [ Html.text <|
            String.join "\n"
                [ ".cp-root, .cp-root *{box-sizing:border-box;}"
                , ".cp-nav-row{transition:background-color .12s ease,color .12s ease;}"

                -- !important: Ui.button sets `background:none` inline, which would
                -- otherwise beat these class rules and swallow the hover / selected
                -- fills.
                , ".cp-nav-row:hover{background:" ++ dsSurfaceAlt ++ " !important;}"
                , ".cp-nav-row:focus-visible{outline:2px solid " ++ dsAccent ++ ";outline-offset:-2px;}"
                , ".cp-nav-row.is-active{background:" ++ dsBrandBlue50 ++ " !important;}"
                , ".cp-nav-row.is-active:hover{background:" ++ dsBrandBlue50 ++ " !important;}"
                , ".cp-nav-icon{color:" ++ dsInk2 ++ ";transition:color .12s ease;}"
                , ".cp-nav-row:hover .cp-nav-icon{color:" ++ dsInk ++ ";}"
                , ".cp-nav-row.is-active .cp-nav-icon{color:" ++ dsAccent ++ ";}"
                , ".cp-nav-row.contains-active .cp-nav-icon{color:" ++ dsAccent ++ ";}"
                , ".cp-nav-chevron{color:" ++ dsInk4 ++ ";transition:color .12s ease;}"
                , ".cp-nav-row:hover .cp-nav-chevron{color:" ++ dsInk3 ++ ";}"
                , ".cp-icon-btn{transition:background-color .12s ease,color .12s ease;}"
                , ".cp-icon-btn:hover{background:" ++ dsSurfaceAlt ++ ";color:" ++ dsInk ++ ";}"
                , ".cp-trigger{transition:background-color .12s ease,box-shadow .12s ease,border-color .12s ease;}"
                , ".cp-trigger:hover{background:" ++ dsSurfaceAlt ++ ";}"
                , ".cp-search{transition:border-color .12s ease,box-shadow .12s ease;}"
                , ".cp-search:focus-within{border-color:" ++ dsBrandBlue ++ ";box-shadow:0 0 0 3px " ++ dsBrandBlue50 ++ ";}"

                -- The in-field clear control: a glyph that recolours on hover /
                -- focus rather than gaining its own button chrome, so it reads as
                -- part of the input.
                , ".cp-clear{color:" ++ dsInk4 ++ ";transition:color .12s ease;cursor:pointer;}"
                , ".cp-clear:hover{color:" ++ dsInk ++ ";}"
                , ".cp-clear:focus-visible{outline:2px solid " ++ dsBrandBlue ++ ";outline-offset:1px;border-radius:" ++ dsRadiusSm ++ ";color:" ++ dsInk ++ ";}"
                , ".cp-inspector{width:380px;flex-shrink:0;height:100vh;border-left:1px solid " ++ dsLine ++ ";background:" ++ dsSurface ++ ";display:flex;flex-direction:column;animation:cp-slide-in .18s ease;}"
                , ".cp-inspector-body{flex:1;min-height:0;overflow-y:auto;}"
                , "@keyframes cp-slide-in{from{transform:translateX(28px);opacity:.3;}to{transform:none;opacity:1;}}"
                , "@media (max-width:1080px){.cp-inspector{position:fixed;top:0;right:0;bottom:0;height:auto;z-index:1000;box-shadow:" ++ dsShadow4 ++ ";}}"
                ]
        ]



-- SIDEBAR


viewSidebar : Model t e -> Html (Msg t e)
viewSidebar model =
    let
        theme =
            model.theme

        footerBand =
            case theme.sidebarFooter of
                Just content ->
                    [ Html.div
                        [ Ui.style "padding" "16px 20px"
                        , Ui.style "border-top" ("1px solid " ++ dsLine2)
                        ]
                        [ Html.map never content ]
                    ]

                Nothing ->
                    []
    in
    Ui.vStack
        [ Ui.style "width" "300px"
        , Ui.style "flex-shrink" "0"
        , Ui.style "height" "100vh"
        , Ui.style "background" dsSidebar
        , Ui.style "border-right" ("1px solid " ++ dsLine)
        ]
        (List.concat
            [ [ Html.div
                    (Ui.headingStyles theme
                        ++ [ Ui.style "padding" "28px 20px 20px 20px"
                           , Ui.style "white-space" "nowrap"
                           , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
                           ]
                    )
                    [ Html.map never theme.sidebarHeader ]
              , viewSearchBand model
              , Html.div
                    [ Ui.style "flex-grow" "1"
                    , Ui.style "min-height" "0"
                    , Ui.style "overflow-y" "auto"
                    , Ui.style "padding" "8px 12px 24px 12px"
                    ]
                    (viewNavList model)
              ]
            , footerBand
            ]
        )


viewSearchBand : Model t e -> Html (Msg t e)
viewSearchBand model =
    let
        theme =
            model.theme

        hasText =
            model.search /= ""

        clearButton =
            if hasText then
                [ Ui.button theme
                    [ Html.Attributes.class "cp-clear"
                    , Html.Attributes.type_ "button"
                    , Html.Attributes.attribute "aria-label" "Clear search"
                    , Ui.style "display" "inline-flex"
                    , Ui.style "align-items" "center"
                    , Ui.style "justify-content" "center"
                    , Ui.style "flex-shrink" "0"

                    -- Clear on click (covers mouse and keyboard Enter/Space), and
                    -- swallow the mousedown default so a pointer press doesn't blur
                    -- the input — focus stays in the field after clearing.
                    , Html.Events.onClick (UpdateSearch "")
                    , Html.Events.preventDefaultOn "mousedown" (Decode.succeed ( NoOp, True ))
                    ]
                    [ iconBox 14 (Ui.phosphorX "") ]
                ]

            else
                []
    in
    Html.div
        [ Ui.style "padding" "12px 16px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
        ]
        [ Html.label
            [ Html.Attributes.class "cp-search"
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "gap" dsSpace2
            , Ui.style "height" "38px"
            , Ui.style "padding" "0 12px"
            , Ui.style "background" dsSurface
            , Ui.style "border" ("1px solid " ++ dsLine)
            , Ui.style "border-radius" dsRadiusMd
            , Ui.style "color" dsInk4
            ]
            (iconBox 16 (Ui.phosphorMagnifyingGlass "")
                :: Html.input
                    [ Html.Attributes.placeholder "Search components…"
                    , Html.Attributes.value model.search
                    , Html.Events.onInput UpdateSearch
                    , Html.Attributes.id "playground-search"

                    -- Escape clears the field too — the keyboard path that keeps
                    -- focus in the input (the field never loses focus).
                    , onEscape (UpdateSearch "")
                    , Ui.style "border" "none"
                    , Ui.style "outline" "none"
                    , Ui.style "padding" "0"
                    , Ui.style "flex-grow" "1"
                    , Ui.style "min-width" "0"
                    , Ui.style "background-color" "transparent"
                    , Ui.style "font-family" theme.fontFamily
                    , Ui.style "font-size" "14px"
                    , Ui.style "color" dsInk
                    , Ui.disableAutocomplete
                    ]
                    []
                :: clearButton
            )
        ]


{-| Fire `msg` when the Escape key is pressed in an input.
-}
onEscape : Msg t e -> Html.Attribute (Msg t e)
onEscape msg =
    Html.Events.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Escape" then
                        Decode.succeed msg

                    else
                        Decode.fail "non-escape key"
                )
        )


viewNavList : Model t e -> List (Html (Msg t e))
viewNavList model =
    -- Top-level sections render in source order (the author's section sequence).
    model.index
        |> List.filter (indexHasMatch model.search)
        |> List.map (viewNavNode model 0)


viewNavNode : Model t e -> Int -> Index -> Html (Msg t e)
viewNavNode model depth (Index item) =
    if List.isEmpty item.children then
        viewPageLink model depth { id = item.id, name = item.name }

    else
        let
            filteredChildren =
                item.children
                    |> List.filter (indexHasMatch model.search)
                    |> orderChildren depth

            -- A search with matches force-opens its groups so results are visible.
            isOpen =
                not (Set.member item.id model.collapsedGroups) || model.search /= ""

            containsActive =
                List.any (nodeContainsActive model.currentPage) item.children

            children =
                if isOpen then
                    List.map (viewNavNode model (depth + 1)) filteredChildren

                else
                    []
        in
        Ui.vStack [] (viewGroupRow model depth item isOpen containsActive :: children)


{-| Does this index subtree contain the currently-selected page? Used to give a
parent category the "expanded / contains selection" treatment so the path to the
active page reads clearly.
-}
nodeContainsActive : String -> Index -> Bool
nodeContainsActive current (Index item) =
    if List.isEmpty item.children then
        item.id == current

    else
        List.any (nodeContainsActive current) item.children


{-| The leading category/page glyph. Coloured by `.cp-nav-icon` (muted, brighter
on hover, accent when the row is active) so it tracks the row state.
-}
navLeadingIcon : Html msg -> Html msg
navLeadingIcon icon =
    Html.span
        [ Html.Attributes.class "cp-nav-icon"
        , Ui.style "display" "inline-flex"
        , Ui.style "flex-shrink" "0"
        ]
        [ iconBox 17 icon ]


viewGroupRow : Model t e -> Int -> { id : String, name : String, children : List Index } -> Bool -> Bool -> Html (Msg t e)
viewGroupRow model depth item isOpen containsActive =
    let
        theme =
            model.theme

        chevron =
            Html.span
                [ Html.Attributes.class "cp-nav-chevron", Ui.style "display" "inline-flex" ]
                [ iconBox 13
                    (if isOpen then
                        Ui.phosphorCaretDown ""

                     else
                        Ui.phosphorCaretRight ""
                    )
                ]
    in
    if depth == 0 then
        -- Top-level section header — an uppercase, collapsible band.
        Ui.button theme
            [ Html.Attributes.class "cp-nav-row"
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "space-between"
            , Ui.style "width" "100%"
            , Ui.style "height" "30px"
            , Ui.style "margin-top" "14px"
            , Ui.style "margin-bottom" "2px"
            , Ui.style "padding" "0 8px"
            , Ui.style "border-radius" dsRadiusMd
            , Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "11px"
            , Ui.style "font-weight" "700"
            , Ui.style "letter-spacing" "0.07em"
            , Ui.style "text-transform" "uppercase"
            , Ui.style "color" dsInk4
            , Ui.onClick (ToggleGroup item.id)
            ]
            [ Html.span [] [ Html.text item.name ]
            , chevron
            ]

    else
        -- Nested, expandable category (e.g. Button). Carries a leading glyph and,
        -- when it contains the active page, an accent icon + label so the path to
        -- the selection reads as one highlighted group.
        Ui.button theme
            [ Html.Attributes.class
                ("cp-nav-row"
                    ++ (if containsActive then
                            " contains-active"

                        else
                            ""
                       )
                )
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "space-between"
            , Ui.style "gap" "8px"
            , Ui.style "width" "100%"
            , Ui.style "height" "34px"
            , Ui.style "padding-left" (navIndent depth)
            , Ui.style "padding-right" "10px"
            , Ui.style "border-radius" dsRadiusMd
            , Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "14px"
            , Ui.style "font-weight" "600"
            , Ui.style "color"
                (if containsActive then
                    dsAccent

                 else
                    dsInk2
                )
            , Ui.onClick (ToggleGroup item.id)
            ]
            [ Html.div
                [ Ui.style "display" "flex"
                , Ui.style "align-items" "center"
                , Ui.style "gap" "10px"
                , Ui.style "min-width" "0"
                ]
                [ navLeadingIcon (Ui.phosphorSquaresFour "")
                , Html.span
                    [ Ui.style "overflow" "hidden"
                    , Ui.style "text-overflow" "ellipsis"
                    , Ui.style "white-space" "nowrap"
                    ]
                    [ Html.text item.name ]
                ]
            , chevron
            ]


viewPageLink : Model t e -> Int -> { id : String, name : String } -> Html (Msg t e)
viewPageLink model depth meta =
    let
        theme =
            model.theme

        isActive =
            meta.id == model.currentPage

        -- Primary nav entries (depth 1, e.g. the Design System pages) carry a
        -- leading glyph; deeper leaf pages under a category do not, so the
        -- category's icon stays the anchor for its group.
        leading =
            if depth <= 1 then
                [ navLeadingIcon (Ui.phosphorCube "") ]

            else
                []
    in
    Ui.button theme
        [ Html.Attributes.class
            ("cp-nav-row"
                ++ (if isActive then
                        " is-active"

                    else
                        ""
                   )
            )
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "gap" "8px"
        , Ui.style "width" "100%"
        , Ui.style "height" "34px"
        , Ui.style "padding-left" (navIndent depth)
        , Ui.style "padding-right" "10px"
        , Ui.style "border-radius" dsRadiusMd
        , Ui.style "font-family" theme.fontFamily
        , Ui.style "font-size" "14px"
        , Ui.style "font-weight"
            (if isActive then
                "600"

             else
                "400"
            )
        , Ui.style "color"
            (if isActive then
                dsInk

             else
                dsInk3
            )
        , Ui.onClick (ViewPage meta.id)
        ]
        [ Html.div
            [ Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "gap" "10px"
            , Ui.style "min-width" "0"
            ]
            (leading
                ++ [ Html.span
                        [ Ui.style "overflow" "hidden"
                        , Ui.style "text-overflow" "ellipsis"
                        , Ui.style "white-space" "nowrap"
                        ]
                        [ Html.text meta.name ]
                   ]
            )
        , if isActive then
            Html.span
                [ Ui.style "width" "6px"
                , Ui.style "height" "6px"
                , Ui.style "flex-shrink" "0"
                , Ui.style "border-radius" "50%"
                , Ui.style "background" dsAccent
                ]
                []

          else
            Html.text ""
        ]


navIndent : Int -> String
navIndent depth =
    String.fromInt (8 + depth * 14) ++ "px"


{-| Order a node's children for display.

  - A top-level section's catalog (`depth == 0`) is sorted alphabetically by
    name with pages and groups **interleaved**, so a sub-category (e.g. Button)
    sits in its natural alphabetical slot among the leaf components rather than
    being pushed to the end.
  - Deeper, curated sub-categories (`depth >= 1`, e.g. the pages inside Button)
    keep their **source order**, so the author controls the sequence.

-}
orderChildren : Int -> List Index -> List Index
orderChildren depth children =
    if depth == 0 then
        List.sortBy (\(Index item) -> String.toLower item.name) children

    else
        children


indexHasMatch : String -> Index -> Bool
indexHasMatch search (Index item) =
    if List.isEmpty item.children then
        String.toLower item.name |> String.contains (String.toLower search)

    else
        List.any (indexHasMatch search) item.children



-- MAIN COLUMN


viewMainColumn : Model t e -> List (Inspectable t e) -> Html (Msg t e)
viewMainColumn model inspectables =
    Ui.vStack
        [ Ui.style "flex-grow" "1"
        , Ui.style "min-width" "0"
        , Ui.style "height" "100vh"
        ]
        [ viewTopRibbon model inspectables
        , Html.div
            [ Ui.style "flex-grow" "1"
            , Ui.style "min-height" "0"
            , Ui.style "overflow-y" "auto"
            , Ui.style "background" dsAppBg
            ]
            [ viewContent model ]
        ]


viewTopRibbon : Model t e -> List (Inspectable t e) -> Html (Msg t e)
viewTopRibbon model inspectables =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "flex-shrink" "0"
        , Ui.style "gap" dsSpace4
        , Ui.style "height" "56px"
        , Ui.style "padding" "0 24px"
        , Ui.style "background" dsSurface
        , Ui.style "border-bottom" ("1px solid " ++ dsLine)
        , Ui.style "box-shadow" dsShadow1
        ]
        [ viewBreadcrumbs model

        -- The ribbon trigger only reopens the Inspector — while the panel is open
        -- it is redundant (the panel has its own close control), so hide it.
        , if List.isEmpty inspectables || model.inspectorOpen then
            Html.text ""

          else
            inspectorTrigger model
        ]


viewBreadcrumbs : Model t e -> Html (Msg t e)
viewBreadcrumbs model =
    let
        theme =
            model.theme

        path =
            pathTo model.currentPage model.index
                |> Maybe.withDefault []

        lastIndex =
            List.length path - 1

        separator =
            Html.span
                [ Ui.style "color" dsInk4
                , Ui.style "font-size" "13px"
                ]
                [ Html.text "›" ]

        crumb i meta =
            let
                isLast =
                    i == lastIndex
            in
            Html.span
                [ Ui.style "font-family" theme.fontFamily
                , Ui.style "font-size" "13px"
                , Ui.style "font-weight"
                    (if isLast then
                        "600"

                     else
                        "400"
                    )
                , Ui.style "color"
                    (if isLast then
                        dsInk

                     else
                        dsInk3
                    )
                , Ui.style "white-space" "nowrap"
                ]
                [ Html.text meta.name ]
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" dsSpace2
        , Ui.style "min-width" "0"
        , Ui.style "overflow" "hidden"
        ]
        (Html.span [ Ui.style "color" dsInk4 ] [ iconBox 16 (Ui.phosphorHouse "") ]
            :: List.concat
                (List.indexedMap
                    (\i meta -> [ separator, crumb i meta ])
                    path
                )
        )


pathTo : String -> List Index -> Maybe (List { id : String, name : String })
pathTo target indices =
    let
        go (Index item) =
            if List.isEmpty item.children then
                if item.id == target then
                    Just [ { id = item.id, name = item.name } ]

                else
                    Nothing

            else
                case List.filterMap go item.children of
                    found :: _ ->
                        Just ({ id = item.id, name = item.name } :: found)

                    [] ->
                        Nothing
    in
    case List.filterMap go indices of
        found :: _ ->
            Just found

        [] ->
            Nothing



-- CONTENT COLUMN


viewContent : Model t e -> Html (Msg t e)
viewContent model =
    let
        frames =
            Dict.get model.currentPage model.pages
                |> Maybe.withDefault []
    in
    Html.div
        [ Ui.style "max-width" "1080px"
        , Ui.style "padding" "40px 40px 96px 40px"
        , Ui.style "display" "flex"
        , Ui.style "flex-direction" "column"
        ]
        (viewHeading model :: viewBody model frames)


{-| The page body. Configurable pages (those with a live component) get a single
structure regardless of how the author ordered their frames: the live component
leads, inside a Playground callout, and everything else — specimens, variant
charts, usage and behaviour notes — drops below it under one **Reference**
section. Pure token / reference pages (no live component) render their frames
as-authored.
-}
viewBody : Model t e -> List (ProcessedFrame e t) -> List (Html (Msg t e))
viewBody model frames =
    case splitLive frames of
        Just { live, rest } ->
            viewPlaygroundCallout model live :: referenceSection model rest

        Nothing ->
            viewFramesList model frames


{-| Split a page into its primary live component (the first interactive / presets
frame) and the reference content that follows it. Returns `Nothing` for pages
with no live component, leaving them untouched.

The live frame's own preceding subheading (its label, e.g. an author's
"Playground" heading or a "Default button" specimen label) is dropped — the
callout supplies its own header — as is any stray "Playground" subheading
elsewhere. Everything else is preserved in author order for the Reference
section.

-}
splitLive : List (ProcessedFrame e t) -> Maybe { live : ProcessedFrame e t, rest : List (ProcessedFrame e t) }
splitLive frames =
    case splitAtLive [] frames of
        Just ( before, live, after ) ->
            let
                beforeTrimmed =
                    case List.reverse before of
                        (ProcessedSubheading _) :: earlier ->
                            List.reverse earlier

                        _ ->
                            before

                rest =
                    (beforeTrimmed ++ after)
                        |> List.filter (not << isPlaygroundSubheading)
            in
            Just { live = live, rest = rest }

        Nothing ->
            Nothing


splitAtLive : List (ProcessedFrame e t) -> List (ProcessedFrame e t) -> Maybe ( List (ProcessedFrame e t), ProcessedFrame e t, List (ProcessedFrame e t) )
splitAtLive acc frames =
    case frames of
        [] ->
            Nothing

        frame :: more ->
            if isLive frame then
                Just ( List.reverse acc, frame, more )

            else
                splitAtLive (frame :: acc) more


isLive : ProcessedFrame e t -> Bool
isLive frame =
    case frame of
        ProcessedInteractive _ _ _ ->
            True

        ProcessedPresets _ _ _ ->
            True

        _ ->
            False


isPlaygroundSubheading : ProcessedFrame e t -> Bool
isPlaygroundSubheading frame =
    case frame of
        ProcessedSubheading label ->
            String.toLower (String.trim label) == "playground"

        _ ->
            False


referenceSection : Model t e -> List (ProcessedFrame e t) -> List (Html (Msg t e))
referenceSection model rest =
    if List.isEmpty rest then
        []

    else
        referenceHeading model.theme :: viewFramesList model rest


{-| The "Reference" section heading — the primary documentation heading for the
page, opening the supporting material below the live Playground component.

It is a full **H2** from the type scale (24 / 600), so it clearly dominates the
content headings beneath it (variant charts, specimens, usage and behaviour
notes), which render one step down at H4 (see `sectionLabel`). The book-open
icon pairs it with the Playground flask while the larger type makes it read as
the section owner, not a peer label.

-}
referenceHeading : Theme -> Html (Msg t e)
referenceHeading theme =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" dsSpace2
        , Ui.style "margin-top" "48px"
        , Ui.style "margin-bottom" dsSpace3
        , Ui.style "color" dsInk3
        ]
        [ iconBox 22 (Ui.phosphorBookOpen "")
        , Html.span
            [ Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "24px"
            , Ui.style "font-weight" "600"
            , Ui.style "color" dsInk
            ]
            [ Html.text "Reference" ]
        ]


viewHeading : Model t e -> Html (Msg t e)
viewHeading model =
    let
        theme =
            model.theme

        pageName =
            lookupPageName model.currentPage model.index
                |> Maybe.withDefault ""

        crumbs =
            pathTo model.currentPage model.index
                |> Maybe.withDefault []

        subtitle =
            crumbs
                |> List.reverse
                |> List.drop 1
                |> List.reverse
                |> List.map .name
                |> String.join " · "
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" dsSpace4
        , Ui.style "margin-bottom" "8px"
        ]
        [ Html.div
            [ Ui.style "width" "44px"
            , Ui.style "height" "44px"
            , Ui.style "flex-shrink" "0"
            , Ui.style "display" "inline-flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "center"
            , Ui.style "background" dsBrandBlue50
            , Ui.style "border-radius" dsRadiusLg
            , Ui.style "color" dsBrandBlue
            ]
            [ iconBox 22 (Ui.phosphorCube "") ]
        , Ui.vStack [ Ui.style "gap" "2px", Ui.style "min-width" "0" ]
            (List.concat
                [ if String.isEmpty subtitle then
                    []

                  else
                    [ Html.div
                        [ Ui.style "font-family" theme.fontFamily
                        , Ui.style "font-size" "11px"
                        , Ui.style "font-weight" "600"
                        , Ui.style "letter-spacing" "0.06em"
                        , Ui.style "text-transform" "uppercase"
                        , Ui.style "color" dsInk4
                        ]
                        [ Html.text subtitle ]
                    ]
                , [ Html.div
                        [ Ui.style "font-family" theme.fontFamily
                        , Ui.style "font-size" "28px"
                        , Ui.style "font-weight" "700"
                        , Ui.style "color" dsInk
                        ]
                        [ Html.text pageName ]
                  ]
                ]
            )
        ]


viewFramesList : Model t e -> List (ProcessedFrame e t) -> List (Html (Msg t e))
viewFramesList model frames =
    List.indexedMap (\i frame -> viewFrame model (i == 0) frame) frames


viewFrame : Model t e -> Bool -> ProcessedFrame e t -> Html (Msg t e)
viewFrame model isFirst frame =
    case frame of
        ProcessedInteractive _ wrapper internals ->
            playgroundCard Nothing (renderComponentView model internals wrapper identity)

        ProcessedPresets _ wrapper internals ->
            let
                ( tabBar, presetWrap ) =
                    presetBits model internals
            in
            playgroundCard tabBar (renderComponentView model internals wrapper presetWrap)

        ProcessedStatic html ->
            Html.div
                [ Ui.style "margin-top"
                    (if isFirst then
                        "0"

                     else
                        dsSpace2
                    )
                , Ui.style "color" dsInk3
                , Ui.style "font-size" "14px"
                ]
                [ Html.map ComponentUpdate html ]

        ProcessedGallery html ->
            specimenBlock (Html.map ComponentUpdate html)

        ProcessedSubheading label ->
            sectionLabel model.theme isFirst label


renderComponentView : Model t e -> ComponentE e t -> (Html (Update t) -> Html (Update t)) -> (Html (Update t) -> Html (Update t)) -> Html (Msg t e)
renderComponentView model internals wrapper viewWrap =
    internals.render (lookupCurrent model)
        |> Tuple.first
        |> viewWrap
        |> wrapper
        |> Html.map ComponentUpdate


presetBits : Model t e -> ComponentE e t -> ( Maybe (Html (Msg t e)), Html (Update t) -> Html (Update t) )
presetBits model internals =
    case internals.presets of
        Just info ->
            let
                lookup =
                    lookupCurrent model
            in
            ( Just (viewPresetTabBar model.theme info lookup)
            , info.current lookup
                |> Maybe.map info.wrapAt
                |> Maybe.withDefault identity
            )

        Nothing ->
            ( Nothing, identity )


{-| Render the page's primary live component as the Playground callout: the
polished preview container, headed by the Playground icon + title, with the live
component (left aligned) below. This is the focus of the page and sits directly
under the heading.
-}
viewPlaygroundCallout : Model t e -> ProcessedFrame e t -> Html (Msg t e)
viewPlaygroundCallout model frame =
    case frame of
        ProcessedInteractive _ wrapper internals ->
            playgroundCallout model.theme Nothing (renderComponentView model internals wrapper identity)

        ProcessedPresets _ wrapper internals ->
            let
                ( tabBar, presetWrap ) =
                    presetBits model internals
            in
            playgroundCallout model.theme tabBar (renderComponentView model internals wrapper presetWrap)

        _ ->
            Html.text ""


{-| The Playground callout: `playgroundCard` chrome with a header row (the
Playground icon + title) inside the same bordered surface, above the live
component.
-}
playgroundCallout : Theme -> Maybe (Html (Msg t e)) -> Html (Msg t e) -> Html (Msg t e)
playgroundCallout theme maybeTabBar inner =
    playgroundShell (playgroundHeaderRow theme :: tabBarItems maybeTabBar) inner


playgroundHeaderRow : Theme -> Html (Msg t e)
playgroundHeaderRow theme =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" dsSpace2
        , Ui.style "padding" "14px 20px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
        , Ui.style "color" dsInk3
        ]
        [ iconBox 18 (Ui.phosphorFlask "")
        , Html.span
            [ Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "14px"
            , Ui.style "font-weight" "600"
            , Ui.style "color" dsInk2
            ]
            [ Html.text "Playground" ]
        ]


tabBarItems : Maybe (Html (Msg t e)) -> List (Html (Msg t e))
tabBarItems maybeTabBar =
    case maybeTabBar of
        Just tabBar ->
            [ tabBar ]

        Nothing ->
            []


{-| The Playground live-component container: a polished surface (line border,
radius, soft elevation) holding a single live component, left aligned. The
component's variant / size / state are driven from the Inspector. `topItems` are
rendered inside the surface above the component (the Playground header row and/or
a preset tab bar).
-}
playgroundShell : List (Html (Msg t e)) -> Html (Msg t e) -> Html (Msg t e)
playgroundShell topItems inner =
    Html.div
        [ Ui.style "margin-top" "16px"
        , Ui.style "background" dsSurface
        , Ui.style "border" ("1px solid " ++ dsLine)
        , Ui.style "border-radius" dsRadiusLg
        , Ui.style "box-shadow" dsShadow2

        -- Visible (not hidden) so an open dropdown / popover menu inside the live
        -- component can float out of the callout instead of being clipped. The
        -- card's own background still respects the radius; children are padded in
        -- and carry no full-bleed fill, so corners stay clean.
        , Ui.style "overflow" "visible"
        ]
        (topItems
            ++ [ Html.div
                    [ Ui.style "display" "flex"
                    , Ui.style "flex-direction" "column"
                    , Ui.style "align-items" "flex-start"
                    , Ui.style "padding" "28px"
                    , Ui.style "min-width" "0"
                    ]
                    [ inner ]
               ]
        )


playgroundCard : Maybe (Html (Msg t e)) -> Html (Msg t e) -> Html (Msg t e)
playgroundCard maybeTabBar inner =
    playgroundShell (tabBarItems maybeTabBar) inner


{-| A reference / specimen block (variant matrices, size charts). Unlike the
Playground card — a contained live panel with border, radius and elevation — the
specimen section reads as an open, table-like reference: no filled box, just a
hairline rule above it for separation.

Overflow is left **visible** rather than `overflow-x: auto`: an `auto` box is a
scroll container, which clips an open dropdown / popover specimen vertically.
Specimens lay their content out with wrapping rows, so they don't need the
horizontal-scroll affordance; any genuinely wide specimen should wrap its own
content in a scroll container.

-}
specimenBlock : Html (Msg t e) -> Html (Msg t e)
specimenBlock html =
    Html.div
        [ Ui.style "margin-top" "20px"
        , Ui.style "padding-top" "20px"
        , Ui.style "border-top" ("1px solid " ++ dsLine2)
        , Ui.style "overflow" "visible"
        ]
        [ html ]


sectionLabel : Theme -> Bool -> String -> Html (Msg t e)
sectionLabel theme isFirst label =
    if label == "Playground" then
        Html.div
            [ Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "gap" dsSpace2
            , Ui.style "margin-top"
                (if isFirst then
                    "20px"

                 else
                    "40px"
                )
            , Ui.style "color" dsInk3
            ]
            [ iconBox 18 (Ui.phosphorFlask "")
            , Html.span
                [ Ui.style "font-family" theme.fontFamily
                , Ui.style "font-size" "14px"
                , Ui.style "font-weight" "600"
                , Ui.style "color" dsInk2
                ]
                [ Html.text label ]
            ]

    else
        -- A content heading inside the Reference section. One step below the
        -- Reference H2 (24 / 600) on the type scale — H4 (16 / 600) — so it
        -- reads as a subsection of Reference rather than competing with it.
        Html.div
            [ Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "16px"
            , Ui.style "font-weight" "600"
            , Ui.style "color" dsInk
            , Ui.style "margin-top"
                (if isFirst then
                    "20px"

                 else
                    "32px"
                )
            ]
            [ Html.text label ]


lookupPageName : String -> List Index -> Maybe String
lookupPageName id =
    List.foldl
        (\(Index item) acc ->
            case acc of
                Just _ ->
                    acc

                Nothing ->
                    if item.id == id && List.isEmpty item.children then
                        Just item.name

                    else
                        lookupPageName id item.children
        )
        Nothing



-- INSPECTOR
-- A full-height, right-side push-in panel (Figma-style). The main content column
-- resizes to make room rather than being overlaid. The header carries the title
-- and a close button; component metadata leads the body, optional component tabs
-- follow, then component settings, then the read-only design-token reference.


dsAppBg : String
dsAppBg =
    "#FBFBFC"


dsSidebar : String
dsSidebar =
    "#F8FAF9"


dsSurface : String
dsSurface =
    "#FEFEFE"


dsSurfaceAlt : String
dsSurfaceAlt =
    "#F1F0F5"


dsLine : String
dsLine =
    "#E5E8EC"


dsLine2 : String
dsLine2 =
    "#EEF0F3"


dsInk : String
dsInk =
    "#0A0F22"


dsInk2 : String
dsInk2 =
    "#3A4149"


dsInk3 : String
dsInk3 =
    "#5A5D66"


dsInk4 : String
dsInk4 =
    "#9DA1AC"


dsBrandBlue : String
dsBrandBlue =
    "#2F7FFE"


dsAccent : String
dsAccent =
    "#0E53F1"


dsBrandBlue50 : String
dsBrandBlue50 =
    "#EAF1FF"


dsSpace2 : String
dsSpace2 =
    "8px"


dsSpace3 : String
dsSpace3 =
    "12px"


dsSpace4 : String
dsSpace4 =
    "16px"


dsRadiusMd : String
dsRadiusMd =
    "8px"


dsRadiusLg : String
dsRadiusLg =
    "10px"


dsShadow1 : String
dsShadow1 =
    "0 1px 2px rgba(16,24,40,0.05)"


dsShadow2 : String
dsShadow2 =
    "0 2px 4px rgba(16,24,40,0.06), 0 4px 8px rgba(16,24,40,0.04)"


dsShadow4 : String
dsShadow4 =
    "0 8px 16px rgba(16,24,40,0.08), 0 24px 48px rgba(16,24,40,0.12)"


iconBox : Int -> Html msg -> Html msg
iconBox size icon =
    Html.span
        [ Ui.style "width" (String.fromInt size ++ "px")
        , Ui.style "height" (String.fromInt size ++ "px")
        , Ui.style "display" "inline-flex"
        , Ui.style "flex-shrink" "0"
        ]
        [ icon ]


{-| Closed state — a labelled, contained "Inspector" trigger in the top ribbon
(design-system button: surface, line border, radius, shadow, side-panel icon).
When the Inspector is open the trigger reads as pressed (brand-tinted).
-}
inspectorTrigger : Model t e -> Html (Msg t e)
inspectorTrigger model =
    let
        open =
            model.inspectorOpen
    in
    Ui.button model.theme
        [ Html.Attributes.class "cp-trigger"
        , Ui.style "display" "inline-flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" dsSpace2
        , Ui.style "height" "34px"
        , Ui.style "padding" "0 12px"
        , Ui.style "flex-shrink" "0"
        , Ui.style "background"
            (if open then
                dsBrandBlue50

             else
                dsSurface
            )
        , Ui.style "border"
            ("1px solid "
                ++ (if open then
                        dsBrandBlue

                    else
                        dsLine
                   )
            )
        , Ui.style "border-radius" dsRadiusMd
        , Ui.style "box-shadow" dsShadow1
        , Ui.style "color"
            (if open then
                dsBrandBlue

             else
                dsInk
            )
        , Ui.style "font-size" "13px"
        , Ui.style "font-weight" "600"
        , Html.Attributes.title "Toggle Inspector"
        , Ui.onClick ToggleInspector
        ]
        [ iconBox 16 (Ui.phosphorSidebar ""), Html.text "Inspector" ]


viewInspectorPanel : Model t e -> List (Inspectable t e) -> Html (Msg t e)
viewInspectorPanel model inspectables =
    let
        theme =
            model.theme

        active =
            activeInspectable model inspectables

        controls =
            active
                |> Maybe.map .controls
                |> Maybe.withDefault []

        tokenGroups =
            active
                |> Maybe.map .tokens
                |> Maybe.withDefault []
    in
    Html.div [ Html.Attributes.class "cp-inspector" ]
        [ inspectorHeader theme
        , Html.div [ Html.Attributes.class "cp-inspector-body" ]
            (List.concat
                [ [ inspectorMetadata theme active ]
                , if List.length inspectables > 1 then
                    [ inspectorTabs model active inspectables ]

                  else
                    []
                , [ inspectorSection theme "Component Settings" Nothing controls
                  , inspectorSection theme "Design Tokens" (Just "Used by this component") [ tokenReference model tokenGroups ]
                  ]
                ]
            )
        ]


inspectorHeader : Theme -> Html (Msg t e)
inspectorHeader theme =
    Html.div
        -- Height matches the top ribbon (56px) so the two bottom hairlines line
        -- up into one continuous rule across the ribbon and the open panel.
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "flex-shrink" "0"
        , Ui.style "height" "56px"
        , Ui.style "padding" "0 20px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine)
        ]
        [ Html.span
            [ Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "15px"
            , Ui.style "font-weight" "700"
            , Ui.style "color" dsInk
            ]
            [ Html.text "Inspector" ]
        , Ui.button theme
            [ Html.Attributes.class "cp-icon-btn"
            , Ui.style "width" "30px"
            , Ui.style "height" "30px"
            , Ui.style "border-radius" dsRadiusMd
            , Ui.style "color" dsInk3
            , Html.Attributes.title "Close inspector"
            , Ui.onClick ToggleInspector
            ]
            [ iconBox 18 (Ui.phosphorX "") ]
        ]


inspectorMetadata : Theme -> Maybe (Inspectable t e) -> Html (Msg t e)
inspectorMetadata theme active =
    case active of
        Just a ->
            Html.div
                [ Ui.style "display" "flex"
                , Ui.style "flex-direction" "column"
                , Ui.style "gap" dsSpace2
                , Ui.style "padding" "16px 20px"
                , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
                ]
                [ Html.div (eyebrowStyles theme) [ Html.text "Component" ]
                , Html.div
                    [ Ui.style "display" "flex"
                    , Ui.style "align-items" "center"
                    , Ui.style "gap" dsSpace2
                    ]
                    [ Html.span [ Ui.style "color" dsBrandBlue ] [ iconBox 16 (Ui.phosphorCube "") ]
                    , Html.span
                        [ Ui.style "font-family" theme.fontFamily
                        , Ui.style "font-size" "14px"
                        , Ui.style "font-weight" "600"
                        , Ui.style "color" dsInk
                        ]
                        [ Html.text a.name ]
                    ]
                , Html.span
                    [ Ui.style "align-self" "flex-start"
                    , Ui.style "font-family" "'Roboto Mono', monospace"
                    , Ui.style "font-size" "11px"
                    , Ui.style "color" dsInk3
                    , Ui.style "background" dsSurfaceAlt
                    , Ui.style "border" ("1px solid " ++ dsLine2)
                    , Ui.style "border-radius" dsRadiusSm
                    , Ui.style "padding" "2px 6px"
                    ]
                    [ Html.text a.id ]
                ]

        Nothing ->
            Html.text ""


inspectorTabs : Model t e -> Maybe (Inspectable t e) -> List (Inspectable t e) -> Html (Msg t e)
inspectorTabs model active inspectables =
    let
        theme =
            model.theme

        activeId =
            Maybe.map .id active

        tab inspectable =
            let
                isActive =
                    Just inspectable.id == activeId
            in
            Ui.button theme
                [ Ui.style "padding" "10px 4px 8px"
                , Ui.style "border-bottom"
                    ("2px solid "
                        ++ (if isActive then
                                dsBrandBlue

                            else
                                "transparent"
                           )
                    )
                , Ui.style "font-size" "13px"
                , Ui.style "font-weight"
                    (if isActive then
                        "600"

                     else
                        "400"
                    )
                , Ui.style "color"
                    (if isActive then
                        dsInk

                     else
                        dsInk3
                    )
                , Ui.onClick (SelectInspector inspectable.id)
                ]
                [ Html.text inspectable.name ]
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "flex-wrap" "wrap"
        , Ui.style "gap" dsSpace3
        , Ui.style "padding" "0 20px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
        ]
        (List.map tab inspectables)


eyebrowStyles : Theme -> List (Html.Attribute msg)
eyebrowStyles theme =
    [ Ui.style "font-family" theme.fontFamily
    , Ui.style "font-size" "10px"
    , Ui.style "font-weight" "600"
    , Ui.style "letter-spacing" "0.08em"
    , Ui.style "text-transform" "uppercase"
    , Ui.style "color" dsInk4
    ]


dsRadiusSm : String
dsRadiusSm =
    "4px"


inspectorSection : Theme -> String -> Maybe String -> List (Html msg) -> Html msg
inspectorSection theme title caption content =
    Html.div
        [ Ui.style "padding" "16px 20px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine2)
        , Ui.style "display" "flex"
        , Ui.style "flex-direction" "column"
        , Ui.style "gap" dsSpace3
        ]
        (Html.div (eyebrowStyles theme)
            (Html.text title
                :: (case caption of
                        Just c ->
                            [ Html.span
                                [ Ui.style "text-transform" "none"
                                , Ui.style "letter-spacing" "0"
                                , Ui.style "font-weight" "400"
                                , Ui.style "color" dsInk4
                                , Ui.style "margin-left" dsSpace2
                                ]
                                [ Html.text c ]
                            ]

                        Nothing ->
                            []
                   )
            )
            :: [ Html.div
                    [ Ui.style "display" "flex"
                    , Ui.style "flex-direction" "column"
                    , Ui.style "gap" dsSpace2
                    ]
                    content
               ]
        )


{-| The Design Tokens reference for the selected component. Each group is the
component's own declared token usage (via `Component.withTokens`), so the list
varies by component and never shows a category the component does not consume.
Groups are collapsed by default and read-only. When a component declares no
token metadata, a short note is shown rather than a misleading global list.
-}
tokenReference : Model t e -> List Internal.TokenGroup -> Html (Msg t e)
tokenReference model groups =
    if List.isEmpty groups then
        Html.span
            [ Ui.style "font-family" model.theme.fontFamily
            , Ui.style "font-size" "12px"
            , Ui.style "color" dsInk4
            ]
            [ Html.text "Token usage isn’t documented for this component yet." ]

    else
        Html.div
            [ Ui.style "display" "flex"
            , Ui.style "flex-direction" "column"
            , Ui.style "gap" dsSpace2
            ]
            (List.map (tokenGroupView model) groups)


tokenGroupView : Model t e -> Internal.TokenGroup -> Html (Msg t e)
tokenGroupView model group =
    let
        theme =
            model.theme

        expanded =
            Set.member group.category model.expandedTokens
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "flex-direction" "column"
        , Ui.style "gap" "4px"
        ]
        (Ui.button theme
            [ Html.Attributes.class "cp-nav-row"
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "space-between"
            , Ui.style "width" "100%"
            , Ui.style "padding" "4px 6px"
            , Ui.style "border-radius" dsRadiusSm
            , Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "11px"
            , Ui.style "font-weight" "600"
            , Ui.style "color" dsInk2
            , Ui.onClick (ToggleTokenGroup group.category)
            ]
            [ Html.span
                [ Ui.style "display" "flex"
                , Ui.style "align-items" "center"
                , Ui.style "gap" "6px"
                ]
                [ Html.text group.category
                , Html.span
                    [ Ui.style "font-weight" "500"
                    , Ui.style "color" dsInk4
                    ]
                    [ Html.text (String.fromInt (List.length group.tokens)) ]
                ]
            , Html.span [ Ui.style "color" dsInk4 ]
                [ iconBox 14
                    (if expanded then
                        Ui.phosphorCaretDown ""

                     else
                        Ui.phosphorCaretRight ""
                    )
                ]
            ]
            :: (if expanded then
                    List.map (tokenRow theme) group.tokens

                else
                    []
               )
        )


tokenRow : Theme -> Internal.Token -> Html (Msg t e)
tokenRow theme { name, value } =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "gap" dsSpace2
        , Ui.style "padding" "3px 8px"
        , Ui.style "background" dsSurfaceAlt
        , Ui.style "border" ("1px solid " ++ dsLine2)
        , Ui.style "border-radius" dsRadiusSm
        ]
        [ Html.span
            [ Ui.style "font-family" "'Roboto Mono', monospace"
            , Ui.style "font-size" "11px"
            , Ui.style "color" dsInk
            ]
            [ Html.text name ]
        , Html.span
            [ Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "11px"
            , Ui.style "color" dsInk4
            ]
            [ Html.text value ]
        ]


viewPresetTabBar : Theme -> Internal.PresetsInfo t -> (Ref -> Maybe (Type t)) -> Html (Msg t e)
viewPresetTabBar theme info lookup =
    let
        activeName =
            info.current lookup

        tab name =
            let
                isActive =
                    activeName == Just name
            in
            Ui.button theme
                [ Ui.style "padding" "12px 8px 8px"
                , Ui.style "border-bottom"
                    (if isActive then
                        "2px solid " ++ dsBrandBlue

                     else
                        "2px solid transparent"
                    )
                , Ui.style "font-weight"
                    (if isActive then
                        "600"

                     else
                        "400"
                    )
                , Ui.style "font-size" "13px"
                , Ui.style "color"
                    (if isActive then
                        dsInk

                     else
                        dsInk3
                    )
                , Ui.style "cursor" "pointer"
                , Ui.onClick (ComponentUpdate (info.pick name))
                ]
                [ Html.text name ]
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "flex-direction" "row"
        , Ui.style "gap" dsSpace3
        , Ui.style "padding" "0 20px"
        , Ui.style "border-bottom" ("1px solid " ++ dsLine)
        ]
        (List.map tab info.names)
