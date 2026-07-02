module Component.Application.AiInspector exposing
    ( Model, Msg(..), Context
    , Mode(..), Tab(..)
    , SelectedElement, Bounds, AppliedToken, TokenCategory(..), TokenSource(..)
    , AiWorkItem, WorkStatus(..), AiChangedFile, FileStatus(..)
    , AiTokenChange, AiWorkActivity, ActivityStatus(..)
    , init, update, subscriptions, view
    , selectedDecoder
    , isSelecting, hasActiveWork, activeWorkComponentIds, historyFor
    , resetForNavigation, selectedSelector
    )

{-| The **AI Inspector**: a shell-level, always-available region pinned to the
bottom of the Inspector panel. It lets a user select one element in the live
component preview and then either ask an agent to make a targeted change
(Agent chat) or edit its design tokens (Token editor), tracking that work
through a component-scoped history and change-details views.

This module owns the whole feature; `Component.Application` holds a single
`aiInspector : Model` field, threads one `AiInspectorMsg`, renders `view` as the
sticky last child of the Inspector panel, wraps the live preview so selection
events can be captured, and shows a side-nav spinner for components with active
work.

**Status: front-end complete, mock-backed.** All states, selection, token
editing, work history/details, the mock lifecycle, and the title-bar/side-nav
spinners are implemented and driven by in-memory work items. The real backend /
Claude Code bridge (and host focus management) are follow-ups tracked in
`docs/ai-inspector.md`.

**Hard rule:** this module must not import any host (Planwisely/sage) module. It
uses only `Component.Ui`, `Theme` tokens, and plain CSS-class strings (including
FontAwesome class names, which the host page's kit renders).

@docs Model, Msg, Context
@docs Mode, Tab
@docs SelectedElement, Bounds, AppliedToken, TokenCategory, TokenSource
@docs AiWorkItem, WorkStatus, AiChangedFile, FileStatus
@docs AiTokenChange, AiWorkActivity, ActivityStatus
@docs init, update, subscriptions, view
@docs selectedDecoder
@docs isSelecting, hasActiveWork, activeWorkComponentIds, historyFor
@docs resetForNavigation, selectedSelector

-}

import Browser.Events
import Component.Application.Theme exposing (Theme)
import Component.Ui as Ui
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode



-- MODES / TABS


{-| Which face of the AI Inspector is showing. `ChangeDetails` carries the work
item id being viewed. History/details are reached from `Selected` and return to
it (and to the remembered `Tab`) on Back.
-}
type Mode
    = Default
    | Selecting
    | Selected
    | WorkHistory
    | ChangeDetails String


{-| The expanded selected-state tabs. The active tab is remembered across
WorkHistory/ChangeDetails navigation so Back restores it (default `AgentChat`).
-}
type Tab
    = AgentChat
    | TokenEditor



-- SELECTED ELEMENT (captured from the preview via a JS custom element)


{-| Bounding box of the selected element, in viewport pixels.
-}
type alias Bounds =
    { x : Float, y : Float, width : Float, height : Float }


{-| Where an applied token value was resolved from.
-}
type TokenSource
    = FromClass
    | FromCssVariable
    | FromComputed
    | FromProp


{-| The design-token categories the Token editor understands. The searchable
dropdown for a row only ever lists tokens of that row's category.
-}
type TokenCategory
    = Typography
    | TextColour
    | BackgroundColour
    | FontFamily
    | LineHeight
    | LetterSpacing
    | Spacing
    | Radius
    | Elevation
    | Border
    | Motion


{-| A design token currently applied to the selected element.
-}
type alias AppliedToken =
    { category : TokenCategory
    , label : String
    , value : String
    , cssProperty : Maybe String
    , cssVariable : Maybe String
    , source : Maybe TokenSource
    }


{-| Structured metadata for the one selected element. `label`/`subtitle` are the
card copy (e.g. "Title (h1)" / "Review account requirements"); `elementType` is
a human-readable role name (e.g. "Heading", "Button", "Input", "Card Title")
preferred over the raw `tagName` wherever it can be inferred.
-}
type alias SelectedElement =
    { id : String
    , label : String
    , subtitle : Maybe String
    , elementType : String
    , tagName : String
    , role : Maybe String
    , textContent : Maybe String
    , componentName : String
    , componentId : Maybe String
    , route : String
    , sourceFile : Maybe String
    , sourceSymbol : Maybe String
    , selector : Maybe String
    , classNames : List String
    , dataAttributes : List ( String, String )
    , tokens : List AppliedToken
    , computedStyles : List ( String, String )
    , bounds : Maybe Bounds
    }



-- WORK ITEMS (component-scoped, mocked this pass)


{-| Lifecycle status of a work item.
-}
type WorkStatus
    = Queued
    | InProgress
    | Completed
    | Failed


{-| Per-file status within a work item's change set.
-}
type FileStatus
    = FilePending
    | FileInProgress
    | FileChanged
    | FileFailed


{-| A file touched (or to be touched) by a work item.
-}
type alias AiChangedFile =
    { path : String, status : FileStatus, diffUrl : Maybe String }


{-| A single token change recorded on a work item (for the change-details view).
-}
type alias AiTokenChange =
    { category : String, from : Maybe String, to : String }


{-| Status of one row in the in-progress activity timeline.
-}
type ActivityStatus
    = ActPending
    | ActInProgress
    | ActCompleted
    | ActFailed


{-| One row of the in-progress activity timeline (e.g. "Planning changes").
-}
type alias AiWorkActivity =
    { label : String, status : ActivityStatus, timestamp : Maybe String }


{-| A unit of agent work. `phaseIndex`/`elapsedMs` are bookkeeping for the mock
lifecycle clock (`subscriptions` advances them); they carry no meaning once a
real backend drives status.
-}
type alias AiWorkItem =
    { id : String
    , componentId : Maybe String
    , componentName : String
    , title : String
    , status : WorkStatus
    , createdAt : String
    , updatedAt : String
    , completedAt : Maybe String
    , selectedElement : Maybe SelectedElement
    , summary : Maybe String
    , filesChanged : List AiChangedFile
    , tokensUpdated : List AiTokenChange
    , elementsAffected : Maybe Int
    , activity : List AiWorkActivity
    , phaseIndex : Int
    , elapsedMs : Float
    }



-- MODEL


{-| All AI Inspector state. `work` is keyed by component id (component-scoped
history for v1). `tokenDrafts`/`openDropdown`/`dropdownQuery`/`dropdownActive`
back the bespoke searchable token dropdowns (one open at a time), keyed by a
row key derived from the token category.
-}
type alias Model =
    { collapsed : Bool
    , mode : Mode
    , tab : Tab
    , selected : Maybe SelectedElement
    , chatInput : String
    , tokenDrafts : Dict String String
    , openDropdown : Maybe String
    , dropdownQuery : String
    , dropdownActive : Maybe Int
    , work : Dict String (List AiWorkItem)
    , nextId : Int
    }


{-| Live context the shell supplies at update/view time: which component/page is
currently shown, so new work and history are scoped correctly.

`componentId`/`componentName` are consumed now (history scoping, work items).
`route`/`sourceFile` are carried for the eventual backend task payload (see
`docs/ai-inspector.md`) and are not read yet — the shell fills them so the
contract is stable when sage integration wires the task API.

-}
type alias Context =
    { componentId : Maybe String
    , componentName : String
    , route : String
    , sourceFile : Maybe String
    }



-- MSG


{-| -}
type Msg
    = ToggleCollapsed
    | InfoClicked
    | StartSelecting
    | SelectionCaptured SelectedElement
    | SelectionCancelled
    | ClearSelection
    | SwitchTab Tab
    | ChatInputChanged String
    | SubmitPrompt
    | OpenDropdown String
    | CloseDropdown
    | DropdownQueryChanged String
    | DropdownActiveChanged (Maybe Int)
    | TokenChosen String String
    | ApplyChanges
    | OpenWorkHistory
    | OpenChangeDetails String
    | Back
    | Tick Float



-- INIT


{-| -}
init : Model
init =
    { collapsed = False
    , mode = Default
    , tab = AgentChat
    , selected = Nothing
    , chatInput = ""
    , tokenDrafts = Dict.empty
    , openDropdown = Nothing
    , dropdownQuery = ""
    , dropdownActive = Nothing
    , work = Dict.empty
    , nextId = 1
    }



-- UPDATE


{-| Pure update — no `Cmd`. The mock task lifecycle is driven by `Tick` messages
from `subscriptions`; real work status will later be driven by backend events.
-}
update : Context -> Msg -> Model -> Model
update context msg model =
    case msg of
        ToggleCollapsed ->
            { model | collapsed = not model.collapsed }

        InfoClicked ->
            -- No-op placeholder (info affordance); real behaviour TBD.
            model

        StartSelecting ->
            -- Enter selection mode; the preview's `<cp-ai-selection>` element
            -- takes over hover/click and emits `SelectionCaptured`.
            { model | mode = Selecting, selected = Nothing }

        SelectionCaptured element ->
            applySelection element model

        SelectionCancelled ->
            -- Esc / dismiss selection mode: keep any prior selection, otherwise
            -- fall back to the default state.
            case model.selected of
                Just _ ->
                    { model | mode = Selected }

                Nothing ->
                    { model | mode = Default }

        ClearSelection ->
            -- Close card → back to Default, clear selection.
            { model
                | selected = Nothing
                , mode = Default
                , tab = AgentChat
                , chatInput = ""
                , tokenDrafts = Dict.empty
                , openDropdown = Nothing
            }

        SwitchTab tab ->
            { model | tab = tab, openDropdown = Nothing }

        ChatInputChanged value ->
            { model | chatInput = value }

        SubmitPrompt ->
            let
                instruction =
                    String.trim model.chatInput
            in
            if instruction == "" then
                model

            else
                model
                    |> startWork context
                        { title = instruction
                        , summary = "Agent request: " ++ instruction
                        , tokensUpdated = []
                        }
                    |> (\m -> { m | chatInput = "" })

        OpenDropdown rowKey ->
            { model | openDropdown = Just rowKey, dropdownQuery = "", dropdownActive = Nothing }

        CloseDropdown ->
            { model | openDropdown = Nothing, dropdownQuery = "", dropdownActive = Nothing }

        DropdownQueryChanged query ->
            { model | dropdownQuery = query }

        DropdownActiveChanged active ->
            { model | dropdownActive = active }

        TokenChosen rowKey value ->
            { model
                | tokenDrafts = Dict.insert rowKey value model.tokenDrafts
                , openDropdown = Nothing
                , dropdownQuery = ""
                , dropdownActive = Nothing
            }

        ApplyChanges ->
            let
                changes =
                    tokenChanges model
            in
            if List.isEmpty changes then
                model

            else
                let
                    elementLabel =
                        model.selected |> Maybe.map .label |> Maybe.withDefault "element"
                in
                model
                    |> startWork context
                        { title = "Update tokens on " ++ elementLabel
                        , summary = "Apply " ++ String.fromInt (List.length changes) ++ " token change(s)."
                        , tokensUpdated = changes
                        }
                    |> (\m -> { m | tokenDrafts = Dict.empty })

        OpenWorkHistory ->
            { model | mode = WorkHistory, openDropdown = Nothing }

        OpenChangeDetails itemId ->
            { model | mode = ChangeDetails itemId }

        Back ->
            -- ChangeDetails → WorkHistory; WorkHistory → Selected (with the
            -- remembered tab, which we never cleared). If somehow there is no
            -- selection to return to, fall back to Default.
            case model.mode of
                ChangeDetails _ ->
                    { model | mode = WorkHistory }

                _ ->
                    case model.selected of
                        Just _ ->
                            { model | mode = Selected }

                        Nothing ->
                            { model | mode = Default }

        Tick delta ->
            { model | work = Dict.map (\_ items -> List.map (advanceItem delta) items) model.work }



-- SUBSCRIPTIONS


{-| A clock that only runs while work is active, driving the mock lifecycle.
Uses the animation-frame delta (elm/browser, already a dependency) rather than
adding elm/time; `Tick` accumulates `elapsedMs` and only advances a phase once a
threshold is crossed. The shell wires this into its own subscriptions (and
`element` does too).
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    if hasActiveWork model then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none



-- QUERIES (used by the shell)


{-| True while selection mode is active — the shell wraps the preview in the
selection custom element only then.
-}
isSelecting : Model -> Bool
isSelecting model =
    model.mode == Selecting


{-| True while any work item anywhere is queued or in progress. Used to drive
the lifecycle clock (which must keep ticking for work on components the user is
no longer viewing) and the side-nav spinner.
-}
hasActiveWork : Model -> Bool
hasActiveWork model =
    List.any isRunning (allItems model)


{-| Reset the ephemeral, per-component view state when the user navigates to a
different component: the selected element belongs to the previous component's
preview, so clear it and return to the default state. Work/history persist (a
running task keeps going and still shows on its own component).
-}
resetForNavigation : Model -> Model
resetForNavigation model =
    { model
        | mode = Default
        , tab = AgentChat
        , selected = Nothing
        , chatInput = ""
        , tokenDrafts = Dict.empty
        , openDropdown = Nothing
        , dropdownQuery = ""
        , dropdownActive = Nothing
    }


{-| The stable selector of the currently selected element, if any. The shell
passes this to the preview's `<cp-ai-selection>` element so it can keep a
persistent outline on the selected element (cleared when this is `Nothing`).
-}
selectedSelector : Model -> Maybe String
selectedSelector model =
    model.selected |> Maybe.andThen .selector


{-| Component ids that currently have active work — the shell shows a side-nav
spinner next to each.
-}
activeWorkComponentIds : Model -> List String
activeWorkComponentIds model =
    Dict.toList model.work
        |> List.filter (\( _, items ) -> List.any isRunning items)
        |> List.map Tuple.first


{-| Work items for the current component. Items are stored newest-first (new
work is prepended, and a finished item is re-prepended), so this returns the
stored list directly. Component-scoped: only the current component's items.
-}
historyFor : Context -> Model -> List AiWorkItem
historyFor context model =
    Dict.get (componentKey context) model.work
        |> Maybe.withDefault []



-- DECODER (cp-select CustomEvent detail → SelectedElement)


{-| Decodes the `detail` payload of the `cp-select` event emitted by the
`<cp-ai-selection>` custom element into a `SelectedElement`. Tolerant: every
field beyond `id`/`tagName` has a sensible default so a partial payload still
decodes.

⚠️ The `andMap` calls below are positional — their order must match
`SelectedElement`'s field declaration order exactly. Several adjacent fields
share a type (`String`, `Maybe String`), so a reordering would type-check but
silently mis-assign values. Keep the two in lock-step.

-}
selectedDecoder : Decode.Decoder SelectedElement
selectedDecoder =
    let
        str key default =
            optionalField key Decode.string default

        maybeStr key =
            optionalField key (Decode.map Just Decode.string) Nothing
    in
    Decode.succeed SelectedElement
        |> andMap (str "id" "")
        |> andMap (str "label" "Element")
        |> andMap (maybeStr "subtitle")
        |> andMap (str "elementType" "Element")
        |> andMap (str "tagName" "div")
        |> andMap (maybeStr "role")
        |> andMap (maybeStr "textContent")
        |> andMap (str "componentName" "")
        |> andMap (maybeStr "componentId")
        |> andMap (str "route" "")
        |> andMap (maybeStr "sourceFile")
        |> andMap (maybeStr "sourceSymbol")
        |> andMap (maybeStr "selector")
        |> andMap (optionalField "classNames" (Decode.list Decode.string) [])
        |> andMap (optionalField "dataAttributes" pairsDecoder [])
        |> andMap (optionalField "tokens" tokensDecoder [])
        |> andMap (optionalField "computedStyles" pairsDecoder [])
        |> andMap (optionalField "bounds" (Decode.map Just boundsDecoder) Nothing)


{-| Applicative apply, so a long record can be built field-by-field.
-}
andMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
andMap =
    Decode.map2 (|>)


{-| A field that falls back to `default` when absent or `null`.
-}
optionalField : String -> Decode.Decoder a -> a -> Decode.Decoder a
optionalField key decoder default =
    Decode.oneOf
        [ Decode.field key decoder
        , Decode.succeed default
        ]


{-| A `{ "k": "v", … }` object as an association list.
-}
pairsDecoder : Decode.Decoder (List ( String, String ))
pairsDecoder =
    Decode.keyValuePairs Decode.string


boundsDecoder : Decode.Decoder Bounds
boundsDecoder =
    Decode.map4 Bounds
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "width" Decode.float)
        (Decode.field "height" Decode.float)


{-| Tokens list, dropping any entry whose category is not recognised.
-}
tokensDecoder : Decode.Decoder (List AppliedToken)
tokensDecoder =
    Decode.list (Decode.maybe tokenDecoder)
        |> Decode.map (List.filterMap identity)


tokenDecoder : Decode.Decoder AppliedToken
tokenDecoder =
    Decode.succeed AppliedToken
        |> andMap (Decode.field "category" tokenCategoryDecoder)
        |> andMap (optionalField "label" Decode.string "Token")
        |> andMap (optionalField "value" Decode.string "")
        |> andMap (optionalField "cssProperty" (Decode.map Just Decode.string) Nothing)
        |> andMap (optionalField "cssVariable" (Decode.map Just Decode.string) Nothing)
        |> andMap (optionalField "source" (Decode.map tokenSourceFromKey Decode.string) Nothing)


tokenCategoryDecoder : Decode.Decoder TokenCategory
tokenCategoryDecoder =
    Decode.string
        |> Decode.andThen
            (\key ->
                case categoryFromKey key of
                    Just category ->
                        Decode.succeed category

                    Nothing ->
                        Decode.fail ("Unknown token category: " ++ key)
            )


categoryFromKey : String -> Maybe TokenCategory
categoryFromKey key =
    case key of
        "typography" ->
            Just Typography

        "text-colour" ->
            Just TextColour

        "background-colour" ->
            Just BackgroundColour

        "font-family" ->
            Just FontFamily

        "line-height" ->
            Just LineHeight

        "letter-spacing" ->
            Just LetterSpacing

        "spacing" ->
            Just Spacing

        "radius" ->
            Just Radius

        "elevation" ->
            Just Elevation

        "border" ->
            Just Border

        "motion" ->
            Just Motion

        _ ->
            Nothing


tokenSourceFromKey : String -> Maybe TokenSource
tokenSourceFromKey key =
    case key of
        "class" ->
            Just FromClass

        "css-variable" ->
            Just FromCssVariable

        "computed" ->
            Just FromComputed

        "prop" ->
            Just FromProp

        _ ->
            Nothing



-- VIEW


{-| The sticky AI Inspector region. Rendered as the Inspector panel's last child
(flex-shrink 0) so it pins to the bottom while the panel body scrolls above it.
Dispatches on `model.mode`:

  - `Default` / `Selecting` — header (sparkle / AI INSPECTOR / info / chevron)
    and the Inspect button + helper.
  - `Selected` — tabbed (Agent chat / Token editor) with the selected-element
    card; title bar gains start-new-selection + close, and a spinner (work
    active) or history icon (history exists), the two mutually exclusive.
    Inspector-level actions (start-new-selection, close) live in the title bar.
  - `WorkHistory` — back + "AI Inspector — Work history"; CURRENTLY WORKING ON
    then RECENT HISTORY (newest-first).
  - `ChangeDetails id` — back + "Change details"; completed/in-progress detail.

-}
view : Theme -> Context -> Model -> Html Msg
view theme context model =
    Html.div
        [ Html.Attributes.class "cp-ai-inspector"
        , Ui.style "font-family" theme.fontFamily
        , Ui.style "background" theme.surface
        , Ui.style "color" theme.ink
        , Ui.style "padding" "14px 18px 18px"
        ]
        (case model.mode of
            Default ->
                viewDefault theme context model

            Selecting ->
                viewDefault theme context model

            Selected ->
                viewSelected theme context model

            WorkHistory ->
                viewWorkHistory theme context model

            ChangeDetails itemId ->
                viewChangeDetails theme model itemId
        )



-- VIEW: DEFAULT STATE


viewDefault : Theme -> Context -> Model -> List (Html Msg)
viewDefault theme context model =
    let
        collapseIcon =
            if model.collapsed then
                "chevron-down"

            else
                "chevron-up"
    in
    titleBar
        { left =
            [ brandGlyph theme
            , titleText theme "AI INSPECTOR"
            , iconButton theme
                { name = "circle-info"
                , label = "About the AI Inspector"
                , muted = True
                , onPress = InfoClicked
                }
            ]
        , right =
            -- The work indicator (spinner while active, else history icon when
            -- this component has history) appears here too, so a component with
            -- history/active work is reachable from the default state. Empty for
            -- a fresh component, matching the mockup's default.
            titleIndicator theme context model
                ++ [ iconButton theme
                        { name = collapseIcon
                        , label =
                            if model.collapsed then
                                "Expand AI Inspector"

                            else
                                "Collapse AI Inspector"
                        , muted = True
                        , onPress = ToggleCollapsed
                        }
                   ]
        }
        :: (if model.collapsed then
                []

            else
                [ Html.div [ Ui.style "margin-top" "14px" ]
                    [ primaryButton theme
                        { label = "Inspect"
                        , icon = "square-dashed-circle-plus"
                        , onPress = StartSelecting
                        }
                    , helperText theme "Select an element in the preview to inspect and edit with AI."
                    ]
                ]
           )



-- VIEW: SELECTED STATE


viewSelected : Theme -> Context -> Model -> List (Html Msg)
viewSelected theme context model =
    case model.selected of
        Nothing ->
            viewDefault theme context model

        Just element ->
            [ titleBar
                { left = [ brandGlyph theme, titleText theme "AI INSPECTOR" ]
                , right =
                    titleIndicator theme context model
                        ++ [ iconButton theme
                                { name = "square-dashed-circle-plus"
                                , label = "Start new selection"
                                , muted = True
                                , onPress = StartSelecting
                                }
                           , iconButton theme
                                { name = "xmark"
                                , label = "Close inspector"
                                , muted = True
                                , onPress = ClearSelection
                                }
                           ]
                }
            , tabsRow theme model.tab
            , Html.div [ Ui.style "margin-top" "14px" ]
                (sectionLabel theme "SELECTED ELEMENT"
                    :: selectedCard theme element
                    :: tabContent theme model element
                )
            ]


{-| The leading title-bar indicator, per the mutually-exclusive rule:

  - active work anywhere → a spinner (always wins), opening Work history;
  - else history exists for this component → a history icon, opening Work history;
  - else nothing.

-}
titleIndicator : Theme -> Context -> Model -> List (Html Msg)
titleIndicator theme context model =
    if List.any isRunning (historyFor context model) then
        [ Html.button
            [ Html.Attributes.type_ "button"
            , Ui.onClick OpenWorkHistory
            , Html.Attributes.attribute "aria-label" "View work in progress"
            , Html.Attributes.title "Working…"
            , Ui.style "display" "inline-flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "center"
            , Ui.style "width" "24px"
            , Ui.style "height" "24px"
            , Ui.style "background" "transparent"
            , Ui.style "border" "none"
            , Ui.style "padding" "0"
            , Ui.style "cursor" "pointer"
            , Ui.style "font-size" "14px"
            , Ui.style "color" theme.brandBlue
            ]
            [ spinnerIcon ]
        ]

    else if List.isEmpty (historyFor context model) then
        []

    else
        [ iconButton theme
            { name = "clock-rotate-left"
            , label = "View work history"
            , muted = True
            , onPress = OpenWorkHistory
            }
        ]


{-| The Agent chat / Token editor tab strip with the active blue underline.
-}
tabsRow : Theme -> Tab -> Html Msg
tabsRow theme active =
    Html.div
        [ Html.Attributes.attribute "role" "tablist"
        , Ui.style "display" "flex"
        , Ui.style "gap" "20px"
        , Ui.style "margin-top" "14px"
        , Ui.style "border-bottom" ("1px solid " ++ theme.line)
        ]
        [ tabButton theme active AgentChat "Agent chat"
        , tabButton theme active TokenEditor "Token editor"
        ]


tabButton : Theme -> Tab -> Tab -> String -> Html Msg
tabButton theme active tab label =
    let
        isActive =
            active == tab
    in
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.attribute "role" "tab"
        , Html.Attributes.attribute "aria-selected"
            (if isActive then
                "true"

             else
                "false"
            )
        , Ui.onClick (SwitchTab tab)
        , Ui.style "background" "transparent"
        , Ui.style "border" "none"
        , Ui.style "padding" "8px 2px"
        , Ui.style "margin-bottom" "-1px"
        , Ui.style "cursor" "pointer"
        , Ui.style "font-family" "inherit"
        , Ui.style "font-size" "13px"
        , Ui.style "font-weight" "600"
        , Ui.style "color"
            (if isActive then
                theme.brandBlue

             else
                theme.ink3
            )
        , Ui.style "border-bottom"
            ("2px solid "
                ++ (if isActive then
                        theme.brandBlue

                    else
                        "transparent"
                   )
            )
        ]
        [ Html.text label ]


{-| The selected-element card: a category icon box, the human-readable title and
subtitle, and a close affordance that clears the selection.
-}
selectedCard : Theme -> SelectedElement -> Html Msg
selectedCard theme element =
    Html.div
        [ Html.Attributes.attribute "role" "group"
        , Html.Attributes.attribute "aria-label" ("Selected element: " ++ element.label)
        , Html.Attributes.tabindex -1
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" "10px"
        , Ui.style "margin-top" "10px"
        , Ui.style "padding" "10px"
        , Ui.style "border" ("1px solid " ++ theme.line)
        , Ui.style "border-radius" theme.radiusMd
        , Ui.style "background" theme.surface
        ]
        [ Html.div
            [ Ui.style "flex-shrink" "0"
            , Ui.style "width" "34px"
            , Ui.style "height" "34px"
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "center"
            , Ui.style "border" ("1px solid " ++ theme.line)
            , Ui.style "border-radius" theme.radiusSm
            , Ui.style "color" theme.ink2
            , Ui.style "font-size" "15px"
            ]
            [ faIcon (elementGlyph element) ]
        , Html.div [ Ui.style "flex" "1", Ui.style "min-width" "0" ]
            [ Html.div
                [ Ui.style "font-size" "13px"
                , Ui.style "font-weight" "600"
                , Ui.style "color" theme.ink
                ]
                [ Html.text element.label ]
            , Html.div
                [ Ui.style "font-size" "12px"
                , Ui.style "color" theme.ink3
                , Ui.style "white-space" "nowrap"
                , Ui.style "overflow" "hidden"
                , Ui.style "text-overflow" "ellipsis"
                ]
                [ Html.text (Maybe.withDefault element.elementType element.subtitle) ]
            ]
        , iconButton theme
            { name = "xmark"
            , label = "Clear selected element"
            , muted = True
            , onPress = ClearSelection
            }
        ]


{-| A FontAwesome glyph representing the element's kind, chosen from its
human-readable `elementType` (falling back to a neutral glyph).
-}
elementGlyph : SelectedElement -> String
elementGlyph element =
    case String.toLower element.elementType of
        "heading" ->
            "heading"

        "button" ->
            "rectangle-list"

        "input" ->
            "input-text"

        "card title" ->
            "heading"

        _ ->
            "font"


{-| The tab-specific body below the selected-element card.
-}
tabContent : Theme -> Model -> SelectedElement -> List (Html Msg)
tabContent theme model element =
    let
        panel label body =
            Html.div
                [ Html.Attributes.attribute "role" "tabpanel"
                , Html.Attributes.attribute "aria-label" label
                ]
                [ body ]
    in
    case model.tab of
        AgentChat ->
            [ panel "Agent chat" (agentChatBody theme model) ]

        TokenEditor ->
            [ panel "Token editor" (tokenEditorBody theme model element) ]


{-| Agent chat: just a plain prompt input. No VSCode-derived chrome. (Prompt
submission → work item lands in slice 8.)
-}
agentChatBody : Theme -> Model -> Html Msg
agentChatBody theme model =
    Html.div [ Ui.style "margin-top" "12px" ]
        [ Html.textarea
            [ Html.Attributes.attribute "aria-label" "Describe what to build or change"
            , Html.Attributes.placeholder "Describe what to build or change..."
            , Html.Attributes.value model.chatInput
            , Html.Events.onInput ChatInputChanged
            , Ui.style "width" "100%"
            , Ui.style "box-sizing" "border-box"
            , Ui.style "min-height" "96px"
            , Ui.style "resize" "vertical"
            , Ui.style "padding" "12px"
            , Ui.style "border" ("1px solid " ++ theme.line)
            , Ui.style "border-radius" theme.radiusMd
            , Ui.style "background" theme.surfaceAlt
            , Ui.style "font-family" "inherit"
            , Ui.style "font-size" "13px"
            , Ui.style "line-height" "1.45"
            , Ui.style "color" theme.ink
            ]
            []
        ]


{-| Token editor: the applied-token rows + Apply changes. Slice 2 renders each
row's current value in a static control shell; slice 7 makes the control a
searchable, category-filtered dropdown.
-}
tokenEditorBody : Theme -> Model -> SelectedElement -> Html Msg
tokenEditorBody theme model element =
    Html.div [ Ui.style "margin-top" "14px" ]
        [ sectionLabel theme "TOKENS APPLIED"
        , Html.div [] (List.map (tokenRow theme model) element.tokens)
        , Html.div [ Ui.style "margin-top" "14px" ]
            [ primaryButton theme
                { label = "Apply changes"
                , icon = "check"
                , onPress = ApplyChanges
                }
            ]
        ]


tokenRow : Theme -> Model -> AppliedToken -> Html Msg
tokenRow theme model token =
    let
        rowKey =
            tokenRowKey token

        currentValue =
            Dict.get rowKey model.tokenDrafts |> Maybe.withDefault token.value

        isOpen =
            model.openDropdown == Just rowKey
    in
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "gap" "12px"
        , Ui.style "padding" "6px 0"
        ]
        [ Html.label
            [ Html.Attributes.for ("cp-ai-token-" ++ rowKey)
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "gap" "8px"
            , Ui.style "min-width" "0"
            ]
            [ tokenCategoryBadge theme
            , Html.span [ Ui.style "font-size" "13px", Ui.style "color" theme.ink2 ] [ Html.text token.label ]
            ]
        , tokenDropdown theme model rowKey token.category currentValue isOpen
        ]


{-| A bespoke, searchable, category-filtered dropdown. The trigger shows the
current value; when open, a menu (opening upward, since the dock sits at the
bottom) offers a search box and only this category's tokens.
-}
tokenDropdown : Theme -> Model -> String -> TokenCategory -> String -> Bool -> Html Msg
tokenDropdown theme model rowKey category currentValue isOpen =
    Html.div [ Ui.style "position" "relative", Ui.style "min-width" "160px" ]
        (Html.button
            [ Html.Attributes.type_ "button"
            , Html.Attributes.id ("cp-ai-token-" ++ rowKey)
            , Html.Attributes.attribute "aria-haspopup" "listbox"
            , Html.Attributes.attribute "aria-expanded"
                (if isOpen then
                    "true"

                 else
                    "false"
                )
            , Ui.onClick
                (if isOpen then
                    CloseDropdown

                 else
                    OpenDropdown rowKey
                )
            , Ui.style "display" "flex"
            , Ui.style "align-items" "center"
            , Ui.style "justify-content" "space-between"
            , Ui.style "gap" "8px"
            , Ui.style "width" "100%"
            , Ui.style "box-sizing" "border-box"
            , Ui.style "padding" "6px 10px"
            , Ui.style "border"
                ("1px solid "
                    ++ (if isOpen then
                            theme.brandBlue

                        else
                            theme.line
                       )
                )
            , Ui.style "border-radius" theme.radiusSm
            , Ui.style "background" theme.surface
            , Ui.style "font-family" "inherit"
            , Ui.style "font-size" "13px"
            , Ui.style "color" theme.ink
            , Ui.style "cursor" "pointer"
            ]
            [ Html.span [ Ui.style "overflow" "hidden", Ui.style "text-overflow" "ellipsis", Ui.style "white-space" "nowrap" ] [ Html.text currentValue ]
            , Html.span [ Ui.style "color" theme.ink4, Ui.style "font-size" "12px" ] [ faIcon "chevron-down" ]
            ]
            :: (if isOpen then
                    [ tokenMenu theme model rowKey category currentValue ]

                else
                    []
               )
        )


tokenMenu : Theme -> Model -> String -> TokenCategory -> String -> Html Msg
tokenMenu theme model rowKey category currentValue =
    let
        options =
            filterTokens model.dropdownQuery (tokenCatalogue category)
    in
    Html.div
        [ Html.Attributes.attribute "role" "listbox"
        , Ui.style "position" "absolute"
        , Ui.style "bottom" "calc(100% + 4px)"
        , Ui.style "left" "0"
        , Ui.style "right" "0"
        , Ui.style "z-index" "20"
        , Ui.style "background" theme.surface
        , Ui.style "border" ("1px solid " ++ theme.line)
        , Ui.style "border-radius" theme.radiusMd
        , Ui.style "box-shadow" theme.shadow2
        , Ui.style "padding" "6px"
        , Ui.style "max-height" "220px"
        , Ui.style "overflow" "auto"
        ]
        (Html.input
            [ Html.Attributes.type_ "text"
            , Html.Attributes.placeholder "Search…"
            , Html.Attributes.value model.dropdownQuery
            , Html.Attributes.attribute "aria-label" "Search tokens"
            , Html.Attributes.autofocus True
            , Html.Events.onInput DropdownQueryChanged
            , Ui.style "width" "100%"
            , Ui.style "box-sizing" "border-box"
            , Ui.style "padding" "6px 8px"
            , Ui.style "margin-bottom" "6px"
            , Ui.style "border" ("1px solid " ++ theme.line)
            , Ui.style "border-radius" theme.radiusSm
            , Ui.style "font-family" "inherit"
            , Ui.style "font-size" "13px"
            ]
            []
            :: (if List.isEmpty options then
                    [ Html.div [ Ui.style "padding" "8px", Ui.style "font-size" "12px", Ui.style "color" theme.ink4 ] [ Html.text "No matching tokens" ] ]

                else
                    List.map (tokenOption theme rowKey currentValue) options
               )
        )


tokenOption : Theme -> String -> String -> String -> Html Msg
tokenOption theme rowKey currentValue value =
    let
        selected =
            value == currentValue
    in
    Html.button
        [ Html.Attributes.type_ "button"
        , Html.Attributes.attribute "role" "option"
        , Html.Attributes.attribute "aria-selected"
            (if selected then
                "true"

             else
                "false"
            )
        , Ui.onClick (TokenChosen rowKey value)
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "gap" "8px"
        , Ui.style "width" "100%"
        , Ui.style "box-sizing" "border-box"
        , Ui.style "text-align" "left"
        , Ui.style "padding" "7px 8px"
        , Ui.style "border" "none"
        , Ui.style "border-radius" theme.radiusSm
        , Ui.style "cursor" "pointer"
        , Ui.style "font-family" "inherit"
        , Ui.style "font-size" "13px"
        , Ui.style "color" theme.ink
        , Ui.style "background"
            (if selected then
                theme.brandBlue50

             else
                "transparent"
            )
        ]
        [ Html.span [] [ Html.text value ]
        , if selected then
            Html.span [ Ui.style "color" theme.brandBlue, Ui.style "font-size" "12px" ] [ faIcon "check" ]

          else
            Html.text ""
        ]


{-| Case-insensitive substring filter over a category's tokens.
-}
filterTokens : String -> List String -> List String
filterTokens query options =
    let
        q =
            String.trim (String.toLower query)
    in
    if q == "" then
        options

    else
        List.filter (\value -> String.contains q (String.toLower value)) options


{-| The design-system token map: which tokens each category offers. A dropdown
only ever lists its own category's tokens.
-}
tokenCatalogue : TokenCategory -> List String
tokenCatalogue category =
    case category of
        Typography ->
            [ "text-display-lg", "text-display-md", "text-display-sm", "text-heading-1", "text-heading-2", "text-heading-3", "text-heading-4", "text-ui-heading-2", "text-ui-heading-3", "text-body-lg", "text-body-md", "text-body-sm", "text-label-regular", "text-label-small" ]

        TextColour ->
            [ "text-ink-1", "text-ink-2", "text-ink-3", "text-ink-4", "text-ink-5", "text-primary", "text-brand", "text-success", "text-danger", "text-inverse" ]

        BackgroundColour ->
            [ "bg-surface", "bg-surface-alt", "bg-sunken", "bg-primary", "bg-brand", "bg-success", "bg-danger" ]

        FontFamily ->
            [ "font-sans", "font-display", "font-mono" ]

        LineHeight ->
            [ "leading-tight", "leading-snug", "leading-normal", "leading-relaxed", "leading-loose" ]

        LetterSpacing ->
            [ "tracking-tight", "tracking-normal", "tracking-wide" ]

        Spacing ->
            [ "space-0", "space-1", "space-2", "space-3", "space-4", "space-5", "space-6", "space-8", "space-10", "space-12", "space-16" ]

        Radius ->
            [ "radius-xs", "radius-sm", "radius-md", "radius-lg", "radius-xl", "radius-2xl", "radius-pill" ]

        Elevation ->
            [ "shadow-0", "shadow-1", "shadow-2", "shadow-3", "shadow-4", "shadow-modal" ]

        Border ->
            [ "border-subtle", "border-strong", "border-focus", "border-none" ]

        Motion ->
            [ "motion-fast", "motion-standard", "motion-slow" ]


{-| The purple square that marks a token category row.
-}
tokenCategoryBadge : Theme -> Html Msg
tokenCategoryBadge theme =
    Html.div
        [ Ui.style "flex-shrink" "0"
        , Ui.style "width" "22px"
        , Ui.style "height" "22px"
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "center"
        , Ui.style "border-radius" theme.radiusSm
        , Ui.style "background" theme.tokenIcon
        , Ui.style "color" "#ffffff"
        , Ui.style "font-size" "10px"
        ]
        [ faIcon "brackets-curly" ]


{-| A stable per-row key for token drafts / open-dropdown scoping.
-}
tokenRowKey : AppliedToken -> String
tokenRowKey token =
    categoryKey token.category


categoryKey : TokenCategory -> String
categoryKey category =
    case category of
        Typography ->
            "typography"

        TextColour ->
            "text-colour"

        BackgroundColour ->
            "background-colour"

        FontFamily ->
            "font-family"

        LineHeight ->
            "line-height"

        LetterSpacing ->
            "letter-spacing"

        Spacing ->
            "spacing"

        Radius ->
            "radius"

        Elevation ->
            "elevation"

        Border ->
            "border"

        Motion ->
            "motion"



-- VIEW: WORK HISTORY & CHANGE DETAILS


viewWorkHistory : Theme -> Context -> Model -> List (Html Msg)
viewWorkHistory theme context model =
    let
        items =
            historyFor context model

        ( working, finished ) =
            List.partition isRunning items
    in
    [ backHeader theme "AI Inspector — Work history"
    , Html.div [ Ui.style "margin-top" "14px" ]
        (List.concat
            [ if List.isEmpty working then
                []

              else
                [ sectionLabel theme "CURRENTLY WORKING ON"
                , Html.div [ Ui.style "margin-bottom" "16px" ] (List.map (workRow theme) working)
                ]
            , [ sectionLabel theme "RECENT HISTORY"
              , if List.isEmpty finished then
                    emptyNote theme "No history yet for this component."

                else
                    Html.div [] (List.map (workRow theme) finished)
              ]
            ]
        )
    ]


viewChangeDetails : Theme -> Model -> String -> List (Html Msg)
viewChangeDetails theme model itemId =
    case lookupItem itemId model of
        Nothing ->
            [ backHeader theme "Change details"
            , emptyNote theme "This change is no longer available."
            ]

        Just item ->
            let
                meta =
                    statusMeta theme item.status

                timeLabel =
                    case item.status of
                        Completed ->
                            Maybe.withDefault item.createdAt item.completedAt

                        _ ->
                            item.createdAt
            in
            [ backHeader theme "Change details"
            , Html.div [ Ui.style "display" "flex", Ui.style "gap" "10px", Ui.style "align-items" "flex-start", Ui.style "margin-top" "14px" ]
                [ statusIconEl meta
                , Html.div []
                    [ Html.div [ Ui.style "font-size" "14px", Ui.style "font-weight" "600", Ui.style "color" theme.ink ] [ Html.text item.title ]
                    , Html.div [ Ui.style "font-size" "12px", Ui.style "color" theme.ink3, Ui.style "margin-top" "2px" ]
                        [ Html.text (meta.label ++ "  •  " ++ timeLabel) ]
                    ]
                ]
            , maybeSection theme "SUMMARY" (item.summary |> Maybe.map (\s -> [ paragraph theme s ]))
            , if isRunning item then
                maybeSection theme
                    "ACTIVITY"
                    (nonEmpty (List.map (activityRow theme) item.activity))

              else
                maybeSection theme
                    "FILES CHANGED"
                    (nonEmpty (List.map (fileRow theme) item.filesChanged))
            , if isRunning item then
                Html.text ""

              else
                maybeSection theme
                    "TOKENS UPDATED"
                    (nonEmpty (List.map (tokenChangeRow theme) item.tokensUpdated))
            , if isRunning item then
                Html.text ""

              else
                maybeSection theme
                    "ELEMENTS AFFECTED"
                    (item.elementsAffected |> Maybe.map (\n -> [ elementsAffectedRow theme n ]))
            ]


{-| A back button + page title. Marked as the panel heading for focus handling.
-}
backHeader : Theme -> String -> Html Msg
backHeader theme title =
    Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "8px" ]
        [ iconButton theme
            { name = "chevron-left"
            , label = "Back"
            , muted = False
            , onPress = Back
            }
        , Html.h2
            [ Html.Attributes.tabindex -1
            , Ui.style "margin" "0"
            , Ui.style "font-size" "14px"
            , Ui.style "font-weight" "600"
            , Ui.style "color" theme.ink
            ]
            [ Html.text title ]
        ]


{-| A history row: status icon, title + meta, status label, chevron. In-progress
rows get a subtle blue wash. The whole row opens Change details.
-}
workRow : Theme -> AiWorkItem -> Html Msg
workRow theme item =
    let
        meta =
            statusMeta theme item.status

        running =
            isRunning item
    in
    Html.button
        [ Html.Attributes.type_ "button"
        , Ui.onClick (OpenChangeDetails item.id)
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" "10px"
        , Ui.style "width" "100%"
        , Ui.style "box-sizing" "border-box"
        , Ui.style "text-align" "left"
        , Ui.style "font-family" "inherit"
        , Ui.style "padding" "10px"
        , Ui.style "border" "none"
        , Ui.style "border-radius" theme.radiusSm
        , Ui.style "cursor" "pointer"
        , Ui.style "background"
            (if running then
                theme.brandBlue50

             else
                "transparent"
            )
        ]
        [ statusIconEl meta
        , Html.div [ Ui.style "flex" "1", Ui.style "min-width" "0" ]
            [ Html.div
                [ Ui.style "font-size" "13px"
                , Ui.style "font-weight" "500"
                , Ui.style "color"
                    (if running then
                        theme.brandBlue

                     else
                        theme.ink
                    )
                , Ui.style "white-space" "nowrap"
                , Ui.style "overflow" "hidden"
                , Ui.style "text-overflow" "ellipsis"
                ]
                [ Html.text item.title ]
            , Html.div [ Ui.style "font-size" "12px", Ui.style "color" theme.ink4, Ui.style "margin-top" "2px" ]
                [ Html.text (item.createdAt ++ "  •  " ++ item.componentName) ]
            ]
        , Html.span [ Ui.style "font-size" "12px", Ui.style "font-weight" "600", Ui.style "color" meta.color, Ui.style "white-space" "nowrap" ]
            [ Html.text meta.label ]
        , Html.span [ Ui.style "color" theme.ink4, Ui.style "font-size" "12px" ] [ faIcon "chevron-right" ]
        ]


type alias StatusMeta =
    { icon : String, color : String, spin : Bool, label : String }


statusMeta : Theme -> WorkStatus -> StatusMeta
statusMeta theme status =
    case status of
        Queued ->
            { icon = "spinner", color = theme.brandBlue, spin = True, label = "In progress" }

        InProgress ->
            { icon = "spinner", color = theme.brandBlue, spin = True, label = "In progress" }

        Completed ->
            { icon = "circle-check", color = theme.success, spin = False, label = "Completed" }

        Failed ->
            { icon = "circle-xmark", color = theme.danger, spin = False, label = "Failed" }


{-| The circular status glyph. Includes visually-hidden text via aria-label for
screen readers (fleshed out in the accessibility pass).
-}
statusIconEl : StatusMeta -> Html Msg
statusIconEl meta =
    Html.span
        [ Html.Attributes.attribute "role" "img"
        , Html.Attributes.attribute "aria-label" meta.label
        , Ui.style "flex-shrink" "0"
        , Ui.style "color" meta.color
        , Ui.style "font-size" "16px"
        , Ui.style "line-height" "1"
        ]
        [ if meta.spin then
            spinnerIcon

          else
            faIcon meta.icon
        ]


activityRow : Theme -> AiWorkActivity -> Html Msg
activityRow theme activity =
    let
        meta =
            activityMeta theme activity.status
    in
    Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "8px", Ui.style "padding" "5px 0" ]
        [ Html.span [ Ui.style "color" meta.color, Ui.style "font-size" "14px" ]
            [ if meta.spin then
                spinnerIcon

              else
                faIcon meta.icon
            ]
        , Html.span [ Ui.style "font-size" "13px", Ui.style "color" theme.ink2 ] [ Html.text activity.label ]
        ]


activityMeta : Theme -> ActivityStatus -> StatusMeta
activityMeta theme status =
    case status of
        ActCompleted ->
            { icon = "circle-check", color = theme.success, spin = False, label = "Completed" }

        ActInProgress ->
            { icon = "spinner", color = theme.brandBlue, spin = True, label = "In progress" }

        ActPending ->
            { icon = "circle", color = theme.ink4, spin = False, label = "Pending" }

        ActFailed ->
            { icon = "circle-xmark", color = theme.danger, spin = False, label = "Failed" }


fileRow : Theme -> AiChangedFile -> Html Msg
fileRow theme file =
    Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "8px", Ui.style "padding" "6px 0" ]
        [ Html.span [ Ui.style "color" theme.ink3, Ui.style "font-size" "13px" ] [ faIcon "file-lines" ]
        , Html.span [ Ui.style "flex" "1", Ui.style "font-size" "13px", Ui.style "color" theme.ink ] [ Html.text file.path ]
        , case file.diffUrl of
            Just _ ->
                Html.span [ Ui.style "font-size" "12px", Ui.style "font-weight" "600", Ui.style "color" theme.brandBlue ] [ Html.text "View diff" ]

            Nothing ->
                Html.text ""
        ]


tokenChangeRow : Theme -> AiTokenChange -> Html Msg
tokenChangeRow theme change =
    Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "8px", Ui.style "padding" "6px 0" ]
        [ tokenCategoryBadge theme
        , Html.span [ Ui.style "flex" "1", Ui.style "font-size" "13px", Ui.style "color" theme.ink2 ] [ Html.text change.category ]
        , Html.span [ Ui.style "font-size" "13px", Ui.style "font-family" "monospace", Ui.style "color" theme.ink ] [ Html.text change.to ]
        ]


elementsAffectedRow : Theme -> Int -> Html Msg
elementsAffectedRow theme n =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        , Ui.style "padding" "10px"
        , Ui.style "border" ("1px solid " ++ theme.line)
        , Ui.style "border-radius" theme.radiusSm
        ]
        [ Html.span [ Ui.style "font-size" "13px", Ui.style "color" theme.ink ]
            [ Html.text
                (String.fromInt n
                    ++ (if n == 1 then
                            " element"

                        else
                            " elements"
                       )
                )
            ]
        , Html.span [ Ui.style "color" theme.ink4, Ui.style "font-size" "12px" ] [ faIcon "chevron-right" ]
        ]


{-| A labelled detail section, rendered only when it has content.
-}
maybeSection : Theme -> String -> Maybe (List (Html Msg)) -> Html Msg
maybeSection theme label content =
    case content of
        Just body ->
            Html.div [ Ui.style "margin-top" "16px" ] (sectionLabel theme label :: body)

        Nothing ->
            Html.text ""


paragraph : Theme -> String -> Html Msg
paragraph theme s =
    Html.p [ Ui.style "margin" "0", Ui.style "font-size" "13px", Ui.style "line-height" "1.5", Ui.style "color" theme.ink2 ] [ Html.text s ]


emptyNote : Theme -> String -> Html Msg
emptyNote theme s =
    Html.p [ Ui.style "margin" "0", Ui.style "font-size" "13px", Ui.style "color" theme.ink4 ] [ Html.text s ]


{-| `Just` the list when non-empty, else `Nothing` — for `maybeSection`.
-}
nonEmpty : List a -> Maybe (List a)
nonEmpty xs =
    if List.isEmpty xs then
        Nothing

    else
        Just xs



-- VIEW: SHARED CHROME


{-| The header row: a left cluster (brand + title + affordances) and a right
cluster (state icons). Reused by every mode.
-}
titleBar : { left : List (Html Msg), right : List (Html Msg) } -> Html Msg
titleBar { left, right } =
    Html.div
        [ Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "space-between"
        ]
        [ Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "8px" ] left
        , Html.div [ Ui.style "display" "flex", Ui.style "align-items" "center", Ui.style "gap" "6px" ] right
        ]


{-| The sparkle glyph that opens the header, in the brand-blue accent.
-}
brandGlyph : Theme -> Html Msg
brandGlyph theme =
    Html.span
        [ Ui.style "color" theme.brandBlue
        , Ui.style "font-size" "15px"
        , Ui.style "line-height" "1"
        ]
        [ faIcon "sparkles" ]


titleText : Theme -> String -> Html Msg
titleText theme label =
    Html.span
        [ Ui.style "font-size" "12px"
        , Ui.style "font-weight" "700"
        , Ui.style "letter-spacing" "0.04em"
        , Ui.style "color" theme.ink
        ]
        [ Html.text label ]


{-| A grey uppercase section label (e.g. SELECTED ELEMENT, TOKENS APPLIED).
-}
sectionLabel : Theme -> String -> Html Msg
sectionLabel theme label =
    Html.div
        [ Ui.style "font-size" "11px"
        , Ui.style "font-weight" "600"
        , Ui.style "letter-spacing" "0.06em"
        , Ui.style "color" theme.ink4
        , Ui.style "margin-bottom" "8px"
        ]
        [ Html.text label ]


helperText : Theme -> String -> Html Msg
helperText theme label =
    Html.p
        [ Ui.style "margin" "10px 2px 0"
        , Ui.style "font-size" "13px"
        , Ui.style "line-height" "1.45"
        , Ui.style "color" theme.ink3
        ]
        [ Html.text label ]


{-| Primary blue, full-width call to action with a leading icon.
-}
primaryButton : Theme -> { label : String, icon : String, onPress : Msg } -> Html Msg
primaryButton theme { label, icon, onPress } =
    Html.button
        [ Html.Attributes.type_ "button"
        , Ui.onClick onPress
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "center"
        , Ui.style "gap" "8px"
        , Ui.style "width" "100%"
        , Ui.style "height" "40px"
        , Ui.style "background" theme.brandBlue
        , Ui.style "color" "#ffffff"
        , Ui.style "border" "none"
        , Ui.style "border-radius" theme.radiusMd
        , Ui.style "font-family" "inherit"
        , Ui.style "font-size" "14px"
        , Ui.style "font-weight" "600"
        , Ui.style "cursor" "pointer"
        ]
        [ Html.span [ Ui.style "font-size" "15px", Ui.style "line-height" "1" ] [ faIcon icon ]
        , Html.text label
        ]


{-| A borderless icon button with an accessible label. `muted` renders it in the
tertiary ink so it reads as a secondary affordance.
-}
iconButton : Theme -> { name : String, label : String, muted : Bool, onPress : Msg } -> Html Msg
iconButton theme { name, label, muted, onPress } =
    Html.button
        [ Html.Attributes.type_ "button"
        , Ui.onClick onPress
        , Html.Attributes.attribute "aria-label" label
        , Html.Attributes.title label
        , Ui.style "display" "inline-flex"
        , Ui.style "align-items" "center"
        , Ui.style "justify-content" "center"
        , Ui.style "width" "24px"
        , Ui.style "height" "24px"
        , Ui.style "background" "transparent"
        , Ui.style "border" "none"
        , Ui.style "padding" "0"
        , Ui.style "cursor" "pointer"
        , Ui.style "font-size" "14px"
        , Ui.style "color"
            (if muted then
                theme.ink4

             else
                theme.ink2
            )
        ]
        [ faIcon name ]


{-| A FontAwesome Pro Regular glyph. Plain class markup (no host dependency); the
host page's FA kit renders it. Standalone library examples need a kit loaded.
-}
faIcon : String -> Html msg
faIcon name =
    Html.i
        [ Html.Attributes.class ("fa-regular fa-" ++ name)
        , Html.Attributes.attribute "aria-hidden" "true"
        ]
        []


{-| An animated spinner, using FontAwesome's own `fa-spin`.
-}
spinnerIcon : Html msg
spinnerIcon =
    Html.i
        [ Html.Attributes.class "fa-regular fa-spinner fa-spin"
        , Html.Attributes.attribute "aria-hidden" "true"
        ]
        []



-- INTERNAL HELPERS


{-| Commit a captured element into the Selected state, resetting per-selection
scratch (chat input, token drafts, any open dropdown).
-}
applySelection : SelectedElement -> Model -> Model
applySelection element model =
    { model
        | selected = Just element
        , mode = Selected
        , chatInput = ""
        , tokenDrafts = Dict.empty
        , openDropdown = Nothing
        , dropdownQuery = ""
        , dropdownActive = Nothing
    }


{-| The dict key under which a context's work is stored.
-}
componentKey : Context -> String
componentKey context =
    Maybe.withDefault "" context.componentId


{-| Find a work item by id across all components (details view lookup).
-}
lookupItem : String -> Model -> Maybe AiWorkItem
lookupItem itemId model =
    allItems model |> List.filter (\item -> item.id == itemId) |> List.head


isRunning : AiWorkItem -> Bool
isRunning item =
    item.status == Queued || item.status == InProgress


{-| Create an in-progress work item for the current component and prepend it
(newest-first) to that component's list. The mock lifecycle clock takes it from
here. `selectedElement` is snapshotted from the current selection.
-}
startWork : Context -> { title : String, summary : String, tokensUpdated : List AiTokenChange } -> Model -> Model
startWork context { title, summary, tokensUpdated } model =
    let
        key =
            componentKey context

        item =
            { id = "work-" ++ String.fromInt model.nextId
            , componentId = context.componentId
            , componentName = context.componentName
            , title = title
            , status = InProgress
            , createdAt = "just now"
            , updatedAt = "just now"
            , completedAt = Nothing
            , selectedElement = model.selected
            , summary = Just summary
            , filesChanged = []
            , tokensUpdated = tokensUpdated
            , elementsAffected = Just 1
            , activity = buildActivity 0 InProgress
            , phaseIndex = 0
            , elapsedMs = 0
            }

        existing =
            Dict.get key model.work |> Maybe.withDefault []
    in
    { model
        | work = Dict.insert key (item :: existing) model.work
        , nextId = model.nextId + 1
    }


{-| The token changes implied by the current drafts: each draft that differs
from the selected element's applied value, resolved back to its category label
and original value.
-}
tokenChanges : Model -> List AiTokenChange
tokenChanges model =
    let
        applied =
            model.selected |> Maybe.map .tokens |> Maybe.withDefault []
    in
    applied
        |> List.filterMap
            (\token ->
                case Dict.get (tokenRowKey token) model.tokenDrafts of
                    Just newValue ->
                        if newValue /= token.value then
                            Just (AiTokenChange token.label (Just token.value) newValue)

                        else
                            Nothing

                    Nothing ->
                        Nothing
            )



-- MOCK LIFECYCLE
--
-- A work item walks four phases; the animation-frame `Tick` accumulates elapsed
-- time and advances one phase per `phaseDurationMs`. On the last phase it
-- settles to Completed (or Failed, for a demo hook). Replaced by real backend
-- status events in a later pass.


phaseLabels : List String
phaseLabels =
    [ "Analyzing request", "Planning changes", "Applying changes", "Updating preview" ]


lastPhaseIndex : Int
lastPhaseIndex =
    List.length phaseLabels - 1


phaseDurationMs : Float
phaseDurationMs =
    1200


{-| Advance one item by `delta` ms if it is running; otherwise leave it be.
-}
advanceItem : Float -> AiWorkItem -> AiWorkItem
advanceItem delta item =
    if not (isRunning item) then
        item

    else
        let
            elapsed =
                item.elapsedMs + delta
        in
        if elapsed < phaseDurationMs then
            { item | elapsedMs = elapsed, status = InProgress }

        else if item.phaseIndex < lastPhaseIndex then
            let
                next =
                    item.phaseIndex + 1
            in
            { item
                | phaseIndex = next
                , elapsedMs = 0
                , status = InProgress
                , activity = buildActivity next InProgress
            }

        else
            let
                finalStatus =
                    if isFailureDemo item then
                        Failed

                    else
                        Completed
            in
            { item
                | status = finalStatus
                , elapsedMs = 0
                , completedAt = Just "just now"
                , activity = buildActivity lastPhaseIndex finalStatus
            }


{-| Demo hook: a prompt/title mentioning "fail" ends in the Failed state so the
error UI is reachable without a backend.
-}
isFailureDemo : AiWorkItem -> Bool
isFailureDemo item =
    String.contains "fail" (String.toLower item.title)


{-| The activity timeline for a given phase and status.
-}
buildActivity : Int -> WorkStatus -> List AiWorkActivity
buildActivity phaseIndex status =
    List.indexedMap
        (\i label ->
            let
                st =
                    case status of
                        Completed ->
                            ActCompleted

                        Failed ->
                            if i < phaseIndex then
                                ActCompleted

                            else if i == phaseIndex then
                                ActFailed

                            else
                                ActPending

                        _ ->
                            if i < phaseIndex then
                                ActCompleted

                            else if i == phaseIndex then
                                ActInProgress

                            else
                                ActPending
            in
            AiWorkActivity label st Nothing
        )
        phaseLabels


allItems : Model -> List AiWorkItem
allItems model =
    Dict.values model.work |> List.concat
