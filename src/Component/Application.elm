module Component.Application exposing
    ( Msg, Model, ComponentPlayground
    , ComponentUpdate, Ref, Type
    , element, init, update, view, fromEffect, fromPreviewUpdate, toUrl
    )

{-| Application runner for the Component Playground.


# Types

@docs Msg, Model, ComponentPlayground


# Re-exported Aliases

@docs ComponentUpdate, Ref, Type


# Running the Playground

The playground can be run as a standalone `element`, or wired into a larger
application using `init`, `update`, and `view`.

@docs element, init, update, view, fromEffect, fromPreviewUpdate, toUrl

-}

import Browser
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
import Component.Type
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import State exposing (State)
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query



-- PROCESSED TYPES
-- These are internal to Application; users interact with Frame/Playground.


type ProcessedPlayground e t
    = ProcessedPage { id : String, name : String } (List (ProcessedFrame e t))
    | ProcessedGroup { id : String, name : String } (List (ProcessedPlayground e t))


type ProcessedFrame e t
    = ProcessedInteractive (FrameInternals e t)
    | ProcessedExample String (FrameInternals e t)
    | ProcessedDoco (Html (Update t e))



-- MSG AND MODEL


type Msg t e
    = ComponentUpdate (Internal.Update t e)
    | ViewPage String
    | UpdateSearch String


type alias Model t e =
    { state : Dict String (Type t)
    , processedTree : List (ProcessedPlayground e t)
    , currentPage : String
    , search : String
    }


type alias ComponentPlayground t e =
    Program () (Model t e) (Msg t e)



-- RE-EXPORTED ALIASES


type alias ComponentUpdate t e =
    Internal.Update t e


type alias Ref =
    Ref.Ref


type alias Type t =
    Component.Type.Type t



-- PROCESSING


extractLibrary_ : List (Playground e t msg) -> Internal.Library_
extractLibrary_ playgrounds =
    { index = List.concatMap extractFlatIndex_ playgrounds
    , groups = List.filterMap extractGroup_ playgrounds
    }


extractFlatIndex_ : Playground e t msg -> List { id : String, name : String }
extractFlatIndex_ pg =
    case pg of
        Page meta _ ->
            [ { id = meta.id, name = meta.name } ]

        Group _ children ->
            List.concatMap extractFlatIndex_ children


extractGroup_ : Playground e t msg -> Maybe { name : String, pages : List { id : String, name : String } }
extractGroup_ pg =
    case pg of
        Page _ _ ->
            Nothing

        Group meta children ->
            Just { name = meta.name, pages = List.concatMap extractFlatIndex_ children }


processPlayground : Internal.Library_ -> Playground e t (Update t e) -> State Ref (ProcessedPlayground e t)
processPlayground library_ pg =
    case pg of
        Page meta frames ->
            let
                lib =
                    Library meta.id library_
            in
            State.traverse (processFrame lib) frames
                |> State.map (ProcessedPage meta)

        Group meta children ->
            State.traverse (processPlayground library_) children
                |> State.map (ProcessedGroup meta)


processFrame : Library e t -> Frame e t (Update t e) -> State Ref (ProcessedFrame e t)
processFrame lib frame =
    case frame of
        InteractiveFrame f ->
            State.map ProcessedInteractive (f lib)

        ExampleFrame name f ->
            State.map (ProcessedExample name) (f lib)

        DocoFrame html ->
            State.state (ProcessedDoco html)


extractFlatIndex : List (ProcessedPlayground e t) -> List { id : String, name : String }
extractFlatIndex =
    List.concatMap extractFlatIndexItem


extractFlatIndexItem : ProcessedPlayground e t -> List { id : String, name : String }
extractFlatIndexItem item =
    case item of
        ProcessedPage meta _ ->
            [ meta ]

        ProcessedGroup _ children ->
            List.concatMap extractFlatIndexItem children


lookupCurrentFrames : List (ProcessedPlayground e t) -> String -> List (ProcessedFrame e t)
lookupCurrentFrames tree pageId =
    List.concatMap (lookupFramesInItem pageId) tree


lookupFramesInItem : String -> ProcessedPlayground e t -> List (ProcessedFrame e t)
lookupFramesInItem pageId item =
    case item of
        ProcessedPage meta frames ->
            if meta.id == pageId then
                frames

            else
                []

        ProcessedGroup _ children ->
            List.concatMap (lookupFramesInItem pageId) children



-- PUBLIC API


element :
    List (Playground () t (Update t ()))
    -> Maybe Url.Url
    -> ComponentPlayground t ()
element playgrounds url =
    Browser.element
        { init = \() -> ( init playgrounds url, Cmd.none )
        , update = \msg model -> ( update msg model |> Tuple.first, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


fromEffect : e -> Msg t e
fromEffect =
    (\e -> Internal.WithEffect [] [ e ]) >> ComponentUpdate


fromPreviewUpdate : ComponentUpdate t e -> Msg t e
fromPreviewUpdate =
    ComponentUpdate


init : List (Playground e t (Update t e)) -> Maybe Url.Url -> Model t e
init playgrounds url =
    let
        library_ =
            extractLibrary_ playgrounds

        processedTree =
            State.traverse (processPlayground library_) playgrounds
                |> Ref.fromTop

        flatIndex =
            extractFlatIndex processedTree

        currentPage =
            Maybe.map urlToPage url
                |> Maybe.withDefault (List.head flatIndex |> Maybe.map .id)
                |> Maybe.withDefault ""
    in
    { state = Dict.empty
    , processedTree = processedTree
    , currentPage = currentPage
    , search = ""
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
        ComponentUpdate previewUpdate ->
            let
                ( updates, effects ) =
                    case previewUpdate of
                        Internal.Update u e ->
                            ( u, e )

                        WithEffect u e ->
                            ( u, e )

                        Computed f ->
                            f (lookupCurrent model)
            in
            ( applyUpdates updates model
            , effects
            )

        ViewPage pageId ->
            ( { model | currentPage = pageId }, [] )

        UpdateSearch newSearch ->
            ( { model | search = newSearch }, [] )


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



-- VIEW


view : Model t e -> Html (Msg t e)
view model =
    UI.hStack
        (UI.fullHeight
            ++ [ UI.style "padding" "12px"
               , UI.style "gap" "12px"
               , UI.style "background-color" "#eee"
               ]
        )
        [ UI.vStack
            [ UI.style "width" "300px"
            , UI.style "overflow-y" "auto"
            , UI.style "max-height" "100%"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            ]
            [ viewSidebarHeader model
            , UI.vStack
                [ UI.style "overflow-y" "auto", UI.style "padding" "12px 24px" ]
                (List.map (viewPlaygroundTree model) model.processedTree)
            ]
        , UI.vStack
            [ UI.style "flex-grow" "1"
            , UI.style "padding" "24px 32px"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            , UI.style "overflow-y" "auto"
            ]
            (lookupCurrentFrames model.processedTree model.currentPage
                |> List.map (viewFrame model)
            )
        ]


viewSidebarHeader : Model t e -> Html (Msg t e)
viewSidebarHeader model =
    Html.div
        (UI.headingStyles ++ [ UI.style "padding" "24px", UI.style "border-bottom" "1px solid rgb(204, 204, 204)" ])
        [ Html.text "Library", viewSearchBox model ]


viewSearchBox : Model t e -> Html (Msg t e)
viewSearchBox model =
    Html.input
        (UI.inputStyles
            ++ [ Html.Attributes.placeholder "Search..."
               , Html.Attributes.value model.search
               , Html.Events.onInput UpdateSearch
               , Html.Attributes.id "playground-search"
               , UI.style "display" "block"
               , UI.style "width" "100%"
               , UI.style "margin-top" "8px"
               , UI.disableAutocomplete
               ]
        )
        []


viewPlaygroundTree : Model t e -> ProcessedPlayground e t -> Html (Msg t e)
viewPlaygroundTree model item =
    case item of
        ProcessedPage meta _ ->
            if String.toLower meta.name |> String.contains (String.toLower model.search) then
                viewPageLink model meta

            else
                Html.text ""

        ProcessedGroup meta children ->
            let
                filteredChildren =
                    List.filter (groupHasMatch model.search) children
            in
            if List.isEmpty filteredChildren then
                Html.text ""

            else
                UI.vStack [ UI.style "margin-bottom" "0.5em" ]
                    (Html.span UI.subHeadingStyles [ Html.text meta.name ]
                        :: List.map (viewPlaygroundTree model) filteredChildren
                    )


groupHasMatch : String -> ProcessedPlayground e t -> Bool
groupHasMatch search item =
    case item of
        ProcessedPage meta _ ->
            String.toLower meta.name |> String.contains (String.toLower search)

        ProcessedGroup _ children ->
            List.any (groupHasMatch search) children


viewPageLink : Model t e -> { id : String, name : String } -> Html (Msg t e)
viewPageLink model meta =
    UI.button
        (List.concat
            [ if meta.id == model.currentPage then
                [ UI.style "background-color" "#eee", UI.style "font-weight" "600" ]

              else
                []
            , [ UI.style "text-align" "left"
              , UI.style "padding" "8px 12px"
              , UI.style "border-radius" "8px"
              , UI.onClick (ViewPage meta.id)
              ]
            ]
        )
        [ Html.text meta.name ]


viewFrame : Model t e -> ProcessedFrame e t -> Html (Msg t e)
viewFrame model frame =
    case frame of
        ProcessedInteractive internals ->
            viewInteractiveFrame model Nothing internals

        ProcessedExample name internals ->
            viewInteractiveFrame model (Just name) internals

        ProcessedDoco html ->
            Html.div
                [ UI.style "padding" "0.5em" ]
                [ Html.map ComponentUpdate html ]


viewInteractiveFrame : Model t e -> Maybe String -> FrameInternals e t -> Html (Msg t e)
viewInteractiveFrame model maybeName internals =
    let
        lookup =
            lookupCurrent model
    in
    UI.vStack [ UI.style "gap" "24px" ]
        [ case maybeName of
            Just name ->
                Html.div UI.subHeadingStyles [ Html.text name ]

            Nothing ->
                Html.text ""
        , UI.hStack []
            [ UI.vStack
                [ UI.style "flex-grow" "1"
                , UI.style "max-height" "100%"
                , UI.style "padding" "0.5em"
                , UI.style "gap" "24px"
                ]
                [ Html.div UI.headingStyles [ Html.text "Component" ]
                , Html.div []
                    [ internals.render lookup
                        |> Tuple.first
                        |> Html.map ComponentUpdate
                    ]
                ]
            , UI.vStack
                [ UI.style "width" "350px"
                , UI.style "padding" "0.5em"
                , UI.style "max-height" "100%"
                , UI.style "align-items" "justify"
                , UI.style "gap" "8px"
                , UI.style "overflow-y" "auto"
                ]
                (Html.div UI.headingStyles [ Html.text "Controls" ]
                    :: List.map (Html.map ComponentUpdate) (internals.controls lookup)
                )
            ]
        ]
