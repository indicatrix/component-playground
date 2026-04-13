module Component.Application exposing
    ( Msg, Model, ProcessedFrame, ComponentPlayground
    , ComponentUpdate, Index, Playground, Ref, Type
    , element, init, update, view, fromEffect, fromPreviewUpdate, toUrl
    )

{-| Application runner for the Component Playground.


# Types

@docs Msg, Model, ProcessedFrame, ComponentPlayground


# Re-exported Aliases

@docs ComponentUpdate, Index, Playground, Ref, Type


# Running the Playground

The playground can be run as a standalone `element`, or wired into a larger
application using `init`, `update`, and `view`.

@docs element, init, update, view, fromEffect, fromPreviewUpdate, toUrl

-}

import Browser
import Component.Application.Theme exposing (Theme)
import Component.Internal as Internal
    exposing
        ( ComponentE
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
import State exposing (State)
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query



-- PROCESSED TYPES
-- These are internal to Application; users interact with Frame/Playground.


type ProcessedFrame e t
    = ProcessedInteractive (Html (Update t e) -> Html (Update t e)) (ComponentE e t)
    | ProcessedExample String (Html (Update t e) -> Html (Update t e)) (ComponentE e t)
    | ProcessedStatic (Html (Update t e))
    | ProcessedGallery String (Html (Update t e))



-- MSG AND MODEL


type Msg t e
    = ComponentUpdate (Internal.Update t e)
    | ViewPage String
    | UpdateSearch String


type alias Model t e =
    { state : Dict String (Type t)
    , pages : Dict String (List (ProcessedFrame e t))
    , index : List Index
    , currentPage : String
    , search : String
    , theme : Theme
    }


type alias ComponentPlayground t e =
    Program () (Model t e) (Msg t e)



-- RE-EXPORTED ALIASES


type alias ComponentUpdate t e =
    Internal.Update t e


{-| Sidebar index tree. Re-exported from `Component.Internal`.
-}
type alias Index =
    Internal.Index


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


{-| Walk the Playground tree and collect all InteractiveFrame/ExampleFrame
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

                                ExampleFrame meta _ f _ ->
                                    Just { id = meta.id, name = meta.name, def = f }

                                StaticFrame _ ->
                                    Nothing

                                GalleryFrame _ _ ->
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
        InteractiveFrame _ f wrapper ->
            State.map (ProcessedInteractive wrapper) (f lib)

        ExampleFrame _ name_ f wrapper ->
            State.map (ProcessedExample name_ wrapper) (f lib)

        StaticFrame html ->
            State.state (ProcessedStatic html)

        GalleryFrame name f ->
            State.map (ProcessedGallery name) (f lib)



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


fromEffect : e -> Msg t e
fromEffect =
    (\e -> Internal.Update [] [ e ]) >> ComponentUpdate


fromPreviewUpdate : ComponentUpdate t e -> Msg t e
fromPreviewUpdate =
    ComponentUpdate


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
    , index = idx
    , currentPage = currentPage
    , search = ""
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
        ComponentUpdate previewUpdate ->
            let
                (Internal.Update updates effects) =
                    previewUpdate
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
    let
        theme =
            model.theme
    in
    Ui.hStack
        (Ui.fullHeight
            ++ [ Ui.style "padding" "12px"
               , Ui.style "gap" "12px"
               , Ui.style "background-color" theme.pageBackground
               ]
        )
        [ Ui.vStack
            [ Ui.style "width" "300px"
            , Ui.style "overflow-y" "auto"
            , Ui.style "max-height" "100%"
            , Ui.style "border-radius" "12px"
            , Ui.style "background-color" theme.panelBackground
            , Ui.style "box-shadow" (theme.shadowColor ++ " 0px 2px 4px")
            ]
            [ viewSidebarHeader model
            , Ui.vStack
                [ Ui.style "overflow-y" "auto", Ui.style "padding" "12px 24px" ]
                (List.map (viewIndex model) (orderChildren model.index))
            ]
        , Ui.vStack
            [ Ui.style "flex-grow" "1"
            , Ui.style "padding" "24px 32px"
            , Ui.style "border-radius" "12px"
            , Ui.style "background-color" theme.panelBackground
            , Ui.style "box-shadow" (theme.shadowColor ++ " 0px 2px 4px")
            , Ui.style "overflow-y" "auto"
            ]
            (Dict.get model.currentPage model.pages
                |> Maybe.withDefault []
                |> List.map (viewFrame model)
            )
        ]


viewSidebarHeader : Model t e -> Html (Msg t e)
viewSidebarHeader model =
    Html.div
        (Ui.headingStyles model.theme ++ [ Ui.style "padding" "24px", Ui.style "border-bottom" ("1px solid " ++ model.theme.sidebarDivider) ])
        [ Html.text "Library", viewSearchBox model ]


viewSearchBox : Model t e -> Html (Msg t e)
viewSearchBox model =
    Html.input
        (Ui.inputStyles model.theme
            ++ [ Html.Attributes.placeholder "Search..."
               , Html.Attributes.value model.search
               , Html.Events.onInput UpdateSearch
               , Html.Attributes.id "playground-search"
               , Ui.style "display" "block"
               , Ui.style "width" "100%"
               , Ui.style "margin-top" "8px"
               , Ui.disableAutocomplete
               ]
        )
        []


viewIndex : Model t e -> Index -> Html (Msg t e)
viewIndex model (Index item) =
    if List.isEmpty item.children then
        -- Page (leaf node)
        if String.toLower item.name |> String.contains (String.toLower model.search) then
            viewPageLink model { id = item.id, name = item.name }

        else
            Html.text ""

    else
        -- Group (has children)
        let
            filteredChildren =
                List.filter (indexHasMatch model.search) item.children
                    |> orderChildren
        in
        if List.isEmpty filteredChildren then
            Html.text ""

        else
            Ui.vStack [ Ui.style "margin-bottom" "0.5em" ]
                [ Html.span (Ui.subHeadingStyles model.theme) [ Html.text item.name ]
                , Ui.vStack
                    [ Ui.style "padding-left" "12px" ]
                    (List.map (viewIndex model) filteredChildren)
                ]


{-| Within a parent: leaf pages first (sorted alphabetically by name),
then groups in source order. Applied at every nesting level.
-}
orderChildren : List Index -> List Index
orderChildren children =
    let
        ( pages, groups ) =
            List.partition (\(Index item) -> List.isEmpty item.children) children
    in
    List.sortBy (\(Index item) -> String.toLower item.name) pages ++ groups


indexHasMatch : String -> Index -> Bool
indexHasMatch search (Index item) =
    if List.isEmpty item.children then
        String.toLower item.name |> String.contains (String.toLower search)

    else
        List.any (indexHasMatch search) item.children


viewPageLink : Model t e -> { id : String, name : String } -> Html (Msg t e)
viewPageLink model meta =
    Ui.button model.theme
        (List.concat
            [ if meta.id == model.currentPage then
                [ Ui.style "background-color" model.theme.activeLinkBackground
                , Ui.style "font-weight" model.theme.headingFontWeight
                ]

              else
                []
            , [ Ui.style "text-align" "left"
              , Ui.style "padding" "8px 12px"
              , Ui.style "border-radius" "8px"
              , Ui.onClick (ViewPage meta.id)
              ]
            ]
        )
        [ Html.text meta.name ]


viewFrame : Model t e -> ProcessedFrame e t -> Html (Msg t e)
viewFrame model frame =
    case frame of
        ProcessedInteractive wrapper internals ->
            viewInteractiveFrame model Nothing wrapper internals

        ProcessedExample name wrapper internals ->
            viewInteractiveFrame model (Just name) wrapper internals

        ProcessedStatic html ->
            Html.div
                [ Ui.style "padding" "0.5em" ]
                [ Html.map ComponentUpdate html ]

        ProcessedGallery name html ->
            Ui.vStack [ Ui.style "gap" "16px" ]
                [ Html.div (Ui.subHeadingStyles model.theme) [ Html.text name ]
                , Html.div [ Ui.style "padding" "0.5em" ] [ Html.map ComponentUpdate html ]
                ]


viewInteractiveFrame : Model t e -> Maybe String -> (Html (Update t e) -> Html (Update t e)) -> ComponentE e t -> Html (Msg t e)
viewInteractiveFrame model maybeName wrapper internals =
    let
        lookup =
            lookupCurrent model

        theme =
            model.theme
    in
    Ui.vStack [ Ui.style "gap" "24px" ]
        [ case maybeName of
            Just name ->
                Html.div (Ui.subHeadingStyles theme) [ Html.text name ]

            Nothing ->
                Html.text ""
        , Ui.hStack []
            [ Ui.vStack
                [ Ui.style "flex-grow" "1"
                , Ui.style "max-height" "100%"
                , Ui.style "padding" "0.5em"
                , Ui.style "gap" "24px"
                ]
                [ Html.div (Ui.headingStyles theme) [ Html.text "Component" ]
                , Html.div []
                    [ internals.render lookup
                        |> Tuple.first
                        |> wrapper
                        |> Html.map ComponentUpdate
                    ]
                ]
            , Ui.vStack
                [ Ui.style "width" "350px"
                , Ui.style "padding" "0.5em"
                , Ui.style "max-height" "100%"
                , Ui.style "align-items" "justify"
                , Ui.style "gap" "8px"
                , Ui.style "overflow-y" "auto"
                ]
                (Html.div (Ui.headingStyles theme) [ Html.text "Controls" ]
                    :: List.map (Html.map ComponentUpdate) (internals.controls theme lookup)
                )
            ]
        ]
