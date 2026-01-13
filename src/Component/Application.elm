module Component.Application exposing
    ( Msg, Model, ComponentPlayground
    , Block, ComponentUpdate, Library_, Preview, PreviewGroup, Ref, Type
    , element, init, update, view, fromEffect, fromPreviewUpdate, viewPreview, toUrl
    , updateAt
    )

{-| TODO: write a description of the module

#Types

@docs Msg, Model, ComponentPlayground

#Re-exported Aliases

These opaque types are defined and exported from submodules. They are aliased
and exported here so that it is possible to write explicit type signatures.

@docs Block, ComponentUpdate, Library_, Preview, PreviewGroup, Ref, Type

#Top-level Application

The component playground can be run in one of two ways. The simplest is to
define an `element`. However, this means that any messages passed back from
components are ignored, so there is no way to run arbitrary commands.
Otherwise, `init`, `update`, and `view` can be called from another application.

@docs element, init, update, view, fromEffect, fromPreviewUpdate, viewPreview, toUrl

@docs updateAt

-}

import Browser
import Component.Internal as Internal
    exposing
        ( Block
        , BlockI(..)
        , Component(..)
        , ComponentRef(..)
        , Library(..)
        , Update(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import State
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


library_ : List (PreviewGroup e t) -> Library_ e t
library_ groups =
    let
        withRef ( meta, Component p ) =
            Ref.take |> State.map (\ref -> ( meta.id, ( ref, p ) ))

        allPreviews =
            List.concatMap .previews groups

        lib =
            allPreviews
                |> State.traverse withRef
                |> Ref.fromTop
                |> Dict.fromList
    in
    { index = List.map Tuple.first allPreviews
    , groups = List.map (\componentGroup -> { name = componentGroup.name, components = List.map Tuple.first componentGroup.previews }) groups
    , lookup = \s -> Dict.get s lib |> Maybe.map (\( _, p ) -> ( s, Component p ))
    , lookup_ = \s -> Dict.get s lib |> Maybe.map (\( r, p ) -> ( s, r, p ))
    }


type Msg t e
    = ComponentUpdate (Internal.Update t e)
    | ViewComponent String
    | UpdateSearch String


type alias Model t e =
    { state : Dict String (Type t)
    , library : Library_ e t
    , currentComponent : String
    , search : String
    }


type alias ComponentPlayground t e =
    Program () (Model t e) (Msg t e)



{- Re-export types from submodules -}


type alias Block e t a =
    Internal.Block e t a


type alias Library_ e t =
    Internal.Library_ e t


type alias Preview e t =
    Internal.Preview e t


type alias PreviewGroup e t =
    Internal.PreviewGroup e t


type alias ComponentUpdate t e =
    Internal.Update t e


type alias Ref =
    Ref.Ref


type alias Type t =
    Component.Type.Type t


element :
    List (PreviewGroup () t)
    -> Maybe Url.Url
    -> ComponentPlayground t ()
element previews url =
    Browser.element
        { init = \() -> ( init previews url, Cmd.none )
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


init : List (PreviewGroup e t) -> Maybe Url.Url -> Model t e
init groups url =
    let
        lib =
            library_ groups
    in
    { state = Dict.empty
    , library = lib
    , currentComponent =
        Maybe.map urlToComponent url
            |> Maybe.withDefault (List.head lib.index |> Maybe.map .id)
            |> Maybe.withDefault ""
    , search = ""
    }


urlToComponent : Url.Url -> Maybe String
urlToComponent url =
    let
        parser =
            Url.Parser.query (Url.Parser.Query.string "component")
    in
    -- see https://github.com/elm/url/issues/17
    Url.Parser.parse parser { url | path = "" }
        |> Maybe.withDefault Nothing


toUrl : String -> Model t e -> String
toUrl path model =
    Url.Builder.relative [ path ] [ Url.Builder.string "component" model.currentComponent ]


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

        ViewComponent componentId ->
            ( { model | currentComponent = componentId }, [] )

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


{-|

    Update a value at the specified ref. WARNING! If provided block is used with
    a 'with' function that provides a default when building the Component (eg:
    withControl, withState, withUnlabelled, ...), the function creates an
    internal Block value which is used for the Component. Use ('underscore')
    variants that don't set a default (eg: withControl_, withState_), along with
    setDefault to create an Block value that can be referenced.

-}
updateAt : Ref -> Block e t a -> (a -> ( a, b )) -> Model t e -> ( Model t e, b )
updateAt ref (Block block_) updateF model =
    let
        b =
            Ref.fromNested ref block_
    in
    b.fromType b.default b.default (lookupCurrent model)
        |> updateF
        |> Tuple.mapFirst (\value -> applyUpdates (b.toType value) model)


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
            , UI.vStack [ UI.style "overflow-y" "auto", UI.style "padding" "12px 24px" ] (List.map (viewComponentGroup model) model.library.groups)
            ]
        , UI.vStack
            [ UI.style "flex-grow" "1"
            , UI.style "padding" "24px 32px"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            , UI.style "overflow-y" "auto"
            ]
            [ UI.hStack [] (model.library.lookup_ model.currentComponent |> Maybe.map (viewConfigurableComponent model) |> Maybe.withDefault [])
            , Html.div [ UI.style "height" "1px", UI.style "width" "100%", UI.style "margin" "1em 0", UI.style "border-bottom" "1px solid #ccc" ] []
            , Html.div UI.headingStyles [ Html.text "Stories" ]
            , UI.vStack [] (model.library.lookup_ model.currentComponent |> Maybe.map (viewComponentStories model) |> Maybe.withDefault [])
            ]
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


viewComponentGroup : Model t e -> { name : String, components : List { name : String, id : String } } -> Html (Msg t e)
viewComponentGroup model group =
    let
        components =
            group.components
                |> List.filter (.name >> String.toLower >> String.contains (String.toLower model.search))
                |> List.sortBy .name
    in
    UI.vStack [ UI.style "margin-bottom" "0.5em" ] <| Html.span UI.subHeadingStyles [ Html.text group.name ] :: List.map (viewComponentMeta model) components


viewComponentMeta : Model t e -> { name : String, id : String } -> Html (Msg t e)
viewComponentMeta model { name, id } =
    UI.button
        (List.concat
            [ if id == model.currentComponent then
                [ UI.style "background-color" "#eee", UI.style "font-weight" "600" ]

              else
                []
            , [ UI.style "text-align" "left", UI.style "padding" "8px 12px", UI.style "border-radius" "8px", UI.onClick <| ViewComponent id ]
            ]
        )
        [ Html.text name ]


viewConfigurableComponent : Model t e -> ( String, Ref, Internal.Component_ e t (Internal.View (Internal.Update t e)) ) -> List (Html (Msg t e))
viewConfigurableComponent model ( componentId, componentRef, p ) =
    let
        lookup r =
            Dict.get (Ref.toString r) model.state
    in
    [ UI.vStack
        [ UI.style "flex-grow" "1"
        , UI.style "max-height" "100%"
        , UI.style "padding" "0.5em"
        , UI.style "gap" "24px"
        ]
        [ Html.div UI.headingStyles
            [ Html.text "Component" ]
        , Html.div []
            [ Ref.from componentRef (p.value (Library componentId model.library) lookup)
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
        (Html.div UI.headingStyles
            [ Html.text "Controls" ]
            :: List.map
                (\c ->
                    c lookup |> Html.map (\( state, effects ) -> Internal.Update state effects |> ComponentUpdate)
                )
                (Ref.from componentRef (p.controls (Library componentId model.library)))
        )
    ]


viewComponentStories : Model t e -> ( String, Ref, Internal.Component_ e t (Internal.View (Internal.Update t e)) ) -> List (Html (Msg t e))
viewComponentStories _ _ =
    -- Not yet implemented - UI is being scaffolded out optimistically
    []


viewPreview : Model t e -> ComponentRef -> Maybe String -> Ref -> Maybe (Html (Msg t e))
viewPreview model (ComponentRef previewRef) viewId ref =
    let
        lookup ref_ =
            Dict.get (Ref.toString ref_) model.state
    in
    model.library.lookup previewRef
        |> Maybe.andThen
            (\( pId, Component p ) ->
                let
                    ( main, aux ) =
                        Ref.fromNested ref (p.value (Library pId model.library) lookup)
                in
                Maybe.map (Html.map ComponentUpdate) <|
                    case viewId of
                        Nothing ->
                            Just main

                        Just auxRef ->
                            Dict.get auxRef aux
            )
