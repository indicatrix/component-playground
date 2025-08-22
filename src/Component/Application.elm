module Component.Application exposing
    ( Msg, Model, ComponentPlayground
    , ComponentMsg, Library_, Preview, PreviewGroup, Type
    , element, init, update, view, fromMsg, fromPreviewMsg, viewPreview, toUrl
    )

{-| TODO: write a description of the module

#Types

@docs Msg, Model, ComponentPlayground

#Re-exported Aliases

These opaque types are defined and exported from submodules. They are aliased
and exported here so that it is possible to write explicit type signatures.

@docs ComponentMsg, Library_, Preview, PreviewGroup, Type

#Top-level Application

The component playground can be run in one of two ways. The simplest is to
define an `element`. However, this means that any messages passed back from
components are ignored, so there is no way to run arbitrary commands.
Otherwise, `init`, `update`, and `view` can be called from another application.

@docs element, init, update, view, fromMsg, fromPreviewMsg, viewPreview, toUrl

-}

import Browser
import Component.Component as Component
    exposing
        ( Component(..)
        , ComponentRef(..)
        , Library(..)
        )
import Component.Ref as Ref exposing (Ref)
import Component.Type
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


type Msg t msg
    = ComponentMsg (Component.Msg t msg)
    | ViewComponent String
    | UpdateSearch String


type alias Model t msg =
    { state : Dict String (Type t)
    , library : Library_ t (Component.Msg t msg)
    , currentComponent : String
    , search : String
    }


type alias ComponentPlayground t msg =
    Program () (Model t msg) (Msg t msg)



{- Re-export types from submodules -}


type alias Library_ t msg =
    Component.Library_ t msg


type alias Preview t msg =
    Component.Preview t msg


type alias PreviewGroup t msg =
    Component.PreviewGroup t msg


type alias ComponentMsg t msg =
    Component.Msg t msg


type alias Type t =
    Component.Type.Type t


element :
    List (PreviewGroup t (Component.Msg t ()))
    -> Maybe Url.Url
    -> ComponentPlayground t ()
element previews url =
    Browser.element
        { init = \() -> ( init previews url, Cmd.none )
        , update = \model msg -> ( update model msg |> Tuple.first, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }


fromMsg : msg -> Msg t msg
fromMsg =
    Component.Msg [] >> ComponentMsg


fromPreviewMsg : ComponentMsg t msg -> Msg t msg
fromPreviewMsg =
    ComponentMsg


init : List (PreviewGroup t (Component.Msg t msg)) -> Maybe Url.Url -> Model t msg
init groups url =
    let
        lib =
            Component.library_ groups
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


toUrl : String -> Model t msg -> String
toUrl path model =
    Url.Builder.relative [ path ] [ Url.Builder.string "component" model.currentComponent ]


update : Msg t msg -> Model t msg -> ( Model t msg, Maybe msg )
update msg model =
    case msg of
        ComponentMsg previewMsg ->
            let
                ( updates, innerMsg ) =
                    case previewMsg of
                        Component.SetState u ->
                            ( u, Nothing )

                        Component.Msg u inner ->
                            ( u, Just inner )

                        Component.Update f ->
                            let
                                lookup ref =
                                    Dict.get (Ref.toString ref) model.state

                                ( u, inner ) =
                                    f lookup
                            in
                            ( u, Just inner )
            in
            ( { model
                | state =
                    List.foldl
                        (\( ref, t ) ->
                            Dict.insert (Ref.toString ref) t
                        )
                        model.state
                        updates
              }
            , innerMsg
            )

        ViewComponent componentId ->
            ( { model | currentComponent = componentId }, Nothing )

        UpdateSearch newSearch ->
            ( { model | search = newSearch }, Nothing )


view : Model t msg -> Html (Msg t msg)
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
            , UI.style "padding" "24px"
            , UI.style "overflow-y" "auto"
            , UI.style "max-height" "100%"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            ]
            (viewSidebarHeader model :: List.map (viewComponentGroup model) model.library.groups)
        , UI.vStack
            [ UI.style "flex-grow" "1"
            , UI.style "padding" "24px 32px"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            ]
            [ UI.hStack [] (model.library.lookup_ model.currentComponent |> Maybe.map (viewConfigurableComponent model) |> Maybe.withDefault [])
            , Html.div [ UI.style "height" "1px", UI.style "width" "100%", UI.style "margin" "1em 0", UI.style "border-bottom" "1px solid #ccc" ] []
            , Html.div UI.headingStyles [ Html.text "Stories" ]
            , UI.vStack [] (model.library.lookup_ model.currentComponent |> Maybe.map (viewComponentStories model) |> Maybe.withDefault [])
            ]
        ]


viewSidebarHeader : Model t msg -> Html (Msg t msg)
viewSidebarHeader model =
    Html.div
        (UI.headingStyles ++ [ UI.style "padding-bottom" "1em" ])
        [ Html.text "Library", viewSearchBox model ]


viewSearchBox : Model t msg -> Html (Msg t msg)
viewSearchBox model =
    Html.input
        (UI.inputStyles ++ [ Html.Attributes.placeholder "Search..."
        , Html.Attributes.value model.search
        , Html.Events.onInput UpdateSearch
        , Html.Attributes.id "playground-search"
        , UI.style "display" "block"
        , UI.style "width" "100%"
        , UI.style "margin-top" "8px"
        , UI.disableAutocomplete
        ])
        []


viewComponentGroup : Model t msg -> { name : String, components : List { name : String, id : String } } -> Html (Msg t msg)
viewComponentGroup model group =
    let
        components =
            group.components
                |> List.filter (.name >> String.toLower >> String.contains (String.toLower model.search))
                |> List.sortBy .name
    in
    UI.vStack [ UI.style "margin-bottom" "0.5em" ] <| Html.span UI.subHeadingStyles [ Html.text group.name ] :: List.map (viewComponentMeta model) components


viewComponentMeta : Model t msg -> { name : String, id : String } -> Html (Msg t msg)
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


viewConfigurableComponent : Model t msg -> ( String, Ref, Component.Component_ t (Component.Msg t msg) (Component.View (Component.Msg t msg)) ) -> List (Html (Msg t msg))
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
                |> Html.map ComponentMsg
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
                    c lookup |> Html.map (Component.SetState >> ComponentMsg)
                )
                (Ref.from componentRef (p.controls (Library componentId model.library)))
        )
    ]


viewComponentStories : Model t msg -> ( String, Ref, Component.Component_ t (Component.Msg t msg) (Component.View (Component.Msg t msg)) ) -> List (Html (Msg t msg))
viewComponentStories _ _ =
    -- Not yet implemented - UI is being scaffolded out optimistically
    []


viewPreview : Model t msg -> ComponentRef -> Maybe String -> Ref -> Maybe (Html (Msg t msg))
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
                Maybe.map (Html.map ComponentMsg) <|
                    case viewId of
                        Nothing ->
                            Just main

                        Just auxRef ->
                            Dict.get auxRef aux
            )
