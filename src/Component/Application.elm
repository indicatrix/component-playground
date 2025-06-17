module Component.Application exposing
    ( Msg, Model, ComponentPlayground
    , ComponentMsg, Library_, Preview, Type
    , element, init, update, view, fromMsg, fromPreviewMsg, viewPreview
    )

{-| TODO: write a description of the module

#Types

@docs Msg, Model, ComponentPlayground

#Re-exported Aliases

These opaque types are defined and exported from submodules. They are aliased
and exported here so that it is possible to write explicit type signatures.

@docs ComponentMsg, Library_, Preview, Type

#Top-level Application

The component playground can be run in one of two ways. The simplest is to
define an `element`. However, this means that any messages passed back from
components are ignored, so there is no way to run arbitrary commands.
Otherwise, `init`, `update`, and `view` can be called from another application.

@docs element, init, update, view, fromMsg, fromPreviewMsg, viewPreview

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


type Msg t msg
    = ComponentMsg (Component.Msg t msg)
    | ViewComponent String


type alias Model t msg =
    { state : Dict String (Type t)
    , library : Library_ t (Component.Msg t msg)
    , currentComponent : String
    }


type alias ComponentPlayground t msg =
    Program () (Model t msg) (Msg t msg)



{- Re-export types from submodules -}


type alias Library_ t msg =
    Component.Library_ t msg


type alias Preview t msg =
    Component.Preview t msg


type alias ComponentMsg t msg =
    Component.Msg t msg


type alias Type t =
    Component.Type.Type t


element :
    List (Preview t (Component.Msg t ()))
    -> ComponentPlayground t ()
element previews =
    Browser.element
        { init = \() -> ( init previews, Cmd.none )
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


init : List (Preview t (Component.Msg t msg)) -> Model t msg
init previews =
    let
        lib =
            Component.library_ previews
    in
    { state = Dict.empty
    , library = lib
    , currentComponent =
        List.head lib.index
            |> Maybe.map .id
            |> Maybe.withDefault ""
    }


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


view : Model t msg -> Html (Msg t msg)
view model =
    let
        lookup ref =
            Dict.get (Ref.toString ref) model.state
    in
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
            (Html.div (UI.headingStyles ++ [ UI.style "padding" "16px 12px" ])
                [ Html.text "Library" ]
                :: List.map
                    (\{ name, id } ->
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
                    )
                    model.library.index
            )
        , UI.hStack
            [ UI.style "flex-grow" "1"
            , UI.style "padding" "24px 32px"
            , UI.style "border-radius" "12px"
            , UI.style "background-color" "#fff"
            , UI.style "box-shadow" "#aaa 0px 2px 4px"
            , UI.style "gap" "48px"
            ]
            [ model.library.lookup_ model.currentComponent
                |> Maybe.map
                    (\( pId, ref, p ) ->
                        UI.vStack
                            [ UI.style "flex-grow" "1"
                            , UI.style "height" "100%"
                            , UI.style "padding" "0.5em"
                            , UI.style "gap" "24px"
                            ]
                            [ Html.div UI.headingStyles
                                [ Html.text "Component" ]
                            , Html.div
                                []
                                [ Html.map ComponentMsg <| Tuple.first <| Ref.from ref (p.value (Library pId model.library) lookup)
                                ]
                            ]
                    )
                |> Maybe.withDefault (Html.div [ UI.style "flex-grow" "1" ] [])
            , model.library.lookup_ model.currentComponent
                |> Maybe.map
                    (\( pId, ref, p ) ->
                        UI.vStack
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
                                    (Ref.from ref (p.controls (Library pId model.library)))
                            )
                    )
                |> Maybe.withDefault (Html.div [] [])
            ]
        ]


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
