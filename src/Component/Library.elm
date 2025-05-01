module Component.Library exposing (LibraryProgram, Msg, libraryProgram)

import Browser
import Component.Internal as Preview exposing (Lookup, Preview, Preview_)
import Component.Ref as Ref exposing (Ref)
import Component.Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)
import State


type Msg t msg
    = PreviewMsg (Preview.Msg t msg)
    | ViewComponent String


type alias Model t msg =
    { state : Dict String (Type t)
    , previews : Dict String (Preview_ t (Preview.Msg t msg) (Html (Preview.Msg t msg)))
    , previewOrder : List String
    , currentComponent : String
    }


type alias LibraryProgram t msg =
    Program () (Model t msg) (Msg t msg)


libraryProgram : List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg))) -> Sub msg -> LibraryProgram t msg
libraryProgram previews customSubs =
    Browser.element
        { init = \() -> ( init previews, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.map (PreviewMsg << Preview.Msg) customSubs
        }


init : List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg))) -> Model t msg
init previews =
    let
        previews_ =
            State.finalValue Ref.init (State.traverse Preview.unwrap previews)

        identified =
            List.map (\p -> ( p.meta.id, p )) previews_
    in
    { state = Dict.empty
    , previews = Dict.fromList identified
    , previewOrder = List.map Tuple.first identified
    , currentComponent =
        List.head identified
            |> Maybe.map Tuple.first
            |> Maybe.withDefault ""
    }


update : Msg t msg -> Model t msg -> ( Model t msg, Cmd (Msg t msg) )
update msg model =
    case msg of
        PreviewMsg (Preview.SetState updates) ->
            ( { model
                | state =
                    List.foldl
                        (\( ref, t ) ->
                            Dict.insert (Ref.toString ref) t
                        )
                        model.state
                        updates
              }
            , Cmd.none
            )

        ViewComponent componentId ->
            ( { model | currentComponent = componentId }, Cmd.none )

        PreviewMsg (Preview.Msg msg_) ->
            ( model, Cmd.none )


view : Model t msg -> Html (Msg t msg)
view model =
    let
        lookup ref =
            Dict.get (Ref.toString ref) model.state

        previewLookup k =
            Dict.get k model.previews
                |> Maybe.withDefault defaultPreview_
                |> Preview.fromPreview_
    in
    UI.hStack UI.fullHeight
        [ UI.sidebar
            { heading = "Component Library"
            , contents =
                model.previewOrder
                    |> List.filterMap
                        (\id ->
                            Dict.get id model.previews
                                |> Maybe.map
                                    (\p ->
                                        { title = p.meta.name
                                        , active = p.meta.id == model.currentComponent
                                        , onClick = p.meta.id |> ViewComponent
                                        }
                                    )
                        )
            }
        , Dict.get model.currentComponent model.previews
            |> Maybe.map
                (\p ->
                    UI.componentArea p.meta.name
                        "#fff"
                        (Html.map PreviewMsg <| p.value previewLookup lookup)
                )
            |> Maybe.withDefault (UI.componentArea "" "clear" (Html.text "None selected"))
        , Dict.get model.currentComponent model.previews
            |> Maybe.map
                (\p ->
                    UI.controlsArea
                        (List.map
                            (\c ->
                                c previewLookup lookup |> Html.map (Preview.SetState >> PreviewMsg)
                            )
                            p.controls
                        )
                        (List.map
                            (\s ->
                                s lookup |> Html.map (\() -> PreviewMsg <| Preview.SetState [])
                            )
                            p.state
                        )
                )
            |> Maybe.withDefault (UI.controlsArea [] [])
        ]


defaultPreview_ : Preview_ t (Preview.Msg t msg) (Html (Preview.Msg t msg))
defaultPreview_ =
    { meta = { name = "Default Component", id = "default-component" }
    , value = \_ _ -> Html.text "Default Component"
    , controls = []
    , state = []
    }
