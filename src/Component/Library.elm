module Component.Library exposing (LibraryProgram, Msg, libraryProgram)

import Browser
import Component.Preview as Preview exposing (Lookup, Preview)
import Component.Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)


type Msg t msg
    = PreviewMsg (Preview.Msg t msg)
    | ViewComponent String


type alias Model t msg =
    { state : Dict String (Type t)
    , previews : Dict String (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg)))
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
        identified =
            -- List.map (\p -> ( Preview.identifier p, Preview.map (Html.map PreviewMsg) p )) previews
            List.map (\p -> ( Preview.identifier p, p )) previews
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
            ( { model | state = List.foldl (\( a, t ) -> Dict.insert a t) model.state updates }
            , Cmd.none
            )

        ViewComponent componentId ->
            ( { model | currentComponent = componentId }, Cmd.none )

        PreviewMsg (Preview.Msg msg_) ->
            ( model, Cmd.none )


view : Model t msg -> Html (Msg t msg)
view model =
    let
        lookup k =
            Dict.get k model.state

        previewLookup k =
            Dict.get k model.previews
                |> Maybe.withDefault
                    (Preview.preview "default-component"
                        { name = "Default Component" }
                        (Html.text "Default Component")
                    )
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
                                    (\b ->
                                        { title = Preview.name b
                                        , active = Preview.identifier b == model.currentComponent
                                        , onClick = Preview.identifier b |> ViewComponent
                                        }
                                    )
                        )
            }
        , Dict.get model.currentComponent model.previews
            |> Maybe.map
                (\p -> UI.componentArea (Preview.name p) "#fff" (Preview.view previewLookup lookup (Preview.map (Html.map PreviewMsg) p)))
            |> Maybe.withDefault (UI.componentArea "" "clear" (Html.text "None selected"))
        , Dict.get model.currentComponent model.previews
            |> Maybe.map
                (\p ->
                    UI.controlsArea
                        (Preview.controls previewLookup lookup (Preview.SetState >> PreviewMsg) p)
                        (Preview.state lookup (Preview.SetState >> PreviewMsg) p)
                )
            |> Maybe.withDefault (UI.controlsArea [] [])
        ]
