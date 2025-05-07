module Component.Application exposing (ComponentPlayground, Msg, playground)

import Browser
import Component.Preview as Preview exposing (Library(..), Library_, Preview)
import Component.Ref as Ref
import Component.Type exposing (Type)
import Component.UI as UI
import Dict exposing (Dict)
import Html exposing (Html)


type Msg t msg
    = PreviewMsg (Preview.Msg t msg)
    | ViewComponent String


type alias Model t msg =
    { state : Dict String (Type t)
    , library : Library_ t (Preview.Msg t msg)
    , currentComponent : String
    }


type alias ComponentPlayground t msg =
    Program () (Model t msg) (Msg t msg)


playground :
    List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg)))
    -> Sub msg
    -> ComponentPlayground t msg
playground previews customSubs =
    Browser.element
        { init = \() -> ( init previews, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.map (PreviewMsg << Preview.Msg) customSubs
        }


init : List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg))) -> Model t msg
init previews =
    let
        lib =
            Preview.library_ previews
    in
    { state = Dict.empty
    , library = lib
    , currentComponent =
        List.head lib.index
            |> Maybe.map .id
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
    in
    UI.hStack UI.fullHeight
        [ UI.sidebar
            { heading = "Component Library"
            , contents =
                model.library.index
                    |> List.map
                        (\{ name, id } ->
                            { title = name
                            , active = id == model.currentComponent
                            , onClick = id |> ViewComponent
                            }
                        )
            }
        , model.library.lookup_ model.currentComponent
            |> Maybe.map
                (\( ref, p ) ->
                    UI.componentArea p.meta.name
                        "#fff"
                        (Html.map PreviewMsg <| Ref.from ref (p.value (Library p.meta.id model.library) lookup))
                )
            |> Maybe.withDefault (UI.componentArea "" "clear" (Html.text "None selected"))
        , model.library.lookup_ model.currentComponent
            |> Maybe.map
                (\( ref, p ) ->
                    UI.controlsArea <|
                        List.map
                            (\c ->
                                c lookup |> Html.map (Preview.SetState >> PreviewMsg)
                            )
                            (Ref.from ref (p.controls (Library p.meta.id model.library)))
                )
            |> Maybe.withDefault (UI.controlsArea [])
        ]
