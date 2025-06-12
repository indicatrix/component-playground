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


type alias Model model t msg =
    { state : Dict String (Type t)
    , library : Library_ t (Preview.Msg t msg)
    , currentComponent : String
    , inner : model
    }


componentMsg : msg -> Msg t msg
componentMsg = PreviewMsg << Preview.Msg


type alias ComponentPlayground model t msg =
    Program () (Model model t msg) (Msg t msg)


playground :
    model
    -> (msg -> model -> ( model, Cmd msg ))
    -> Sub msg
    -> List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg)))
    -> ComponentPlayground model t msg
playground init update customSubs previews =
    Browser.element
        { init = \() -> ( initWrapper init previews, Cmd.none )
        , update = updateWrapper update
        , view = view
        , subscriptions = \_ -> Sub.map componentMsg customSubs
        }


initWrapper : model -> List (Preview t (Preview.Msg t msg) (Html (Preview.Msg t msg))) -> Model model t msg
initWrapper inner previews =
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
    , inner = inner
    }


updateWrapper : (msg -> model -> ( model, Cmd msg )) -> Msg t msg -> Model model t msg -> ( Model model t msg, Cmd (Msg t msg) )
updateWrapper innerUpdate msg model =
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

        PreviewMsg (Preview.Msg innerMsg) ->
            let
                ( m, cmd ) =
                    innerUpdate innerMsg model.inner
            in
            ( { model | inner = m }, Cmd.map componentMsg cmd )


view : Model model t msg -> Html (Msg t msg)
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
                    (\( ref, p ) ->
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
                                [ Html.map PreviewMsg <| Ref.from ref (p.value (Library p.meta.id model.library) lookup)
                                ]
                            ]
                    )
                |> Maybe.withDefault (Html.div [ UI.style "flex-grow" "1" ] [])
            , model.library.lookup_ model.currentComponent
                |> Maybe.map
                    (\( ref, p ) ->
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
                                        c lookup |> Html.map (Preview.SetState >> PreviewMsg)
                                    )
                                    (Ref.from ref (p.controls (Library p.meta.id model.library)))
                            )
                    )
                |> Maybe.withDefault (Html.div [] [])
            ]
        ]
