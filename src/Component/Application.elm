module Component.Application exposing
    ( ComponentPlayground
    , Model
    , Msg
    , element
    , fromMsg
    , fromPreviewMsg
    , init
    , update
    , view
    , viewPreview
    )

import Browser
import Component.Preview as Preview
    exposing
        ( Library(..)
        , Library_
        , Preview(..)
        , PreviewRef(..)
        )
import Component.Ref as Ref exposing (Ref)
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


element :
    List (Preview t (Preview.Msg t ()) (Preview.View (Preview.Msg t ())))
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
    Preview.Msg [] >> PreviewMsg


fromPreviewMsg : Preview.Msg t msg -> Msg t msg
fromPreviewMsg =
    PreviewMsg


init : List (Preview t (Preview.Msg t msg) (Preview.View (Preview.Msg t msg))) -> Model t msg
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


update : Msg t msg -> Model t msg -> ( Model t msg, Maybe msg )
update msg model =
    case msg of
        PreviewMsg previewMsg ->
            let
                ( updates, innerMsg ) =
                    case previewMsg of
                        Preview.SetState u ->
                            ( u, Nothing )

                        Preview.Msg u inner ->
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
                                [ Html.map PreviewMsg <| Tuple.first <| Ref.from ref (p.value (Library p.meta.id model.library) lookup)
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


viewPreview : Model t msg -> PreviewRef -> Maybe String -> Ref -> Maybe (Html (Msg t msg))
viewPreview model (PreviewRef previewRef) viewId ref =
    let
        lookup ref_ =
            Dict.get (Ref.toString ref_) model.state
    in
    model.library.lookup previewRef
        |> Maybe.andThen
            (\(Preview p) ->
                let
                    ( main, aux ) =
                        Ref.from ref (p.value (Library p.meta.id model.library) lookup)
                in
                Maybe.map (Html.map PreviewMsg) <|
                    case viewId of
                        Nothing ->
                            Just main

                        Just auxRef ->
                            Dict.get auxRef aux
            )
