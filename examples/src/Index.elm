port module Index exposing (main)

import Browser
import Component
import Component.Application
import Component.Application.Theme as Theme
import Component.Frame as Frame
import Component.Playground as Playground
import Components
import Html
import Html.Attributes
import Url
import Url.Builder
import Url.Parser
import Url.Parser.Query


port pushUrl_ : String -> Cmd msg


type alias Model =
    Component.Application.Model () ()


type alias Msg =
    Component.Application.Msg () ()


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = Component.Application.view
        , subscriptions = \_ -> Sub.none
        }


init : String -> ( Model, Cmd Msg )
init urlString =
    ( Component.Application.init Theme.default previews
        |> (case Url.fromString urlString |> Maybe.andThen urlToLocation of
                Just loc ->
                    Component.Application.setLocation loc >> Tuple.first

                Nothing ->
                    identity
           )
    , Cmd.none
    )


urlToLocation : Url.Url -> Maybe Component.Application.Location
urlToLocation url =
    let
        parser =
            Url.Parser.query
                (Url.Parser.Query.map2
                    (\page preset -> Maybe.map (\p -> { page = p, preset = preset }) page)
                    (Url.Parser.Query.string "component")
                    (Url.Parser.Query.string "preset")
                )
    in
    -- see https://github.com/elm/url/issues/17
    Url.Parser.parse parser { url | path = "" }
        |> Maybe.andThen identity


locationToUrl : Component.Application.Location -> String
locationToUrl loc =
    Url.Builder.relative [ "/" ]
        (Url.Builder.string "component" loc.page
            :: (loc.preset |> Maybe.map (Url.Builder.string "preset") |> Maybe.map List.singleton |> Maybe.withDefault [])
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, _ ) =
            Component.Application.update msg model
    in
    ( newModel
    , pushUrl_ (locationToUrl (Component.Application.location newModel))
    )


previews : List (Component.Application.Playground () ())
previews =
    [ Playground.group { uuid = "18935ead-b783-4b33-99f5-44f016916e1b", name = "Components" }
        [ Playground.fromComponent Components.textField
        , Playground.fromComponent Components.dropdownInput
        , Playground.fromComponent Components.identifierTest
        , Playground.fromComponent Components.intInput
        , Playground.fromComponent Components.floatInput
        , Playground.fromComponent Components.listTest
        , Playground.fromComponent Components.comboElement
        , Playground.fromComponent Components.contentBlock
        ]
    , Playground.group { uuid = "efce65ca-759c-4616-a98b-e56e091a5bdd", name = "Presets" }
        [ Playground.fromFrames { name = "Panel (preset tabs)" }
            [ Frame.subheading "Preset tab bar"
            , Frame.presets Components.panel
            ]
        , Playground.fromFrames { name = "Panel (preset gallery)" }
            [ Frame.subheading "All presets side-by-side"
            , Frame.presetGallery Components.panel
            ]
        , Playground.fromFrames { name = "Dashboard (embeds Panel)" }
            [ Frame.subheading "Panel preset picker travels into the controls pane"
            , Frame.fromComponent Components.dashboard
            ]
        ]
    , Playground.group { uuid = "580a8081-2380-435f-a31f-d73503c4bc51", name = "Frame Types" }
        [ Playground.fromFrames { name = "fromComponent with wrap" }
            [ Frame.fromComponent (Components.textFieldWith { uuid = "60396c7e-dfd8-423b-91cf-ce8ec6712e26", name = "Wrapped text field" })
                |> Frame.wrap
                    (\inner ->
                        Html.div
                            [ Html.Attributes.style "background-color" "#1a1a2e"
                            , Html.Attributes.style "padding" "32px"
                            , Html.Attributes.style "border-radius" "8px"
                            ]
                            [ inner ]
                    )
            ]
        , Playground.fromFrames { name = "example with wrap" }
            [ Frame.subheading "Pre-filled, framed"
            , Components.textFieldWith { uuid = "f8e7433a-5090-494e-af59-27855be01319", name = "Pre-filled text field" }
                |> Component.withPresets
                    [ Component.preset "Prefilled"
                        { value = "Hello", label = "Name", id = "wex-1", error = "" }
                    ]
                |> Frame.presets
                |> Frame.wrap
                    (\inner ->
                        Html.div
                            [ Html.Attributes.style "border" "2px dashed #888"
                            , Html.Attributes.style "padding" "24px"
                            , Html.Attributes.style "border-radius" "8px"
                            ]
                            [ inner ]
                    )
            ]
        , Playground.group { uuid = "35692f84-30ef-4768-9d0a-3b2789bd447a", name = "Gallery" }
            [ Playground.fromFrames { name = "Text field variants" }
                [ Frame.subheading "Text field states"
                , Frame.gallery Components.textField
                    (\render ->
                        Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            , Html.Attributes.style "gap" "16px"
                            ]
                            [ render { value = "Hello", label = "Filled", id = "gf-1", error = "" }
                            , render { value = "", label = "Empty", id = "gf-2", error = "" }
                            , render { value = "Invalid", label = "With error", id = "gf-3", error = "This field is required" }
                            ]
                    )
                ]
            , Playground.fromFrames { name = "Content block variants" }
                [ Frame.subheading "Content block variants"
                , Frame.gallery Components.contentBlock
                    (\render ->
                        Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "flex-direction" "column"
                            , Html.Attributes.style "gap" "16px"
                            ]
                            [ render { kind = "text", text = "Hello world", number = 0, toggle = False }
                            , render { kind = "number", text = "", number = 42, toggle = False }
                            , render { kind = "toggle", text = "", number = 0, toggle = True }
                            ]
                    )
                ]
            ]
        ]
    ]
