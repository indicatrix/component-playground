port module Index exposing (main)

import Browser
import Component
import Component.Application
import Component.Application.Theme as Theme
import Component.Control as Control
import Component.Frame as Frame
import Component.Playground as Playground
import Components
import Html
import Html.Attributes
import Url


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
        , subscriptions = Component.Application.subscriptions
        }


init : String -> ( Model, Cmd Msg )
init urlString =
    ( Component.Application.init Theme.default previews (Url.fromString urlString)
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, _ ) =
            Component.Application.update msg model
    in
    ( newModel
    , pushUrl_ (Component.Application.toUrl "/" newModel)
    )


{-| A demo component whose markup carries the AI Inspector `data-*` hooks, so
the selection engine has real metadata + tokens to capture. Doubles as living
documentation of how a host annotates inspectable elements.
-}
aiDemo : Component.Component e t Bool msg
aiDemo =
    Component.component
        { id = "ai-inspector-demo"
        , name = "AI Inspector Demo"
        , controls = Control.bool |> Control.withDefault False
        , view = \_ _ -> aiDemoCard
        }


aiDemoCard : Html.Html msg
aiDemoCard =
    let
        attr =
            Html.Attributes.attribute
    in
    Html.div
        [ attr "data-ai-inspectable" ""
        , attr "data-component" "Auth Card"
        , attr "data-element" "Card"
        , attr "data-source-file" "js/src/UI/AuthCard.elm"
        , attr "data-source-symbol" "AuthCard.view"

        -- High-level layout tokens for the surface (Surface / Radius / Shadow /
        -- Padding / Gap).
        , attr "data-token-surface" "surface"
        , attr "data-token-radius" "radius-xl"
        , attr "data-token-shadow" "shadow-2"
        , attr "data-token-padding" "space-8"
        , attr "data-token-gap" "space-6"
        , Html.Attributes.style "max-width" "420px"
        , Html.Attributes.style "margin" "0 auto"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "24px"
        , Html.Attributes.style "padding" "32px"
        , Html.Attributes.style "border" "1px solid #E5E8EC"
        , Html.Attributes.style "border-radius" "12px"
        , Html.Attributes.style "background" "#ffffff"
        , Html.Attributes.style "font-family" "Inter, system-ui, sans-serif"
        ]
        [ Html.h1
            [ attr "data-ai-inspectable" ""
            , attr "data-element" "Title"

            -- Typography folds family / line-height / letter-spacing / weight
            -- into the one "Style" token; plus a colour token.
            , attr "data-token-style" "text-display-lg"
            , attr "data-token-colour" "ink"
            , Html.Attributes.style "font-size" "28px"
            , Html.Attributes.style "font-weight" "700"
            , Html.Attributes.style "color" "#0A0F22"
            , Html.Attributes.style "margin" "0"
            ]
            [ Html.text "Review account requirements" ]
        , Html.p
            [ attr "data-ai-inspectable" ""
            , attr "data-element" "Body"
            , attr "data-token-style" "text-body-md"
            , attr "data-token-colour" "ink-3"
            , Html.Attributes.style "font-size" "15px"
            , Html.Attributes.style "line-height" "1.5"
            , Html.Attributes.style "color" "#5A5D66"
            , Html.Attributes.style "margin" "0"
            ]
            [ Html.text "Confirm your details before continuing to your new account." ]
        , Html.button
            [ attr "data-ai-inspectable" ""
            , attr "data-element" "Continue"

            -- Button-level tokens: style variant, size, colour/intent.
            , attr "data-token-style" "primary"
            , attr "data-token-size" "medium"
            , attr "data-token-colour" "primary"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "44px"
            , Html.Attributes.style "border" "none"
            , Html.Attributes.style "border-radius" "8px"
            , Html.Attributes.style "background" "#2F7FFE"
            , Html.Attributes.style "color" "#ffffff"
            , Html.Attributes.style "font-size" "15px"
            , Html.Attributes.style "font-weight" "600"
            , Html.Attributes.style "cursor" "pointer"
            ]
            [ Html.text "Continue" ]
        ]


previews : List (Component.Application.Playground () ())
previews =
    [ Playground.group { id = "ai-inspector", name = "AI Inspector" }
        [ Playground.fromComponent { id = "ai-inspector-demo", name = "AI Inspector Demo" } aiDemo
        ]
    , Playground.group { id = "components", name = "Components" }
        [ Playground.fromComponent { id = "text-field", name = "Text field" } Components.textField
        , Playground.fromComponent { id = "dropdown-input", name = "Simple Dropdown Input" } Components.dropdownInput
        , Playground.fromComponent { id = "test-1", name = "Test 1" } Components.identifierTest
        , Playground.fromComponent { id = "int-input", name = "Int Input" } Components.intInput
        , Playground.fromComponent { id = "float-input", name = "Float Input" } Components.floatInput
        , Playground.fromComponent { id = "list-test", name = "List test" } Components.listTest
        , Playground.fromComponent { id = "combo-element", name = "Combination Element" } Components.comboElement
        , Playground.fromComponent { id = "content-block", name = "Content Block (Sum Type)" } Components.contentBlock
        ]
    , Playground.group { id = "presets", name = "Presets" }
        [ Playground.fromFrames { id = "panel-presets", name = "Panel (preset tabs)" }
            [ Frame.subheading "Preset tab bar"
            , Frame.presets Components.panel
            ]
        , Playground.fromFrames { id = "panel-gallery", name = "Panel (preset gallery)" }
            [ Frame.subheading "All presets side-by-side"
            , Frame.presetGallery Components.panel
            ]
        , Playground.fromFrames { id = "dashboard", name = "Dashboard (embeds Panel)" }
            [ Frame.subheading "Panel preset picker travels into the controls pane"
            , Frame.fromComponent Components.dashboard
            ]
        ]
    , Playground.group { id = "frame-types", name = "Frame Types" }
        [ Playground.fromFrames { id = "wrapped-explore", name = "fromComponent with wrap" }
            [ Frame.fromComponent Components.textField
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
        , Playground.fromFrames { id = "wrapped-example", name = "example with wrap" }
            [ Frame.subheading "Pre-filled, framed"
            , Components.textField
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
        , Playground.group { name = "Gallery", id = "gallery" }
            [ Playground.fromFrames { id = "frame", name = "Text field variants" }
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
            , Playground.fromFrames { id = "frame-mapped", name = "Content block variants" }
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
