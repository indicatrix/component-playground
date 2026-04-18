port module Index exposing (main)

import Browser
import Component.Application
import Component.Application.Theme as Theme
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
        , subscriptions = \_ -> Sub.none
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


previews : List (Component.Application.Playground () ())
previews =
    [ Playground.group { id = "components", name = "Components" }
        [ Playground.fromComponent { id = "text-field", name = "Text field" } Components.textField
        , Playground.fromComponent { id = "dropdown-input", name = "Simple Dropdown Input" } Components.dropdownInput
        , Playground.fromComponent { id = "test-1", name = "Test 1" } Components.identifierTest
        , Playground.fromComponent { id = "int-input", name = "Int Input" } Components.intInput
        , Playground.fromComponent { id = "float-input", name = "Float Input" } Components.floatInput
        , Playground.fromComponent { id = "list-test", name = "List test" } Components.listTest
        , Playground.fromComponent { id = "combo-element", name = "Combination Element" } Components.comboElement
        , Playground.fromComponent { id = "content-block", name = "Content Block (Sum Type)" } Components.contentBlock
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
            [ Frame.example "Pre-filled, framed"
                { value = "Hello", label = "Name", id = "wex-1", error = "" }
                Components.textField
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
        , Playground.group { name = "Gallery", id= "gallery" }
            [ Playground.fromFrames { id = "frame", name = "Text field variants" }
                [ Frame.gallery "Text field states"
                    Components.textField
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
                [ Frame.gallery "Content block variants"
                    Components.contentBlock
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
