port module Index exposing (main)

import Browser
import Component
import Component.Application
import Component.Application.Theme as Theme
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
    [ Component.group { id = "components", name = "Components" }
        [ Component.playground { id = "text-field", name = "Text field" }
            [ Component.explore Components.textField ]
        , Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
            [ Component.explore Components.dropdownInput ]
        , Component.playground { id = "test-1", name = "Test 1" }
            [ Component.explore Components.identifierTest ]
        , Component.playground { id = "int-input", name = "Int Input" }
            [ Component.explore Components.intInput ]
        , Component.playground { id = "float-input", name = "Float Input" }
            [ Component.explore Components.floatInput ]
        , Component.playground { id = "list-test", name = "List test" }
            [ Component.explore Components.listTest ]
        , Component.playground { id = "combo-element", name = "Combination Element" }
            [ Component.explore Components.comboElement ]
        , Component.playground { id = "content-block", name = "Content Block (Sum Type)" }
            [ Component.explore Components.contentBlock ]
        ]
    , Component.group { id = "frame-types", name = "Frame Types" }
        [ Component.playground { id = "explore-frame", name = "exploreFrame (with wrapper)" }
            [ Component.exploreFrame
                (\inner ->
                    Html.div
                        [ Html.Attributes.style "background-color" "#1a1a2e"
                        , Html.Attributes.style "padding" "32px"
                        , Html.Attributes.style "border-radius" "8px"
                        ]
                        [ inner ]
                )
                Components.textField
            ]
        , Component.playground { id = "gallery-frame", name = "galleryFrame (text field variants)" }
            [ Component.galleryFrame "Text field states"
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
        , Component.playground { id = "gallery-frame-mapped", name = "galleryFrame_ (content block variants)" }
            [ Component.galleryFrame_ "Content block variants"
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
