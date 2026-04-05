port module Index exposing (main)

import Browser
import Component
import Component.Application
import Components
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
    ( Component.Application.init previews (Url.fromString urlString)
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


previews : List (Component.Application.Playground () () (Component.Application.ComponentUpdate () ()))
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
            [ Component.explore_ Components.comboElement ]
        , Component.playground { id = "content-block", name = "Content Block (Sum Type)" }
            [ Component.explore_ Components.contentBlock ]
        ]
    ]
