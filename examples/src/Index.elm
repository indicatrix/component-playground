module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Controls
import Html
import Html.Events



-- TEXT FIELD


type alias TextFieldModel =
    { value : String
    , label : String
    , id : String
    , error : String
    }


textField : Component.Component e t TextFieldModel msg
textField =
    { id = "text-field"
    , name = "Text field"
    , controls =
        Controls.builder TextFieldModel
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.add "Error" .error Controls.string
            |> Controls.toControls
    , view =
        Component.view <|
            \model setter ->
                let
                    e =
                        if model.error == "" then
                            Nothing

                        else
                            Just model.error
                in
                UI.textField
                    { msg = \v -> setter { model | value = v }
                    , label = model.label
                    , id = model.id
                    , value = model.value
                    , error = e
                    }
    }



-- DROPDOWN INPUT


type alias DropdownModel =
    { label : String
    , value : String
    , options : List { label : String, value : String }
    , id : String
    }


dropdownInput : Component.Component e t DropdownModel msg
dropdownInput =
    let
        optionControls =
            Controls.builder (\label value -> { label = label, value = value })
                |> Controls.add "Label" .label Controls.string
                |> Controls.add "Value" .value Controls.string
                |> Controls.toControls
    in
    { id = "dropdown-input"
    , name = "Simple Dropdown Input"
    , controls =
        Controls.builder DropdownModel
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Options" .options (Controls.list optionControls)
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.toControls
    , view =
        Component.view <|
            \model setter ->
                UI.select
                    { id = model.id
                    , label = model.label
                    , options = model.options
                    , value = model.value
                    , msg = \v -> setter { model | value = v }
                    }
    }



-- INT INPUT


intInput : Component.Component e t Int msg
intInput =
    { id = "int-input"
    , name = "Int Input"
    , controls = Controls.int |> Controls.withDefault 5
    , view =
        Component.view <|
            \value _ ->
                Html.div [] [ Html.text ("Int value: " ++ String.fromInt value) ]
    }



-- FLOAT INPUT


floatInput : Component.Component e t Float msg
floatInput =
    { id = "float-input"
    , name = "Float Input"
    , controls = Controls.float |> Controls.withDefault 0.5
    , view =
        Component.view <|
            \value _ ->
                Html.div [] [ Html.text ("Float value: " ++ String.fromFloat value) ]
    }



-- Identifier Test


identifierTest : Component.Component e t ( String, String, String ) msg
identifierTest =
    { id = "test-1"
    , name = "Test 1"
    , controls =
        Controls.builder (\a b c -> ( a, b, c ))
            |> Controls.add "Unlabelled 1" (\( x, _, _ ) -> x) Controls.identifier
            |> Controls.add "Unlabelled 2" (\( _, x, _ ) -> x) Controls.identifier
            |> Controls.add "Unlabelled 3" (\( _, _, x ) -> x) Controls.identifier
            |> Controls.toControls
    , view =
        Component.view <|
            \( a, b, c ) msg ->
                UI.vStack []
                    [ Html.div [] [ UI.text [] [ Html.text a ] ]
                    , Html.div [] [ UI.text [] [ Html.text b ] ]
                    , Html.div [] [ UI.text [] [ Html.text c ] ]
                    , Html.div []
                        [ UI.button [ Html.Events.onClick (msg ( a, b, c )) ]
                            [ Html.text "Test button" ]
                        ]
                    ]
    }



-- TEST 2


test2 : Component.Component e t ( String, String ) msg
test2 =
    { id = "test-2"
    , name = "Test 2"
    , controls =
        Controls.builder Tuple.pair
            |> Controls.add "Unlabelled 1" Tuple.first Controls.identifier
            |> Controls.add "Unlabelled 2" Tuple.second Controls.identifier
            |> Controls.toControls
    , view =
        Component.view <|
            \( a, b ) _ ->
                UI.vStack []
                    [ Html.div [] [ UI.text [] [ Html.text a ] ]
                    , Html.div [] [ UI.text [] [ Html.text b ] ]
                    ]
    }



-- LIST TEST


listTest : Component.Component e t (List String) msg
listTest =
    { id = "list-test"
    , name = "List test"
    , controls =
        Controls.list Controls.string |> Controls.withDefault [ "One", "Two", "Three" ]
    , view =
        Component.view <|
            \value _ ->
                UI.text [] [ Html.text (String.join ", " value) ]
    }



-- MAIN


main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element
        [ Component.group { id = "components", name = "Components" }
            [ Component.playground { id = "text-field", name = "Text field" }
                [ Component.explore textField ]
            , Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
                [ Component.explore dropdownInput ]
            , Component.playground { id = "test-1", name = "Test 1" }
                [ Component.explore identifierTest ]
            , Component.playground { id = "test-2", name = "Test 2" }
                [ Component.explore test2 ]
            , Component.playground { id = "int-input", name = "Int Input" }
                [ Component.explore intInput ]
            , Component.playground { id = "float-input", name = "Float Input" }
                [ Component.explore floatInput ]
            , Component.playground { id = "list-test", name = "List test" }
                [ Component.explore listTest ]
            ]
        ]
        Nothing
