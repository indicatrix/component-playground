module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Controls
import Html



-- TEXT FIELD


type alias TextFieldModel =
    { value : String
    , label : String
    , id : String
    , error : String
    }


textField : Component.Component () () TextFieldModel (Component.Update () ())
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


dropdownInput : Component.Component () () DropdownModel (Component.Update () ())
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


type alias IntInputModel =
    { value : Int }


intInput : Component.Component () () IntInputModel (Component.Update () ())
intInput =
    { id = "int-input"
    , name = "Int Input"
    , controls =
        Controls.builder IntInputModel
            |> Controls.add "Int Value" .value Controls.int
            |> Controls.toControls
    , view =
        Component.view <|
            \model _ ->
                Html.div [] [ Html.text ("Int value: " ++ String.fromInt model.value) ]
    }



-- FLOAT INPUT


type alias FloatInputModel =
    { value : Float }


floatInput : Component.Component () () FloatInputModel (Component.Update () ())
floatInput =
    { id = "float-input"
    , name = "Float Input"
    , controls =
        Controls.builder FloatInputModel
            |> Controls.add "Float Value" .value Controls.float
            |> Controls.toControls
    , view =
        Component.view <|
            \model _ ->
                Html.div [] [ Html.text ("Float value: " ++ String.fromFloat model.value) ]
    }



-- LIST TEST


type alias ListTestModel =
    { contents : List String }


listTest : Component.Component () () ListTestModel (Component.Update () ())
listTest =
    { id = "list-test"
    , name = "List test"
    , controls =
        Controls.builder ListTestModel
            |> Controls.add "Contents" .contents (Controls.list Controls.string)
            |> Controls.toControls
    , view =
        Component.view <|
            \model _ ->
                UI.text [] [ Html.text (String.join ", " model.contents) ]
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
            , Component.playground { id = "int-input", name = "Int Input" }
                [ Component.explore intInput ]
            , Component.playground { id = "float-input", name = "Float Input" }
                [ Component.explore floatInput ]
            , Component.playground { id = "list-test", name = "List test" }
                [ Component.explore listTest ]
            ]
        ]
        Nothing
