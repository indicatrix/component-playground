module Components exposing
    ( ComboModel
    , DropdownModel
    , TextFieldModel
    , comboElement
    , intInput
    , textField
    )

import Component
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


textField : Component.Component e t TextFieldModel msg
textField =
    Component.component { id = "text-field", name = "Text field" }
        (Controls.builder TextFieldModel
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.add "Error" .error Controls.string
            |> Controls.toControls
            |> Controls.withDefault { id = "not used", label = "Label", value = "Value", error = "" }
        )
        (\model setter ->
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
        )



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
    Component.component { id = "dropdown-input", name = "Simple Dropdown Input" }
        (Controls.builder DropdownModel
            |> Controls.add "Label" .label Controls.string
            |> Controls.add "Value" .value Controls.string
            |> Controls.add "Options" .options (Controls.list optionControls)
            |> Controls.add "Id" .id Controls.identifier
            |> Controls.toControls
            |> Controls.withDefault
                { label = "Label"
                , value = "2"
                , options =
                    [ { label = "One", value = "1" }
                    , { label = "Two", value = "2" }
                    , { label = "Three", value = "3" }
                    ]
                , id = "not used"
                }
        )
        (\model setter ->
            UI.select
                { id = model.id
                , label = model.label
                , options = model.options
                , value = model.value
                , msg = \v -> setter { model | value = v }
                }
        )



-- INT INPUT


intInput : Component.Component e t Int msg
intInput =
    Component.component { id = "int-input", name = "Int Input" }
        (Controls.int |> Controls.withDefault 5)
        (\value _ ->
            Html.div [] [ Html.text ("Int value: " ++ String.fromInt value) ]
        )



-- COMBINATION ELEMENT


type alias ComboModel =
    { title : String
    , inner : Html.Html (Component.Update () ())
    , innerList : List (Html.Html (Component.Update () ()))
    }


comboElement : Component.Component () () ComboModel (Component.Update () ())
comboElement =
    Component.component { id = "combo-element", name = "Combination Element" }
        (Controls.builder ComboModel
            |> Controls.add "Title" .title (Controls.string |> Controls.withDefault "Title")
            |> Controls.addMapped "Element" Controls.componentRef
            |> Controls.addMapped "Element list"
                (Controls.listMapped Controls.componentRef
                    |> Controls.withDefaultMapped
                        [ Component.toRef textField
                        , Component.toRef dropdownInput
                        ]
                )
            |> Controls.toControls
        )
        (\model _ ->
            UI.vStack [ UI.style "gap" "8px" ]
                ([ UI.text [] [ Html.text model.title ]
                 , model.inner
                 ]
                    ++ model.innerList
                )
        )
