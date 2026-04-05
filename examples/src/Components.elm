module Components exposing
    ( ComboStorage
    , ComboView
    , DropdownModel
    , TextFieldModel
    , comboElement
    , dropdownInput
    , floatInput
    , identifierTest
    , intInput
    , listTest
    , textField
    )

import Component
import Component.Control as Control
import Component.UI as UI
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
    Component.component
        { id = "text-field"
        , name = "Text field"
        , controls =
            Control.builder TextFieldModel
                |> Control.add "Value" .value Control.string
                |> Control.add "Label" .label Control.string
                |> Control.add "Id" .id Control.identifier
                |> Control.add "Error" .error Control.string
                |> Control.toControl
                |> Control.withDefault { id = "not used", label = "Label", value = "Value", error = "" }
        , view =
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
            Control.builder (\label value -> { label = label, value = value })
                |> Control.add "Label" .label Control.string
                |> Control.add "Value" .value Control.string
                |> Control.toControl
    in
    Component.component
        { id = "dropdown-input"
        , name = "Simple Dropdown Input"
        , controls =
            Control.builder DropdownModel
                |> Control.add "Label" .label Control.string
                |> Control.add "Value" .value Control.string
                |> Control.add "Options" .options (Control.list optionControls)
                |> Control.add "Id" .id Control.identifier
                |> Control.toControl
                |> Control.withDefault
                    { label = "Label"
                    , value = "2"
                    , options =
                        [ { label = "One", value = "1" }
                        , { label = "Two", value = "2" }
                        , { label = "Three", value = "3" }
                        ]
                    , id = "not used"
                    }
        , view =
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
    Component.component
        { id = "int-input"
        , name = "Int Input"
        , controls = Control.int |> Control.withDefault 5
        , view =
            \value _ ->
                Html.div [] [ Html.text ("Int value: " ++ String.fromInt value) ]
        }



-- FLOAT INPUT


floatInput : Component.Component e t Float msg
floatInput =
    Component.component
        { id = "float-input"
        , name = "Float Input"
        , controls = Control.float |> Control.withDefault 0.5
        , view =
            \value _ ->
                Html.div [] [ Html.text ("Float value: " ++ String.fromFloat value) ]
        }



-- IDENTIFIER TEST


identifierTest : Component.Component e t ( String, String, String ) msg
identifierTest =
    Component.component
        { id = "test-1"
        , name = "Test 1"
        , controls =
            Control.builder (\a b c -> ( a, b, c ))
                |> Control.add "Unlabelled 1" (\( x, _, _ ) -> x) Control.identifier
                |> Control.add "Unlabelled 2" (\( _, x, _ ) -> x) Control.identifier
                |> Control.add "Unlabelled 3" (\( _, _, x ) -> x) Control.identifier
                |> Control.toControl
        , view =
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



-- LIST TEST


listTest : Component.Component e t (List String) msg
listTest =
    Component.component
        { id = "list-test"
        , name = "List test"
        , controls = Control.list Control.string |> Control.withDefault [ "One", "Two", "Three" ]
        , view =
            \value _ ->
                UI.text [] [ Html.text (String.join ", " value) ]
        }



-- COMBINATION ELEMENT


type alias ComboStorage =
    { title : String
    , inner : String
    , innerList : List String
    }


type alias ComboView =
    { title : String
    , inner : Html.Html (Component.Update () ())
    , innerList : List (Html.Html (Component.Update () ()))
    }


comboElement : Component.Component_ () () ComboStorage ComboView (Component.Update () ())
comboElement =
    Component.component_
        { id = "combo-element"
        , name = "Combination Element"
        , controls =
            Control.builder
                (\title inner renderInner innerList renderInnerList ->
                    ( { title = title, inner = inner, innerList = innerList }
                    , \_ s ->
                        { title = s.title
                        , inner = renderInner s.inner
                        , innerList = renderInnerList s.innerList
                        }
                    )
                )
                |> Control.add "Title" .title (Control.string |> Control.withDefault "Title")
                |> Control.add_ "Element" .inner Control.componentRef
                |> Control.add_ "Element list"
                    .innerList
                    (Control.list Control.componentRef
                        |> Control.withDefault
                            [ Component.toRef textField
                            , Component.toRef dropdownInput
                            ]
                    )
                |> Control.toControl_
        , view =
            \_ model _ ->
                UI.vStack [ UI.style "gap" "8px" ]
                    ([ UI.text [] [ Html.text model.title ]
                     , model.inner
                     ]
                        ++ model.innerList
                    )
        }
