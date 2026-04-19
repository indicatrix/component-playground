module Components exposing
    ( ComboStorage
    , ComboView
    , DashboardStorage
    , DashboardView
    , DropdownModel
    , TextFieldModel
    , comboElement
    , contentBlock
    , dashboard
    , dropdownInput
    , floatInput
    , identifierTest
    , intInput
    , listTest
    , panel
    , textField
    )

import Component
import Component.Application.Theme as Theme
import Component.Control as Control
import Component.Ui as Ui
import Html
import Html.Attributes
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
                Ui.textField Theme.default
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
                Ui.select Theme.default
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
                Ui.vStack []
                    [ Html.div [] [ Ui.text Theme.default [] [ Html.text a ] ]
                    , Html.div [] [ Ui.text Theme.default [] [ Html.text b ] ]
                    , Html.div [] [ Ui.text Theme.default [] [ Html.text c ] ]
                    , Html.div []
                        [ Ui.button Theme.default
                            [ Html.Events.onClick (msg ( a, b, c )) ]
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
                Ui.text Theme.default [] [ Html.text (String.join ", " value) ]
        }



-- COMBINATION ELEMENT


type alias ComboStorage =
    { title : String
    , inner : Component.ComponentRef
    , innerList : List Component.ComponentRef
    }


type alias ComboView =
    { title : String
    , inner : Html.Html (Component.Update ())
    , innerList : List (Html.Html (Component.Update ()))
    }


comboElement : Component.Component_ () () ComboStorage ComboView (Component.Update ())
comboElement =
    Component.component_
        { id = "combo-element"
        , name = "Combination Element"
        , controls =
            Control.builder
                (\title inner innerList ->
                    { state = { title = title, inner = inner.state, innerList = innerList.state }
                    , toValue =
                        \s ->
                            { title = s.title
                            , inner = inner.toValue s.inner
                            , innerList = innerList.toValue s.innerList
                            }
                    }
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
                Ui.vStack [ Ui.style "gap" "8px" ]
                    ([ Ui.text Theme.default [] [ Html.text model.title ]
                     , model.inner
                     ]
                        ++ model.innerList
                    )
        }



-- SUM TYPE EXAMPLE (conditional rendering)


type ContentBlock
    = TextContent String
    | NumberContent Int
    | ToggleContent Bool


type alias ContentBlockStorage =
    { kind : String
    , text : String
    , number : Int
    , toggle : Bool
    }


contentBlock : Component.Component_ () () ContentBlockStorage ContentBlock (Component.Update ())
contentBlock =
    Component.component_
        { id = "content-block"
        , name = "Content Block (Sum Type)"
        , controls =
            Control.builder
                (\kind text number toggle ->
                    { state = ContentBlockStorage kind text number toggle
                    , toValue =
                        \s ->
                            case s.kind of
                                "text" ->
                                    TextContent s.text

                                "number" ->
                                    NumberContent s.number

                                _ ->
                                    ToggleContent s.toggle
                    }
                )
                |> Control.add "Kind"
                    .kind
                    (Control.fromOptions "Kind"
                        ( "text", "Text" )
                        [ ( "number", "Number" )
                        , ( "toggle", "Toggle" )
                        ]
                    )
                |> Control.addWhen (\s -> s.kind == "text") "Text" .text Control.string
                |> Control.addWhen (\s -> s.kind == "number") "Number" .number Control.int
                |> Control.addWhen (\s -> s.kind == "toggle") "Enabled" .toggle Control.bool
                |> Control.toControl_
        , view =
            \_ model _ ->
                case model of
                    TextContent text ->
                        Html.div []
                            [ Ui.text Theme.default [] [ Html.text ("Text: " ++ text) ] ]

                    NumberContent n ->
                        Html.div []
                            [ Ui.text Theme.default [] [ Html.text ("Number: " ++ String.fromInt n) ] ]

                    ToggleContent on ->
                        Html.div []
                            [ Ui.text Theme.default
                                []
                                [ Html.text
                                    ("Toggle: "
                                        ++ (if on then
                                                "ON"

                                            else
                                                "OFF"
                                           )
                                    )
                                ]
                            ]
        }



-- PANEL (component with presets)


type alias PanelStorage =
    { color : String
    , label : String
    , body : String
    }


panel : Component.Component e t PanelStorage msg
panel =
    Component.component
        { id = "panel"
        , name = "Panel"
        , controls =
            Control.builder PanelStorage
                |> Control.add "Color" .color Control.string
                |> Control.add "Label" .label Control.string
                |> Control.add "Body" .body Control.string
                |> Control.toControl
        , view =
            \m _ ->
                Html.div
                    [ Html.Attributes.style "background-color" m.color
                    , Html.Attributes.style "padding" "16px"
                    , Html.Attributes.style "border-radius" "6px"
                    , Html.Attributes.style "color" "white"
                    , Html.Attributes.style "font-family" "sans-serif"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "font-weight" "600"
                        , Html.Attributes.style "margin-bottom" "4px"
                        ]
                        [ Html.text m.label ]
                    , Html.text m.body
                    ]
        }
        |> Component.withPresets
            [ Component.preset "Info"
                { color = "#0b74de", label = "Info", body = "Helpful context goes here." }
            , Component.preset "Warning"
                { color = "#d97706", label = "Warning", body = "Something needs attention." }
            , { name = "Error"
              , value = { color = "#b91c1c", label = "Error", body = "Something went wrong." }
              , wrap =
                    \inner ->
                        Html.div
                            [ Html.Attributes.style "outline" "3px solid #fca5a5"
                            , Html.Attributes.style "outline-offset" "4px"
                            ]
                            [ inner ]
              }
            ]



-- DASHBOARD (embeds Panel via componentRef)


type alias DashboardStorage =
    { title : String
    , panel : Component.ComponentRef
    }


type alias DashboardView =
    { title : String
    , panel : Html.Html (Component.Update ())
    }


dashboard : Component.Component_ () () DashboardStorage DashboardView (Component.Update ())
dashboard =
    Component.component_
        { id = "dashboard"
        , name = "Dashboard"
        , controls =
            Control.builder
                (\title p ->
                    { state = { title = title, panel = p.state }
                    , toValue =
                        \s ->
                            { title = s.title
                            , panel = p.toValue s.panel
                            }
                    }
                )
                |> Control.add "Title" .title (Control.string |> Control.withDefault "Dashboard")
                |> Control.add_ "Panel"
                    .panel
                    (Control.componentRef
                        |> Control.withDefault (Component.toRef panel)
                    )
                |> Control.toControl_
        , view =
            \_ model _ ->
                Ui.vStack
                    [ Ui.style "gap" "12px"
                    , Ui.style "padding" "16px"
                    , Ui.style "border" "1px solid #ddd"
                    , Ui.style "border-radius" "8px"
                    ]
                    [ Ui.text Theme.default
                        [ Ui.style "font-weight" "600"
                        , Ui.style "font-size" "16px"
                        ]
                        [ Html.text model.title ]
                    , model.panel
                    ]
        }
