module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Controls
import Html
import Html.Events


type alias Preview =
    Component.Preview () ()


textFieldPreview : Preview
textFieldPreview =
    Component.new
        (\s msg l i err ->
            let
                e =
                    if err == "" then
                        Nothing

                    else
                        Just err
            in
            UI.textField { msg = msg, label = l, id = i, value = s, error = e }
        )
        |> Component.withState_ "Value" Controls.string
        |> Component.withControl "Label" Controls.string "Label"
        |> Component.withUnlabelled_ Controls.identifier
        |> Component.withControl "Error" Controls.string ""
        |> Component.toPreview { id = "text-field", name = "Text field" }


dropdownInputPreview : Preview
dropdownInputPreview =
    Component.new
        (\label selected msg options i ->
            UI.select
                { id = i
                , label = label
                , options = options
                , value = selected
                , msg = msg
                }
        )
        |> Component.withControl "Label" Controls.string "Label"
        |> Component.withState "Value" Controls.string "2"
        |> Component.withControl "Options"
            (Controls.list
                (Controls.builder (\label value -> { label = label, value = value })
                    |> Controls.add "Label" .label Controls.string
                    |> Controls.add "Value" .value Controls.string
                    |> Controls.toControls
                )
            )
            [ { label = "One", value = "1" }
            , { label = "Two", value = "2" }
            , { label = "Three", value = "3" }
            ]
        |> Component.withUnlabelled_ Controls.identifier
        |> Component.toPreview { id = "dropdown-input", name = "Simple Dropdown Input" }


main : Component.Application.ComponentPlayground () ()
main =
    let
        previews : List Preview
        previews =
            [ textFieldPreview
            , dropdownInputPreview
            , Component.new
                (\a b c msg ->
                    UI.vStack []
                        [ Html.div [] [ UI.text [] [ Html.text a ] ]
                        , Html.div [] [ UI.text [] [ Html.text b ] ]
                        , Html.div [] [ UI.text [] [ Html.text c ] ]
                        , Html.div [] [ UI.button [ Html.Events.onClick (msg ()) ] [ Html.text "Test button" ] ]
                        ]
                )
                |> Component.withUnlabelled_ Controls.identifier
                |> Component.withUnlabelled_ Controls.identifier
                |> Component.withUnlabelled_ Controls.identifier
                |> Component.withMsg identity
                |> Component.toPreview { id = "test-1", name = "Test 1" }
            , Component.new
                (\a b ->
                    UI.vStack []
                        [ Html.div [] [ UI.text [] [ Html.text a ] ]
                        , Html.div [] [ UI.text [] [ Html.text b ] ]
                        ]
                )
                |> Component.withUnlabelled_ Controls.identifier
                |> Component.withUnlabelled_ Controls.identifier
                |> Component.toPreview { id = "test-2", name = "Test 2" }
            , Component.new
                (\a ->
                    Html.div [] [ Html.text <| "Int value: " ++ String.fromInt a ]
                )
                |> Component.withControl "Int Value" Controls.int 5
                |> Component.toPreview { id = "int-input", name = "Int Input" }
            , Component.new
                (\a ->
                    Html.div [] [ Html.text <| "Float value: " ++ String.fromFloat a ]
                )
                |> Component.withControl "Float Value" Controls.float 0.5
                |> Component.toPreview { id = "float-input", name = "Float Input" }
            , Component.new
                (\ll ->
                    UI.text [] [ Html.text <| String.join ", " ll ]
                )
                |> Component.withControl "Contents"
                    (Controls.list Controls.string)
                    [ "One", "Two", "Three" ]
                |> Component.toPreview { id = "list-test", name = "List test" }
            , Component.new
                (\title inner innerList ->
                    UI.vStack [ UI.style "gap" "8px" ]
                        ([ UI.text [] [ Html.text title ]
                         , inner
                         ]
                            ++ innerList
                        )
                )
                |> Component.withControl "Title" Controls.string "Title"
                |> Component.withControl_ "Element" Component.previewBlock
                |> Component.withControl "Element list"
                    (Component.list Component.previewBlock)
                    [ Component.fromPreview textFieldPreview, Component.fromPreview dropdownInputPreview ]
                |> Component.toPreview { id = "combo-element", name = "Combination Element" }
            ]
    in
    Component.Application.element [ Component.group "Components" previews ] Nothing
