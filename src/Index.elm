module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Html
import Html.Events


type alias Preview =
    Component.Preview () (Component.Msg () ())


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
        |> Component.withState_ "Value" Component.string
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withUnlabelled_ Component.identifier
        |> Component.withControl "Error" Component.string ""
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
        |> Component.withControl "Label" Component.string "Label"
        |> Component.withState "Value" Component.string "2"
        |> Component.withControl "Options"
            (Component.build (\label value -> { label = label, value = value })
                |> Component.addVia .label "Label" Component.string
                |> Component.addVia .value "Value" Component.string
                |> Component.finish_
                |> Component.list
            )
            [ { label = "One", value = "1" }
            , { label = "Two", value = "2" }
            , { label = "Three", value = "3" }
            ]
        |> Component.withUnlabelled_ Component.identifier
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
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withMsg identity
                |> Component.toPreview { id = "test-1", name = "Test 1" }
            , Component.new
                (\a b ->
                    UI.vStack []
                        [ Html.div [] [ UI.text [] [ Html.text a ] ]
                        , Html.div [] [ UI.text [] [ Html.text b ] ]
                        ]
                )
                |> Component.withUnlabelled_ Component.identifier
                |> Component.withUnlabelled_ Component.identifier
                |> Component.toPreview { id = "test-2", name = "Test 2" }
            , Component.new
                (\a ->
                    Html.div [] [ Html.text <| "Int value: " ++ String.fromInt a ]
                )
                |> Component.withControl "Int Value" Component.int 5
                |> Component.toPreview { id = "int-input", name = "Int Input" }
            , Component.new
                (\a ->
                    Html.div [] [ Html.text <| "Float value: " ++ String.fromFloat a ]
                )
                |> Component.withControl "Float Value" Component.float 0.5
                |> Component.toPreview { id = "float-input", name = "Float Input" }
            , Component.new
                (\ll ->
                    UI.text [] [ Html.text <| String.join ", " ll ]
                )
                |> Component.withControl "Contents"
                    (Component.list Component.string)
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
                |> Component.withControl "Title" Component.string "Title"
                |> Component.withComponent_ "Element" Component.previewBlock
                |> Component.withComponent "Element list"
                    (Component.list2 Component.previewBlock)
                    [ Component.fromPreview textFieldPreview, Component.fromPreview dropdownInputPreview ]
                |> Component.toPreview { id = "combo-element", name = "Combination Element" }
            ]

    in
    Component.Application.element previews
