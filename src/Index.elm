module Index exposing (main)

import Component
import Component.Application
import Component.UI as UI
import Html


main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.playground
        [ Component.preview "test-1"
            { name = "Test 1" }
            (\a b c ->
                UI.vStack []
                    [ Html.div [] [ UI.text [] [ Html.text a ] ]
                    , Html.div [] [ UI.text [] [ Html.text b ] ]
                    , Html.div [] [ UI.text [] [ Html.text c ] ]
                    ]
            )
            |> Component.withUnlabelled Component.identifier
            |> Component.withUnlabelled Component.identifier
            |> Component.withUnlabelled Component.identifier
        , Component.preview "test-2"
            { name = "Test 2" }
            (\a b ->
                UI.vStack []
                    [ Html.div [] [ UI.text [] [ Html.text a ] ]
                    , Html.div [] [ UI.text [] [ Html.text b ] ]
                    ]
            )
            |> Component.withUnlabelled Component.identifier
            |> Component.withUnlabelled Component.identifier
        , Component.preview "text-field"
            { name = "Text field" }
            (\s msg l i ->
                UI.textField { msg = msg, label = l, id = i, value = s }
            )
            |> Component.withState "Value" Component.string
            |> Component.withControl "Label" Component.string
            |> Component.withUnlabelled Component.identifier
        , Component.preview "spy"
            { name = "Spy" }
            (\i s ->
                UI.hStack []
                    [ Html.div [] [ Html.text i ]
                    , Html.div [] [ Html.text s ]
                    ]
            )
            |> Component.withUnlabelled Component.identifier
            |> Component.withControl "Value" Component.string
        , Component.preview "list-test"
            { name = "List test" }
            (\ll ->
                UI.text [] [ Html.text <| String.join ", " ll ]
            )
            |> Component.withControl "Contents" (Component.list Component.string)
        , Component.preview "dropdown-input"
            { name = "Simple Dropdown Input" }
            (\label options selected msg i ->
                UI.select
                    { id = i
                    , label = label
                    , options = options
                    , value = selected
                    , msg = msg
                    }
            )
            |> Component.withControl "Label" Component.string
            |> Component.withControl "Options"
                (Component.build (\label value -> { label = label, value = value })
                    |> Component.add "Label" Component.string
                    |> Component.add "Value" Component.string
                    |> Component.finish
                    |> Component.list
                )
            |> Component.withState "Value" Component.string
            |> Component.withUnlabelled Component.identifier
        , Component.preview "combo-element"
            { name = "Combination Element" }
            (\title inner innerList ->
                UI.vStack [ UI.style "gap" "8px" ]
                    ([ UI.text [] [ Html.text title ]
                     , inner
                     ]
                        ++ innerList
                    )
            )
            |> Component.withControl "Title" Component.string
            |> Component.withSubcomponent "Element" Component.subcomponent
            |> Component.withSubcomponent "Element list" (Component.list2 Component.subcomponent)
        ]
        Sub.none
