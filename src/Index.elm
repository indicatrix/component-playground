module Index exposing (main)

import Component.Application
import Component.Block as Component
import Component.Preview as Component
import Component.UI as UI
import Html


main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.playground
        [ Component.preview "text-field"
            { name = "Text field" }
            (\s msg l i ->
                UI.textField { msg = msg, label = l, id = i, value = s }
            )
            |> Component.withState "Value" Component.string
            |> Component.withControl "Label" Component.string
            |> Component.withAnonymous Component.identifier
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
                    , value = Debug.log "Selected" selected
                    , msg = msg
                    }
            )
            |> Component.withControl "Label" Component.string
            |> Component.withControl "Options"
                (Component.build (\label value -> { label = label, value = value })
                    |> Component.andMap "Label" Component.string
                    |> Component.andMap "Value" Component.string
                    |> Component.finish
                    |> Component.list
                )
            |> Component.withState "Value" Component.string
            |> Component.withAnonymous Component.identifier
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
