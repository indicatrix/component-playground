module Index exposing (main)

import Component.Block as Component
import Component.Library
import Component.Preview as Component
import Component.UI as UI
import Html


main : Component.Library.LibraryProgram () ()
main =
    Component.Library.libraryProgram
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
            (\label defaultText options selected msg i ->
                UI.select
                    { id = i
                    , label = label
                    , options = options
                    , defaultText = defaultText
                    , value =
                        if selected == "" then
                            Nothing

                        else
                            Just selected
                    , msg = Maybe.withDefault "" >> msg
                    }
            )
            |> Component.withControl "Label" Component.string
            |> Component.withControl "Default Text" Component.string
            |> Component.withControl "Options" (Component.list Component.string)
            |> Component.withState "Value" Component.string
            |> Component.withAnonymous Component.identifier
        ]
        Sub.none
