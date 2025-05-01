module Index exposing (main)

import Component.Block as Component
import Component.Library
import Component.Preview as Component
import Component.UI as UI
import Html


main : Component.Library.LibraryProgram () ()
main =
    let
        a =
            Component.preview "text-field"
                { name = "Text field" }
                (\s msg l i ->
                    UI.textField { msg = msg, label = l, id = i, value = s }
                )
                |> Component.withState "Value" Component.string
                |> Component.withControl "Label" Component.string
                |> Component.withAnonymous Component.identifier

        b =
            Component.preview "list-test"
                { name = "List test" }
                (\ll ->
                    UI.text [] [ Html.text <| String.join ", " ll ]
                )
                |> Component.withControl "Contents" (Component.list Component.string)
    in
    Component.Library.libraryProgram
        [ a, b ]
        Sub.none
