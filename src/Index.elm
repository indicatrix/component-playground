module Index exposing (main)

import Component
import Component.Library
import Component.Preview as Preview exposing (Preview)
import Component.UI as UI
import Html


main : Component.Library.LibraryProgram () ()
main =
    let
        a : Preview t (Preview.Msg t m) (Html.Html (Preview.Msg t m))
        a =
            Component.preview "text-field"
                { name = "Text field" }
                (\s msg l i ->
                    UI.textField { msg = msg, label = l, id = i, value = s }
                )
                |> Component.withState "Value" Component.string
                |> Component.withControl "Label" Component.string
                |> Component.withControl "Identifier" Component.string

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
