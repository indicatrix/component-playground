module Index exposing (main)

import Component
import Component.Application
import Components


main : Component.Application.ComponentPlayground () ()
main =
    Component.Application.element
        [ Component.group { id = "components", name = "Components" }
            [ Component.playground { id = "text-field", name = "Text field" }
                [ Component.explore Components.textField ]
            , Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
                [ Component.explore Components.dropdownInput ]
            , Component.playground { id = "test-1", name = "Test 1" }
                [ Component.explore Components.identifierTest ]
            , Component.playground { id = "test-2", name = "Test 2" }
                [ Component.explore Components.test2 ]
            , Component.playground { id = "int-input", name = "Int Input" }
                [ Component.explore Components.intInput ]
            , Component.playground { id = "float-input", name = "Float Input" }
                [ Component.explore Components.floatInput ]
            , Component.playground { id = "list-test", name = "List test" }
                [ Component.explore Components.listTest ]
            , Component.playground { id = "combo-element", name = "Combination Element" }
                [ Component.explore Components.comboElement ]
            ]
        ]
        Nothing
