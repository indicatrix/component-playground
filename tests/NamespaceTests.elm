module NamespaceTests exposing (suite)

import Component
import Component.Application as Application
import Component.Application.Theme as Theme
import Component.Control as Control
import Component.Frame as Frame
import Component.Internal as Internal
import Component.Playground as Playground
import Components
import Dict
import Expect
import Html
import Html.Attributes
import Html.Events
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Component namespaces"
        [ slugTests
        , uuidLookupTests
        , componentRefWithinTests
        , embeddedLibraryTests
        , treeErrorTests
        ]



-- SLUG


slugTests : Test
slugTests =
    let
        slugs name expected =
            Test.test (name ++ " → " ++ expected) <|
                \_ -> Internal.slug name |> Expect.equal expected
    in
    Test.describe "slug"
        [ slugs "Progress & Effects" "progress-effects"
        , slugs "Text field" "text-field"
        , slugs "Content Block (Sum Type)" "content-block-sum-type"
        , slugs "  Leading and trailing  " "leading-and-trailing"
        , slugs "a -- b" "a-b"
        , slugs "-dashes-" "dashes"
        , slugs "V2.1" "v21"
        , slugs "Café" "caf"
        ]



-- UUID LOOKUP


{-| A counter whose updates report which counter ran them.
-}
counter : String -> Component.Component String t Int msg
counter tag =
    Component.component
        { uuid = "counter-" ++ tag
        , name = "Counter"
        , controls =
            Control.int
                |> Control.withDefault 0
                |> Control.withUpdate (\_ _ _ new -> ( new, [ tag ++ " ran " ++ String.fromInt new ] ))
        , view =
            \count setter ->
                Html.button [ Html.Attributes.id "increment", Html.Events.onClick (setter (count + 1)) ]
                    [ Html.text ("Count: " ++ String.fromInt count) ]
        }


uuidLookupTests : Test
uuidLookupTests =
    let
        playgrounds =
            [ Playground.group { uuid = "group-a", name = "A" } [ Playground.fromComponent (counter "a") ]
            , Playground.group { uuid = "group-b", name = "B" } [ Playground.fromComponent (counter "b") ]
            ]

        clickOn page =
            let
                model =
                    Application.init Theme.default playgrounds
                        |> Application.setLocation { page = page, preset = Nothing }
                        |> Tuple.first
            in
            Application.view model
                |> Query.fromHtml
                |> Query.find [ Selector.id "increment" ]
                |> Event.simulate Event.click
                |> Event.toResult
                |> Result.map (\msg -> Application.update msg model)
    in
    Test.describe "two components with the same name in different groups"
        [ Test.test "have their own pages" <|
            \_ ->
                Application.init Theme.default playgrounds
                    |> .pages
                    |> Dict.keys
                    |> Expect.equal [ "a/counter", "b/counter" ]
        , Test.test "the first receives its own updates" <|
            \_ -> clickOn "a/counter" |> expectOwnUpdate "a ran 1"
        , Test.test "the second receives its own updates" <|
            \_ -> clickOn "b/counter" |> expectOwnUpdate "b ran 1"
        ]


expectOwnUpdate : String -> Result String ( Application.Model t String, List String ) -> Expect.Expectation
expectOwnUpdate effect result =
    case result of
        Ok ( model, effects ) ->
            Expect.all
                [ \_ -> Expect.equal [ effect ] effects
                , \_ -> Application.view model |> Query.fromHtml |> Query.has [ Selector.text "Count: 1" ]
                ]
                ()

        Err err ->
            Expect.fail err



-- COMPONENT REF WITHIN


componentRefWithinTests : Test
componentRefWithinTests =
    let
        picker groupUuid =
            Component.component_
                { uuid = "picker"
                , name = "Picker"
                , controls = Control.componentRefWithin groupUuid
                , view = \_ inner _ -> inner
                }

        playgrounds groupUuid =
            [ Playground.group { uuid = "e0b7c1d2-components", name = "Components" }
                [ Playground.fromComponent (picker groupUuid)
                , Playground.group { uuid = "9a4f3e10-inner", name = "Inner" }
                    [ Playground.fromComponent Components.textField
                    , Playground.group { uuid = "5d2c8b77-deeper", name = "Deeper" }
                        [ Playground.fromComponent Components.intInput ]
                    ]
                , Playground.fromComponent Components.dropdownInput
                ]
            ]

        view groupUuid =
            Application.init Theme.default (playgrounds groupUuid)
                |> Application.view
                |> Query.fromHtml

        options groupUuid =
            view groupUuid |> Query.findAll [ Selector.tag "option" ]

        offers name =
            Query.keep (Selector.text name) >> Query.count (Expect.equal 1)

        omits name =
            Query.keep (Selector.text name) >> Query.count (Expect.equal 0)
    in
    Test.describe "Control.componentRefWithin a group uuid"
        [ Test.test "offers components on pages in the group's subtree" <|
            \_ -> options "9a4f3e10-inner" |> Expect.all [ offers "Text field", offers "Int Input" ]
        , Test.test "leaves out components outside the group" <|
            \_ -> options "9a4f3e10-inner" |> omits "Simple Dropdown Input"
        , Test.test "a nested group narrows further" <|
            \_ -> options "5d2c8b77-deeper" |> Expect.all [ offers "Int Input", omits "Text field" ]
        , Test.test "the enclosing group leaves out the picker's own page" <|
            \_ -> options "e0b7c1d2-components" |> Expect.all [ offers "Simple Dropdown Input", omits "Picker" ]
        , Test.test "an unknown group uuid says so in the control" <|
            \_ ->
                view "missing"
                    |> Expect.all
                        [ Query.has [ Selector.text "Group not found: missing" ]
                        , Query.findAll [ Selector.tag "option" ] >> Query.keep (Selector.text "Text field") >> Query.count (Expect.equal 0)
                        ]
        ]



-- EMBEDDED COMPONENTS


embeddedLibraryTests : Test
embeddedLibraryTests =
    let
        refersTo uuid name =
            Component.component_
                { uuid = name
                , name = name
                , controls = Control.componentRef |> Control.withDefault (Internal.ComponentRef uuid)
                , view = \_ inner _ -> inner
                }

        view playgrounds =
            Application.init Theme.default playgrounds
                |> Application.view
                |> Query.fromHtml

        options =
            view
                [ Playground.fromComponent (refersTo "combo-element" "picker")
                , Playground.fromComponent Components.comboElement
                , Playground.fromComponent Components.textField
                ]
                |> Query.findAll [ Selector.tag "option" ]

        offered name expectation =
            Query.keep (Selector.text name) >> Query.count expectation
    in
    -- The picker's page embeds the combination element, whose own component
    -- refs leave out its own page and the picker's.
    Test.describe "an embedded component"
        [ Test.test "its refs offer other pages' components" <|
            \_ -> options |> offered "Text field" (Expect.atLeast 1)
        , Test.test "its refs leave out its own page" <|
            \_ -> options |> offered "Combination Element" (Expect.equal 1)
        , Test.test "its refs leave out the page embedding it" <|
            \_ -> options |> offered "picker" (Expect.equal 0)
        , Test.test "a ref back to an embedding page says so" <|
            \_ ->
                view
                    [ Playground.fromComponent (refersTo "loop-b" "loop-a")
                    , Playground.fromComponent (refersTo "loop-a" "loop-b")
                    ]
                    |> Query.has [ Selector.text "loop-a can't be embedded here: it would embed itself" ]
        , Test.test "an unknown component uuid says so" <|
            \_ ->
                view [ Playground.fromComponent (refersTo "missing" "picker") ]
                    |> Query.has [ Selector.text "Component not found: missing" ]
        ]



-- TREE ERRORS


treeErrorTests : Test
treeErrorTests =
    let
        errors playgrounds =
            (Application.init Theme.default playgrounds).treeErrors
    in
    Test.describe "tree errors"
        [ Test.test "a well-formed tree has none" <|
            \_ ->
                errors
                    [ Playground.group { uuid = "g1", name = "Inputs" }
                        [ Playground.fromComponent Components.textField
                        , Playground.fromComponent Components.intInput
                        ]
                    , Playground.group { uuid = "g2", name = "Layout" }
                        [ Playground.fromFrames { name = "Text field" } [ Frame.fromComponent (Components.textFieldWith { uuid = "text-field-2", name = "Text field" }) ] ]
                    ]
                    |> Expect.equal []
        , Test.test "a component uuid on two pages" <|
            \_ ->
                errors
                    [ Playground.fromComponent Components.intInput
                    , Playground.fromFrames { name = "Again" } [ Frame.fromComponent Components.intInput ]
                    ]
                    |> Expect.equal
                        [ "The uuid \"int-input\" is used more than once: component \"Int Input\" on int-input, component \"Int Input\" on again" ]
        , Test.test "one component in two frames" <|
            \_ ->
                errors
                    [ Playground.fromFrames { name = "Ints" }
                        [ Frame.fromComponent Components.intInput, Frame.presets Components.intInput ]
                    ]
                    |> Expect.equal
                        [ "The uuid \"int-input\" is used more than once: component \"Int Input\" on ints, component \"Int Input\" on ints" ]
        , Test.test "a group uuid used twice" <|
            \_ ->
                errors
                    [ Playground.group { uuid = "same", name = "One" } []
                    , Playground.group { uuid = "same", name = "Two" } []
                    ]
                    |> Expect.equal
                        [ "The uuid \"same\" is used more than once: group \"One\" at one, group \"Two\" at two" ]
        , Test.test "a group and a component sharing a uuid" <|
            \_ ->
                errors
                    [ Playground.group { uuid = "int-input", name = "Numbers" } [ Playground.fromComponent Components.intInput ] ]
                    |> Expect.equal
                        [ "The uuid \"int-input\" is used more than once: group \"Numbers\" at numbers, component \"Int Input\" on numbers/int-input" ]
        , Test.test "siblings whose names slug the same" <|
            \_ ->
                errors
                    [ Playground.group { uuid = "g", name = "Demo" }
                        [ Playground.fromFrames { name = "Progress & Effects" } []
                        , Playground.group { uuid = "h", name = "progress effects" } []
                        ]
                    ]
                    |> Expect.equal
                        [ "The names \"Progress & Effects\", \"progress effects\" share the path demo/progress-effects" ]
        , Test.test "the same name under different parents is fine" <|
            \_ ->
                errors
                    [ Playground.group { uuid = "g1", name = "A" } [ Playground.fromFrames { name = "Same" } [] ]
                    , Playground.group { uuid = "g2", name = "B" } [ Playground.fromFrames { name = "Same" } [] ]
                    ]
                    |> Expect.equal []
        , Test.test "the sidebar lists them" <|
            \_ ->
                Application.init Theme.default
                    [ Playground.group { uuid = "same", name = "One" } []
                    , Playground.group { uuid = "same", name = "Two" } []
                    ]
                    |> Application.view
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "cp-tree-errors" ]
                    |> Query.has [ Selector.text "The uuid \"same\" is used more than once" ]
        ]
