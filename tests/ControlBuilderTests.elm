module ControlBuilderTests exposing (suite)

import Component.Control as Control
import ControlTestHelper as Helper
import Expect
import Html
import Html.Attributes
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Control.builder"
        [ twoStringFieldsTest
        , mixedTypesTest
        , fieldsIndependentTest
        , withDefaultOverrideTest
        , toControlMappedTest
        , addMappedFieldTest
        , addWhenTest
        ]



-- TWO STRING FIELDS


twoStringFieldsTest : Test
twoStringFieldsTest =
    let
        b =
            Helper.run
                (Control.builder (\a b_ -> { a = a, b = b_ })
                    |> Control.add "A" .a Control.string
                    |> Control.add "B" .b Control.string
                    |> Control.toControl
                )
    in
    Test.describe "two string fields"
        [ Test.test "default is constructed from field defaults" <|
            \_ ->
                Expect.equal { a = "Value", b = "Value" } b.default
        , Test.test "roundtrip" <|
            \_ ->
                let
                    stored =
                        b.toType { a = "hello", b = "world" }

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal { a = "hello", b = "world" } result
        ]



-- MIXED TYPES


mixedTypesTest : Test
mixedTypesTest =
    let
        b =
            Helper.run
                (Control.builder (\name count enabled -> { name = name, count = count, enabled = enabled })
                    |> Control.add "Name" .name Control.string
                    |> Control.add "Count" .count Control.int
                    |> Control.add "Enabled" .enabled Control.bool
                    |> Control.toControl
                )
    in
    Test.describe "mixed types (string + int + bool)"
        [ Test.test "defaults from each primitive" <|
            \_ ->
                Expect.equal { name = "Value", count = 1, enabled = True } b.default
        , Test.test "roundtrip preserves all types" <|
            \_ ->
                let
                    input =
                        { name = "test", count = 99, enabled = False }

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        ]



-- FIELDS INDEPENDENT


fieldsIndependentTest : Test
fieldsIndependentTest =
    let
        b =
            Helper.run
                (Control.builder (\a b_ -> { a = a, b = b_ })
                    |> Control.add "A" .a Control.string
                    |> Control.add "B" .b Control.string
                    |> Control.toControl
                )
    in
    Test.describe "fields are independent"
        [ Test.test "changing one field doesn't affect the other" <|
            \_ ->
                let
                    stored =
                        b.toType { a = "changed", b = "original" }

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.all
                    [ \r -> Expect.equal "changed" r.a
                    , \r -> Expect.equal "original" r.b
                    ]
                    result
        ]



-- WITH DEFAULT OVERRIDE


withDefaultOverrideTest : Test
withDefaultOverrideTest =
    let
        b =
            Helper.run
                (Control.builder (\a b_ -> { a = a, b = b_ })
                    |> Control.add "A" .a Control.string
                    |> Control.add "B" .b Control.string
                    |> Control.toControl
                    |> Control.withDefault { a = "Hello", b = "World" }
                )
    in
    Test.describe "withDefault overrides composed default"
        [ Test.test "default is the override, not field defaults" <|
            \_ ->
                Expect.equal { a = "Hello", b = "World" } b.default
        , Test.test "fromType with empty lookup returns override defaults" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (Helper.lookup [])
                in
                Expect.equal { a = "Hello", b = "World" } result
        ]



-- TOCONTROL_ (mapped builder finaliser)


toControlMappedTest : Test
toControlMappedTest =
    let
        -- Build a Maybe String control: storage is { has : Bool, val : String },
        -- output is Maybe String.
        b =
            Helper.run
                (Control.builder
                    (\has val ->
                        { state = { has = has, val = val }
                        , toValue = \s ->
                            if s.has then
                                Just s.val

                            else
                                Nothing
                        }
                    )
                    |> Control.add "Enabled" .has Control.bool
                    |> Control.add "Value" .val Control.string
                    |> Control.toControl_
                )
    in
    Test.describe "toControl_ (Maybe String)"
        [ Test.test "default storage has Bool default and String default" <|
            \_ ->
                Expect.equal { has = True, val = "Value" } b.default
        , Test.test "map produces Just when enabled" <|
            \_ ->
                Expect.equal (Just "Value")
                    (b.map (Helper.lookup []) b.default)
        , Test.test "map produces Nothing when disabled" <|
            \_ ->
                Expect.equal Nothing
                    (b.map (Helper.lookup []) { has = False, val = "Value" })
        , Test.test "roundtrip preserves storage" <|
            \_ ->
                let
                    input =
                        { has = False, val = "hello" }

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        ]



-- ADD_ (mapped field in builder)


addMappedFieldTest : Test
addMappedFieldTest =
    let
        -- Use add_ with fromLookup: stores String key, maps to Int.
        -- Constructor receives a { state, toValue } record for the mapped field.
        sizeControl =
            Control.fromLookup "" ( "sm", 10 ) [ ( "md", 20 ), ( "lg", 30 ) ]

        b =
            Helper.run
                (Control.builder
                    (\name size ->
                        { state = { name = name, sizeKey = size.state }
                        , toValue = \s -> { name = s.name, size = size.toValue s.sizeKey }
                        }
                    )
                    |> Control.add "Name" .name Control.string
                    |> Control.add_ "Size" .sizeKey sizeControl
                    |> Control.toControl_
                )
    in
    Test.describe "add_ with fromLookup"
        [ Test.test "default storage has string key" <|
            \_ ->
                Expect.equal "sm" b.default.sizeKey
        , Test.test "map produces mapped output" <|
            \_ ->
                Expect.equal { name = "Value", size = 10 }
                    (b.map (Helper.lookup []) b.default)
        , Test.test "storage roundtrips" <|
            \_ ->
                let
                    input =
                        { name = "test", sizeKey = "lg" }

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        , Test.test "map after roundtrip uses mapped value" <|
            \_ ->
                let
                    input =
                        { name = "test", sizeKey = "lg" }

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal 30 (b.map (Helper.lookup []) result).size
        ]



-- ADDWHEN (conditional field)


addWhenTest : Test
addWhenTest =
    let
        b =
            Helper.run
                (Control.builder
                    (\branch strVal intVal ->
                        { state = { branch = branch, strVal = strVal, intVal = intVal }
                        , toValue = \s ->
                            case s.branch of
                                "string" ->
                                    s.strVal

                                _ ->
                                    String.fromInt s.intVal
                        }
                    )
                    |> Control.add "Type"
                        .branch
                        (Control.withPresets "Type"
                            ( "string", "String" )
                            [ ( "int", "Int" ) ]
                        )
                    |> Control.addWhen (\s -> s.branch == "string") "String" .strVal Control.string
                    |> Control.addWhen (\s -> s.branch == "int") "Int" .intVal Control.int
                    |> Control.toControl_
                )
    in
    Test.describe "addWhen (conditional visibility)"
        [ Test.test "controls shown when predicate is true" <|
            \_ ->
                let
                    default =
                        { branch = "string", strVal = "Value", intVal = 1 }

                    ctrls =
                        b.controls (Just "Thing") default
                in
                -- Should have controls (group with branch + string field)
                Expect.atLeast 1 (List.length ctrls)
        , Test.test "storage roundtrips regardless of visibility" <|
            \_ ->
                let
                    input =
                        { branch = "int", strVal = "hidden", intVal = 42 }

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        , Test.test "map uses correct branch" <|
            \_ ->
                Expect.equal "42"
                    (b.map (Helper.lookup []) { branch = "int", strVal = "x", intVal = 42 })
        , Test.test "default branch shows string input with default value" <|
            \_ ->
                b.controls (Just "Thing") b.default
                    |> List.map (\c -> c (Helper.lookup []))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "input" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "Value") ]
        , Test.test "switching branch to int shows int input instead" <|
            \_ ->
                let
                    intState =
                        { branch = "int", strVal = "Value", intVal = 1 }

                    stored =
                        b.toType intState

                    lk =
                        Helper.lookup stored

                    current =
                        b.fromType b.default b.default lk
                in
                b.controls (Just "Thing") current
                    |> List.map (\c -> c lk)
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "input" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "1") ]
        ]
