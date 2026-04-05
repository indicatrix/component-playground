module ControlsBuilderTests exposing (suite)

import Controls
import ControlsTestHelper as Helper
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Controls.builder"
        [ twoStringFieldsTest
        , mixedTypesTest
        , fieldsIndependentTest
        , withDefaultOverrideTest
        , addMappedTest
        ]



-- TWO STRING FIELDS


twoStringFieldsTest : Test
twoStringFieldsTest =
    let
        b =
            Helper.run
                (Controls.builder (\a b_ -> { a = a, b = b_ })
                    |> Controls.add "A" .a Controls.string
                    |> Controls.add "B" .b Controls.string
                    |> Controls.toControls
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
                (Controls.builder (\name count enabled -> { name = name, count = count, enabled = enabled })
                    |> Controls.add "Name" .name Controls.string
                    |> Controls.add "Count" .count Controls.int
                    |> Controls.add "Enabled" .enabled Controls.bool
                    |> Controls.toControls
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
                (Controls.builder (\a b_ -> { a = a, b = b_ })
                    |> Controls.add "A" .a Controls.string
                    |> Controls.add "B" .b Controls.string
                    |> Controls.toControls
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
                (Controls.builder (\a b_ -> { a = a, b = b_ })
                    |> Controls.add "A" .a Controls.string
                    |> Controls.add "B" .b Controls.string
                    |> Controls.toControls
                    |> Controls.withDefault { a = "Hello", b = "World" }
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



-- ADD MAPPED


addMappedTest : Test
addMappedTest =
    let
        -- Build a record with a regular `add` field (String) and an `addMapped`
        -- field using `fromLookup` (stores String key, maps to Int).
        sizeControl =
            Controls.fromLookup "" ( "sm", 10 ) [ ( "md", 20 ), ( "lg", 30 ) ]

        b =
            Helper.run
                (Controls.builder (\name size -> { name = name, size = size })
                    |> Controls.add "Name" .name Controls.string
                    |> Controls.addMapped "Size" sizeControl
                    |> Controls.toControls
                )
    in
    Test.describe "addMapped with fromLookup"
        [ Test.test "default uses mapped value from first lookup entry" <|
            \_ ->
                -- fromLookup default key is "sm", which maps to 10
                Expect.equal 10 b.default.size
        , Test.test "default name comes from string field" <|
            \_ ->
                Expect.equal "Value" b.default.name
        , Test.test "regular add field roundtrips normally" <|
            \_ ->
                let
                    stored =
                        b.toType { name = "hello", size = 20 }

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal "hello" result.name
        , Test.test "mapped field reads from lookup on fromType" <|
            \_ ->
                -- addMapped fields reconstruct from refs, so the mapped value
                -- reflects whatever the inner control's fromType+map produces
                Expect.equal 10 (b.fromType b.default b.default (Helper.lookup [])).size
        , Test.test "controls include both regular and mapped field controls" <|
            \_ ->
                let
                    ctrls =
                        b.controls (Just "Widget") b.default
                in
                -- Should have at least 1 control (the group wrapping both fields)
                Expect.atLeast 1 (List.length ctrls)
        ]
