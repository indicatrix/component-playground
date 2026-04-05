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
