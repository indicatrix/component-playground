module ControlsListTests exposing (suite)

import Component.Control as Control
import Component.Type as Type
import ControlsTestHelper as Helper
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Control.list"
        [ defaultListTest
        , roundtripTest
        , toTypeIncludesLengthRefTest
        , addRemoveItemTest
        , withDefaultOverrideTest
        ]



-- DEFAULT LIST


defaultListTest : Test
defaultListTest =
    let
        b =
            Helper.run (Control.list Control.string)
    in
    Test.describe "default list"
        [ Test.test "has 3 items (hardcoded in listHelper)" <|
            \_ ->
                Expect.equal 3 (List.length b.default)
        , Test.test "each item has the inner control's default" <|
            \_ ->
                Expect.equal [ "Value", "Value", "Value" ] b.default
        ]



-- ROUNDTRIP


roundtripTest : Test
roundtripTest =
    let
        b =
            Helper.run (Control.list Control.string)
    in
    Test.describe "roundtrip"
        [ Test.test "2-item list roundtrips" <|
            \_ ->
                let
                    input =
                        [ "one", "two" ]

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        , Test.test "5-item list roundtrips" <|
            \_ ->
                let
                    input =
                        [ "a", "b", "c", "d", "e" ]

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        , Test.test "empty list roundtrips" <|
            \_ ->
                let
                    input =
                        []

                    stored =
                        b.toType input

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal input result
        ]



-- TO TYPE INCLUDES LENGTH REF


toTypeIncludesLengthRefTest : Test
toTypeIncludesLengthRefTest =
    let
        b =
            Helper.run (Control.list Control.string)
    in
    Test.describe "toType structure"
        [ Test.test "first entry is the length ref as IntValue" <|
            \_ ->
                let
                    stored =
                        b.toType [ "a", "b" ]
                in
                case stored of
                    ( _, Type.IntValue len ) :: _ ->
                        Expect.equal 2 len

                    _ ->
                        Expect.fail "Expected first entry to be (ref, IntValue length)"
        , Test.test "remaining entries are per-item StringValues" <|
            \_ ->
                let
                    stored =
                        b.toType [ "hello", "world" ]

                    stringValues =
                        List.filterMap
                            (\( _, t ) ->
                                Type.stringValue t
                            )
                            stored
                in
                Expect.equal [ "hello", "world" ] stringValues
        ]



-- ADD / REMOVE ITEMS


addRemoveItemTest : Test
addRemoveItemTest =
    let
        b =
            Helper.run (Control.list Control.string)

        -- Start with a 2-item list, get its stored state
        baseStored =
            b.toType [ "one", "two" ]

        -- The length ref is the first entry; item refs follow
        setLength len =
            case baseStored of
                ( lengthRef, _ ) :: itemRefs ->
                    Helper.lookup (( lengthRef, Type.IntValue len ) :: itemRefs)

                _ ->
                    Helper.lookup []
    in
    Test.describe "add and remove items"
        [ Test.test "adding an item increments length and adds default" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (setLength 3)
                in
                Expect.equal 3 (List.length result)
        , Test.test "added item gets inner control's default" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (setLength 3)
                in
                Expect.equal [ "one", "two", "Value" ] result
        , Test.test "removing an item decrements length" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (setLength 1)
                in
                Expect.equal [ "one" ] result
        , Test.test "removing all items produces empty list" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (setLength 0)
                in
                Expect.equal [] result
        ]



-- WITH DEFAULT OVERRIDE


withDefaultOverrideTest : Test
withDefaultOverrideTest =
    let
        b =
            Helper.run
                (Control.list Control.string
                    |> Control.withDefault [ "One", "Two", "Three" ]
                )
    in
    Test.describe "withDefault override"
        [ Test.test "default is the override list" <|
            \_ ->
                Expect.equal [ "One", "Two", "Three" ] b.default
        , Test.test "override values roundtrip" <|
            \_ ->
                let
                    stored =
                        b.toType [ "One", "Two", "Three" ]

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal [ "One", "Two", "Three" ] result
        ]
