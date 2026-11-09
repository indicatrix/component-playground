module ControlListTests exposing (suite)

import Component.Control as Control
import Component.Type as Type
import ControlTestHelper as Helper
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



-- TO TYPE STRUCTURE


toTypeIncludesLengthRefTest : Test
toTypeIncludesLengthRefTest =
    let
        b =
            Helper.run (Control.list Control.string)
    in
    Test.describe "toType structure"
        [ Test.test "first entry is the slot order" <|
            \_ ->
                case b.toType [ "a", "b" ] of
                    ( _, Type.StringValue order ) :: _ ->
                        Expect.equal "0,1" order

                    _ ->
                        Expect.fail "Expected first entry to be (ref, StringValue order)"
        , Test.test "remaining entries are per-item StringValues" <|
            \_ ->
                b.toType [ "hello", "world" ]
                    |> List.drop 1
                    |> List.filterMap (\( _, t ) -> Type.stringValue t)
                    |> Expect.equal [ "hello", "world" ]
        ]



-- INSERT / MOVE / DELETE BY SLOT ORDER


addRemoveItemTest : Test
addRemoveItemTest =
    let
        b =
            Helper.run (Control.list Control.string)

        baseStored =
            b.toType [ "one", "two" ]

        withOrder order =
            case baseStored of
                ( orderRef, _ ) :: itemRefs ->
                    Helper.lookup (( orderRef, Type.StringValue order ) :: itemRefs)

                _ ->
                    Helper.lookup []
    in
    Test.describe "slot order"
        [ Test.test "a fresh slot reads as the inner control's default" <|
            \_ ->
                b.fromType b.default b.default (withOrder "0,5,1")
                    |> Expect.equal [ "one", "Value", "two" ]
        , Test.test "reordering slots reorders the items" <|
            \_ ->
                b.fromType b.default b.default (withOrder "1,0")
                    |> Expect.equal [ "two", "one" ]
        , Test.test "dropping a slot deletes its item" <|
            \_ ->
                b.fromType b.default b.default (withOrder "1")
                    |> Expect.equal [ "two" ]
        , Test.test "an empty order is an empty list" <|
            \_ ->
                b.fromType b.default b.default (withOrder "")
                    |> Expect.equal []
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
