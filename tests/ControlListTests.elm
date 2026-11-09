module ControlListTests exposing (suite)

import Component.Control as Control
import Component.Internal as Internal
import Component.Ref as Ref
import Component.Type as Type
import ControlTestHelper as Helper
import Dict
import Expect
import State
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
        [ Test.test "first entry is the length" <|
            \_ ->
                case b.toType [ "a", "b" ] of
                    ( _, Type.IntValue len ) :: _ ->
                        Expect.equal 2 len

                    _ ->
                        Expect.fail "Expected first entry to be (ref, IntValue length)"
        , Test.test "remaining entries are per-item StringValues" <|
            \_ ->
                b.toType [ "hello", "world" ]
                    |> List.drop 1
                    |> List.filterMap (\( _, t ) -> Type.stringValue t)
                    |> Expect.equal [ "hello", "world" ]
        ]



-- SHIFTING ITEMS BY COPYING SUBTREES


addRemoveItemTest : Test
addRemoveItemTest =
    let
        b =
            Helper.run (Control.list Control.string)

        listRef =
            Ref.fromTop Ref.take

        slot index =
            Ref.child index listRef

        -- A ref inside item `index`'s subtree that `toType` never writes, like
        -- an embedded component's state.
        nestedRef index =
            Ref.from (slot index) (State.map2 (\_ r -> r) Ref.take Ref.take)

        store updates =
            Internal.applyUpdates updates Dict.empty

        lookupIn state ref =
            Dict.get (Ref.toString ref) state

        -- The component write-back that follows every Inspector change.
        writeBack state =
            Internal.applyUpdates (b.toType (b.fromType b.default b.default (lookupIn state))) state

        start =
            store
                (b.toType [ "a", "b", "c" ]
                    ++ List.map (\i -> ( nestedRef i, Type.StringValue ("nested " ++ String.fromInt i) )) [ 0, 1, 2 ]
                )

        deleteMiddle =
            Internal.applyUpdates
                [ ( slot 1, Type.Copy (slot 2) )
                , ( slot 2, Type.Empty )
                , ( listRef, Type.IntValue 2 )
                ]
                start
                |> writeBack

        swapFirstTwo =
            Internal.applyUpdates
                [ ( Ref.tmp, Type.Copy (slot 0) )
                , ( slot 0, Type.Copy (slot 1) )
                , ( slot 1, Type.Copy Ref.tmp )
                , ( Ref.tmp, Type.Empty )
                ]
                start
                |> writeBack
    in
    Test.describe "shifting items"
        [ Test.test "deleting an item shifts the later items up" <|
            \_ ->
                b.fromType b.default b.default (lookupIn deleteMiddle)
                    |> Expect.equal [ "a", "c" ]
        , Test.test "a shifted item takes its nested state along" <|
            \_ ->
                List.map (nestedRef >> lookupIn deleteMiddle) [ 0, 1, 2 ]
                    |> Expect.equal [ Just (Type.StringValue "nested 0"), Just (Type.StringValue "nested 2"), Nothing ]
        , Test.test "a swap through the scratch ref swaps values and nested state" <|
            \_ ->
                ( b.fromType b.default b.default (lookupIn swapFirstTwo)
                , List.map (nestedRef >> lookupIn swapFirstTwo) [ 0, 1 ]
                )
                    |> Expect.equal
                        ( [ "b", "a", "c" ]
                        , [ Just (Type.StringValue "nested 1"), Just (Type.StringValue "nested 0") ]
                        )
        , Test.test "the scratch ref is empty after a swap" <|
            \_ ->
                Dict.keys swapFirstTwo
                    |> List.filter (String.startsWith (Ref.toString Ref.tmp))
                    |> Expect.equal []
        , Test.test "a fresh length past the stored items reads defaults" <|
            \_ ->
                b.fromType b.default b.default (lookupIn (Internal.applyUpdates [ ( listRef, Type.IntValue 4 ) ] start))
                    |> Expect.equal [ "a", "b", "c", "Value" ]
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
