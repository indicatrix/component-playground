module ControlTests exposing (suite)

import Component.Control as Control
import Component.Type as Type
import ControlTestHelper as Helper
import Expect
import Html
import Html.Attributes
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Control"
        [ stringTests
        , intTests
        , floatTests
        , boolTests
        , identifierTests
        , withPresetsTests
        , fromLookupTests
        , hiddenTests
        , withUpdateTests
        , customTests
        , builderControlsHtmlTest
        , maybeTests
        ]



-- STRING


stringTests : Test
stringTests =
    let
        b =
            Helper.run Control.string
    in
    Test.describe "Control.string"
        [ Test.test "default is \"Value\"" <|
            \_ ->
                Expect.equal "Value" b.default
        , Test.test "roundtrip: toType then fromType" <|
            \_ ->
                let
                    stored =
                        b.toType "hello"

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal "hello" result
        , Test.test "fromType with empty lookup returns default" <|
            \_ ->
                Expect.equal "Value"
                    (b.fromType b.default b.default (Helper.lookup []))
        , Test.test "withDefault overrides default" <|
            \_ ->
                let
                    b2 =
                        Helper.run (Control.string |> Control.withDefault "custom")
                in
                Expect.equal "custom" b2.default
        , Test.test "control renders text input with correct value" <|
            \_ ->
                let
                    stored =
                        b.toType "hello"
                in
                b.controls (Just "Name") b.default
                    |> List.map (\c -> c (Helper.lookup stored))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "input" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "hello") ]
        , Test.test "typing in text input produces correct message" <|
            \_ ->
                b.controls (Just "Name") b.default
                    |> List.map (\c -> c (Helper.lookup []))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "input" ]
                    |> Event.simulate (Event.input "new text")
                    |> Event.expect (b.toType "new text")
        ]



-- INT


intTests : Test
intTests =
    let
        b =
            Helper.run Control.int
    in
    Test.describe "Control.int"
        [ Test.test "default is 1" <|
            \_ ->
                Expect.equal 1 b.default
        , Test.test "roundtrip: toType then fromType" <|
            \_ ->
                let
                    stored =
                        b.toType 42

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal 42 result
        , Test.test "fromType with empty lookup returns default" <|
            \_ ->
                Expect.equal 1
                    (b.fromType b.default b.default (Helper.lookup []))
        , Test.test "control renders text input showing value" <|
            \_ ->
                let
                    stored =
                        b.toType 42
                in
                b.controls (Just "Count") b.default
                    |> List.map (\c -> c (Helper.lookup stored))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.find [ Selector.tag "input" ]
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "42") ]
        ]



-- FLOAT


floatTests : Test
floatTests =
    let
        b =
            Helper.run Control.float
    in
    Test.describe "Control.float"
        [ Test.test "default is 1.0" <|
            \_ ->
                Expect.equal 1.0 b.default
        , Test.test "roundtrip: toType then fromType" <|
            \_ ->
                let
                    stored =
                        b.toType 3.14

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.within (Expect.Absolute 0.001) 3.14 result
        , Test.test "fromType with empty lookup returns default" <|
            \_ ->
                Expect.within (Expect.Absolute 0.001)
                    1.0
                    (b.fromType b.default b.default (Helper.lookup []))
        ]



-- BOOL


boolTests : Test
boolTests =
    let
        b =
            Helper.run Control.bool
    in
    Test.describe "Control.bool"
        [ Test.test "default is True (first preset)" <|
            \_ ->
                Expect.equal True b.default
        , Test.test "roundtrip True" <|
            \_ ->
                let
                    stored =
                        b.toType True

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal True result
        , Test.test "roundtrip False" <|
            \_ ->
                let
                    stored =
                        b.toType False

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal False result
        , Test.test "control renders select with True and False options" <|
            \_ ->
                b.controls (Just "Enabled") b.default
                    |> List.map (\c -> c (Helper.lookup []))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "option" ]
                    |> Query.count (Expect.equal 2)
        ]



-- IDENTIFIER


identifierTests : Test
identifierTests =
    let
        b =
            Helper.run Control.identifier
    in
    Test.describe "Control.identifier"
        [ Test.test "fromType produces a ref-derived string, not \"pending\"" <|
            \_ ->
                let
                    result =
                        b.fromType b.default b.default (Helper.lookup [])
                in
                Expect.notEqual "pending" result
        , Test.test "toType produces empty list (no serialisation)" <|
            \_ ->
                Expect.equal [] (b.toType "anything")
        , Test.test "controls produces empty list (no UI)" <|
            \_ ->
                Expect.equal [] (b.controls (Just "Id") b.default)
        ]



-- WITH PRESETS


withPresetsTests : Test
withPresetsTests =
    let
        b =
            Helper.run
                (Control.withPresets "" ( "red", "Red" ) [ ( "green", "Green" ), ( "blue", "Blue" ) ])
    in
    Test.describe "Control.withPresets"
        [ Test.test "default is first preset" <|
            \_ ->
                Expect.equal "red" b.default
        , Test.test "roundtrip through index-based storage" <|
            \_ ->
                let
                    stored =
                        b.toType "green"

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal "green" result
        , Test.test "unknown value returns default" <|
            \_ ->
                Expect.equal "red"
                    (b.fromType b.default b.default (Helper.lookup []))
        , Test.test "select lists all preset labels" <|
            \_ ->
                b.controls (Just "Color") b.default
                    |> List.map (\c -> c (Helper.lookup []))
                    |> Html.div []
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "option" ]
                    |> Query.count (Expect.equal 3)
        ]



-- FROM LOOKUP


fromLookupTests : Test
fromLookupTests =
    let
        b =
            Helper.run
                (Control.fromLookup "" ( "sm", 10 ) [ ( "md", 20 ), ( "lg", 30 ) ])
    in
    Test.describe "Control.fromLookup"
        [ Test.test "default is first key" <|
            \_ ->
                Expect.equal "sm" b.default
        , Test.test "roundtrip through string key storage" <|
            \_ ->
                let
                    stored =
                        b.toType "lg"

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal "lg" result
        , Test.test "map produces the associated value, not the key" <|
            \_ ->
                Expect.equal 30
                    (b.map (Helper.lookup []) "lg")
        , Test.test "map with unknown key returns first value" <|
            \_ ->
                Expect.equal 10
                    (b.map (Helper.lookup []) "unknown")
        ]



-- HIDDEN


hiddenTests : Test
hiddenTests =
    let
        b =
            Helper.run (Control.hidden Control.string)
    in
    Test.describe "Control.hidden"
        [ Test.test "controls returns empty list" <|
            \_ ->
                Expect.equal [] (b.controls (Just "Hidden") b.default)
        , Test.test "roundtrip still works" <|
            \_ ->
                let
                    stored =
                        b.toType "secret"

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal "secret" result
        ]



-- WITH UPDATE


withUpdateTests : Test
withUpdateTests =
    let
        -- Clamp string length to max 5
        b =
            Helper.run
                (Control.string
                    |> Control.withUpdate
                        (\_ new ->
                            ( String.left 5 new, [] )
                        )
                )
    in
    Test.describe "Control.withUpdate"
        [ Test.test "update function is called with old and new values" <|
            \_ ->
                let
                    ( result, _ ) =
                        b.update "old" "longer than five"
                in
                Expect.equal "longe" result
        , Test.test "update can produce effects" <|
            \_ ->
                let
                    bWithEffect =
                        Helper.run
                            (Control.string
                                |> Control.withUpdate
                                    (\_ new ->
                                        ( new, [ "effect!" ] )
                                    )
                            )

                    ( _, effects ) =
                        bWithEffect.update "old" "new"
                in
                Expect.equal [ "effect!" ] effects
        ]



-- CUSTOM


customTests : Test
customTests =
    let
        b =
            Helper.run
                (Control.custom
                    String.toInt
                    String.fromInt
                    0
                )
    in
    Test.describe "Control.custom"
        [ Test.test "default is 0" <|
            \_ ->
                Expect.equal 0 b.default
        , Test.test "roundtrip through CustomValue" <|
            \_ ->
                let
                    stored =
                        b.toType 42

                    result =
                        b.fromType b.default b.default (Helper.lookup stored)
                in
                Expect.equal 42 result
        , Test.test "fromType with empty lookup returns default" <|
            \_ ->
                Expect.equal 0
                    (b.fromType b.default b.default (Helper.lookup []))
        , Test.test "toType produces a CustomValue" <|
            \_ ->
                case b.toType 7 of
                    [ ( _, Type.CustomValue t ) ] ->
                        Expect.equal "7" t

                    other ->
                        Expect.fail ("Expected [(ref, CustomValue \"7\")], got " ++ Debug.toString other)
        , Test.test "controls returns empty list (no UI)" <|
            \_ ->
                Expect.equal [] (b.controls (Just "Custom") b.default)
        , Test.test "fromType ignores non-custom type values" <|
            \_ ->
                let
                    stored =
                        b.toType 99

                    -- Replace the CustomValue with a StringValue to simulate wrong type
                    badLookup =
                        Helper.lookup
                            (List.map
                                (\( ref, _ ) -> ( ref, Type.StringValue "99" ))
                                stored
                            )
                in
                Expect.equal 0 (b.fromType b.default b.default badLookup)
        , Test.test "fromType returns default when custom fromType_ returns Nothing" <|
            \_ ->
                let
                    stored =
                        b.toType 42

                    -- Replace with a CustomValue that won't parse as Int
                    badLookup =
                        Helper.lookup
                            (List.map
                                (\( ref, _ ) -> ( ref, Type.CustomValue "not-a-number" ))
                                stored
                            )
                in
                Expect.equal 0 (b.fromType b.default b.default badLookup)
        ]



-- BUILDER CONTROLS HTML


builderControlsHtmlTest : Test
builderControlsHtmlTest =
    let
        controls =
            Control.builder (\value label id error -> { id = id, label = label, value = value, error = error })
                |> Control.add "Value" .value Control.string
                |> Control.add "Label" .label Control.string
                |> Control.add "Id" .id Control.identifier
                |> Control.add "Error" .error Control.string
                |> Control.toControl
                |> Control.withDefault { id = "not used", label = "Label", value = "Value", error = "" }

        b =
            Helper.run controls

        emptyLookup =
            Helper.lookup []

        controlsHtml =
            b.controls (Just "Text field") b.default
                |> List.map (\c -> c emptyLookup)
                |> Html.div []
    in
    Test.describe "Builder controls HTML (textField shape)"
        [ Test.test "text inputs show default string values, not refs" <|
            \_ ->
                controlsHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "input" ]
                    |> Query.each
                        (Query.hasNot
                            [ Selector.attribute (Html.Attributes.value "0")
                            , Selector.attribute (Html.Attributes.value "0.0")
                            , Selector.attribute (Html.Attributes.value "1.0")
                            ]
                        )
        , Test.test "Value field shows default \"Value\"" <|
            \_ ->
                controlsHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "input" ]
                    |> Query.first
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "Value") ]
        , Test.test "Label field shows default \"Label\"" <|
            \_ ->
                controlsHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "input" ]
                    |> Query.index 1
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "Label") ]
        , Test.test "Error field shows default empty string" <|
            \_ ->
                controlsHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "input" ]
                    |> Query.index 2
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.value "") ]
        ]



-- MAYBE


maybeTests : Test
maybeTests =
    let
        b =
            Helper.run (Control.maybe Control.string)
    in
    Test.describe "Control.maybe"
        [ Test.test "default storage has enabled=True" <|
            \_ ->
                Expect.equal True b.default.has
        , Test.test "map returns Just when enabled" <|
            \_ ->
                Expect.equal (Just "Value")
                    (b.map (Helper.lookup []) { has = True, value = "Value" })
        , Test.test "map returns Nothing when disabled" <|
            \_ ->
                Expect.equal Nothing
                    (b.map (Helper.lookup []) { has = False, value = "Value" })
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
        , Test.test "withDefault overrides storage" <|
            \_ ->
                let
                    b2 =
                        Helper.run
                            (Control.maybe Control.string
                                |> Control.withDefault { has = False, val = "none" }
                            )
                in
                Expect.equal { has = False, val = "none" } b2.default
        ]
