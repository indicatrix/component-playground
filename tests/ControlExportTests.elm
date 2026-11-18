module ControlExportTests exposing (suite)

import Component.Control as Control
import Component.Internal as Internal
import ControlTestHelper as Helper
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test)


type alias Settings =
    { name : String
    , count : Int
    , size : Size
    , tags : List String
    , note : String
    }


type Size
    = Small
    | Large


settingsControl : Control.Control e t Settings
settingsControl =
    Control.builder Settings
        |> Control.add "Name" .name Control.string
        |> Control.add "Count" .count Control.int
        |> Control.add "Size" .size (Control.fromOptions "Size" ( Small, "Small" ) [ ( Large, "Large" ) ])
        |> Control.add "Tags" .tags (Control.list Control.string)
        |> Control.addWhen (\s -> s.count > 1) "Note" .note Control.string
        |> Control.toControl


example : Settings
example =
    { name = "Panel", count = 2, size = Large, tags = [ "a", "b", "c", "d" ], note = "hi" }


exportJson : Settings -> String
exportJson settings =
    let
        b =
            Helper.run settingsControl
    in
    case b.export settings (Helper.lookup (b.toType settings)) of
        Internal.Value v ->
            Encode.encode 0 v

        _ ->
            "not a value"


suite : Test
suite =
    Test.describe "settings export and import"
        [ Test.test "exports labelled fields with option labels" <|
            \_ ->
                exportJson example
                    |> Expect.equal """{"Name":"Panel","Count":2,"Size":"Large","Tags":["a","b","c","d"],"Note":"hi"}"""
        , Test.test "leaves out fields whose controls are hidden" <|
            \_ ->
                exportJson { example | count = 1 }
                    |> Expect.equal """{"Name":"Panel","Count":1,"Size":"Large","Tags":["a","b","c","d"]}"""
        , Test.test "import into the defaults round-trips" <|
            \_ ->
                let
                    b =
                        Helper.run settingsControl

                    json =
                        Decode.decodeString Decode.value (exportJson example)
                            |> Result.withDefault Encode.null

                    loaded =
                        Helper.lookup (b.import_ b.default (Helper.lookup []) json)
                in
                b.fromType b.default b.default loaded
                    |> Expect.equal example
        , Test.test "import keeps fields the file leaves out" <|
            \_ ->
                let
                    b =
                        Helper.run settingsControl

                    current =
                        Helper.lookup (b.toType example)

                    updates =
                        b.import_ example current (Encode.object [ ( "Name", Encode.string "Other" ) ])
                in
                b.fromType example example (Helper.lookup (b.toType example ++ updates))
                    |> Expect.equal { example | name = "Other" }
        , Test.test "import ignores unknown option labels" <|
            \_ ->
                let
                    b =
                        Helper.run settingsControl
                in
                b.import_ example (Helper.lookup []) (Encode.object [ ( "Size", Encode.string "Huge" ) ])
                    |> Expect.equal []
        ]
