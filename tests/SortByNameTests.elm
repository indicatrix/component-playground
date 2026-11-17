module SortByNameTests exposing (suite)

import Component.Internal exposing (Playground(..))
import Component.Playground as Playground
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Playground.sortByName"
        [ Test.test "sorts pages by name, ignoring case" <|
            \_ ->
                Playground.sortByName [ page "cherry", page "Apple", page "banana" ]
                    |> List.map name
                    |> Expect.equal [ "Apple", "banana", "cherry" ]
        , Test.test "groups and pages share one order" <|
            \_ ->
                Playground.sortByName [ page "Zip", group "m" "Middle" [], page "Ant" ]
                    |> List.map name
                    |> Expect.equal [ "Ant", "Middle", "Zip" ]
        , Test.test "sorts every level" <|
            \_ ->
                Playground.sortByName
                    [ group "outer"
                        "Outer"
                        [ group "inner" "Inner" [ page "Yak", page "Xenon" ]
                        , page "Bee"
                        ]
                    ]
                    |> List.concatMap names
                    |> Expect.equal [ "Outer", "Bee", "Inner", "Xenon", "Yak" ]
        , Test.test "ties keep their source order" <|
            \_ ->
                Playground.sortByName [ page "same", page "Same", page "SAME" ]
                    |> List.map name
                    |> Expect.equal [ "same", "Same", "SAME" ]
        , Test.test "digits compare as text" <|
            \_ ->
                Playground.sortByName [ page "V2", page "V10" ]
                    |> List.map name
                    |> Expect.equal [ "V10", "V2" ]
        ]


page : String -> Playground e t
page pageName =
    Playground.fromFrames { name = pageName } []


group : String -> String -> List (Playground e t) -> Playground e t
group groupUuid groupName =
    Playground.group { uuid = groupUuid, name = groupName }


name : Playground e t -> String
name pg =
    case pg of
        Page meta _ ->
            meta.name

        Group meta _ ->
            meta.name


names : Playground e t -> List String
names pg =
    case pg of
        Page meta _ ->
            [ meta.name ]

        Group meta children ->
            meta.name :: List.concatMap names children
