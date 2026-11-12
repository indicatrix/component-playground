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
                Playground.sortByName [ page "c" "cherry", page "a" "Apple", page "b" "banana" ]
                    |> List.map name
                    |> Expect.equal [ "Apple", "banana", "cherry" ]
        , Test.test "groups and pages share one order" <|
            \_ ->
                Playground.sortByName [ page "z" "Zip", group "m" "Middle" [], page "a" "Ant" ]
                    |> List.map name
                    |> Expect.equal [ "Ant", "Middle", "Zip" ]
        , Test.test "sorts every level" <|
            \_ ->
                Playground.sortByName
                    [ group "outer"
                        "Outer"
                        [ group "inner" "Inner" [ page "y" "Yak", page "x" "Xenon" ]
                        , page "b" "Bee"
                        ]
                    ]
                    |> List.concatMap names
                    |> Expect.equal [ "Outer", "Bee", "Inner", "Xenon", "Yak" ]
        , Test.test "ties keep their source order" <|
            \_ ->
                Playground.sortByName [ page "second" "Same", page "first" "same", page "third" "SAME" ]
                    |> List.map id
                    |> Expect.equal [ "second", "first", "third" ]
        , Test.test "digits compare as text" <|
            \_ ->
                Playground.sortByName [ page "v2" "V2", page "v10" "V10" ]
                    |> List.map name
                    |> Expect.equal [ "V10", "V2" ]
        ]


page : String -> String -> Playground e t
page pageId pageName =
    Playground.fromFrames { id = pageId, name = pageName } []


group : String -> String -> List (Playground e t) -> Playground e t
group groupId groupName =
    Playground.group { id = groupId, name = groupName }


name : Playground e t -> String
name pg =
    case pg of
        Page meta _ ->
            meta.name

        Group meta _ ->
            meta.name


id : Playground e t -> String
id pg =
    case pg of
        Page meta _ ->
            meta.id

        Group meta _ ->
            meta.id


names : Playground e t -> List String
names pg =
    case pg of
        Page meta _ ->
            [ meta.name ]

        Group meta children ->
            meta.name :: List.concatMap names children
