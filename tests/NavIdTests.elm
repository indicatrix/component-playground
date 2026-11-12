module NavIdTests exposing (suite)

import Component.Application as Application
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Sidebar nav ids"
        [ Test.test "a row id is the prefixed page id, slashes kept" <|
            \_ ->
                Application.navRowId "modals/login"
                    |> Expect.equal "cp-nav-modals/login"
        , Test.test "the list id differs from every row id" <|
            \_ ->
                Application.navListId
                    |> String.startsWith (Application.navRowId "")
                    |> Expect.equal False
        ]
