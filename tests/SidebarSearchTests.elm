module SidebarSearchTests exposing (suite)

import Component.Internal as Internal exposing (Index(..))
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Internal.filterIndex"
        [ Test.test "tokens match across ancestor and page names" <|
            \_ ->
                search "Maptalk Symbol"
                    |> Expect.equal [ "maptalk/components/pickers/symbol-picker" ]
        , Test.test "token order does not matter" <|
            \_ ->
                search "symbol maptalk"
                    |> Expect.equal [ "maptalk/components/pickers/symbol-picker" ]
        , Test.test "every token must match somewhere on the path" <|
            \_ ->
                search "maptalk button"
                    |> Expect.equal []
        , Test.test "a matching group keeps all its pages" <|
            \_ ->
                search "pickers"
                    |> Expect.equal
                        [ "maptalk/components/pickers/symbol-picker"
                        , "maptalk/components/pickers/colour-picker"
                        ]
        , Test.test "blank search keeps everything" <|
            \_ ->
                search "  "
                    |> List.length
                    |> Expect.equal 4
        ]


search : String -> List String
search query =
    List.filterMap (Internal.filterIndex query) tree
        |> List.concatMap pageIds


pageIds : Index -> List String
pageIds (Index item) =
    if List.isEmpty item.children then
        [ item.id ]

    else
        List.concatMap pageIds item.children


tree : List Index
tree =
    [ group "maptalk"
        "Maptalk"
        [ group "maptalk/components"
            "Components"
            [ group "maptalk/components/pickers"
                "Pickers"
                [ page "maptalk/components/pickers/symbol-picker" "symbol-picker"
                , page "maptalk/components/pickers/colour-picker" "colour-picker"
                ]
            ]
        ]
    , group "ui"
        "UI"
        [ page "ui/button" "Button"
        , page "ui/symbol" "Symbol"
        ]
    ]


group : String -> String -> List Index -> Index
group id name children =
    Index { id = id, name = name, children = children }


page : String -> String -> Index
page id name =
    Index { id = id, name = name, children = [] }
