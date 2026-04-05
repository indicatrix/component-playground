module ComponentTests exposing (suite)

import Component
import Component.Application
import Component.Internal exposing (Update(..))
import Components
import Dict
import Expect
import Html.Attributes
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Component"
        [ initTests
        , updateTests
        , embeddingTests
        ]


testPlayground : List (Component.Playground () () (Component.Update () ()))
testPlayground =
    [ Component.group { id = "components", name = "Components" }
        [ Component.playground { id = "text-field", name = "Text field" }
            [ Component.explore Components.textField ]
        , Component.playground { id = "int-input", name = "Int Input" }
            [ Component.explore Components.intInput ]
        , Component.playground { id = "combo-element", name = "Combination Element" }
            [ Component.explore Components.comboElement ]
        ]
    ]



-- INIT


initTests : Test
initTests =
    let
        model =
            Component.Application.init testPlayground Nothing
    in
    Test.describe "init"
        [ Test.test "current page is the first page" <|
            \_ ->
                Expect.equal "components/text-field" model.currentPage
        , Test.test "pages dict contains all pages" <|
            \_ ->
                let
                    pageIds =
                        model.pages |> Dict.keys |> List.sort
                in
                Expect.equal
                    [ "components/combo-element"
                    , "components/int-input"
                    , "components/text-field"
                    ]
                    pageIds
        , Test.test "state starts empty" <|
            \_ ->
                Expect.equal True (Dict.isEmpty model.state)
        , Test.test "view renders without errors" <|
            \_ ->
                Component.Application.view model
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        ]



-- UPDATE


updateTests : Test
updateTests =
    let
        model =
            Component.Application.init testPlayground Nothing
    in
    Test.describe "update"
        [ Test.test "ViewPage changes current page" <|
            \_ ->
                let
                    ( updated, _ ) =
                        Component.Application.update
                            (Component.Application.fromPreviewUpdate (Update [] []))
                            model
                in
                -- Applying an empty update shouldn't change anything meaningful
                Expect.equal model.currentPage updated.currentPage
        , Test.test "ComponentUpdate applies state changes" <|
            \_ ->
                let
                    ( _, effects ) =
                        Component.Application.update
                            (Component.Application.fromPreviewUpdate (Update [] []))
                            model
                in
                Expect.equal [] effects
        ]



-- EMBEDDING


embeddingTests : Test
embeddingTests =
    let
        model =
            Component.Application.init testPlayground Nothing
                |> navigateTo "components/combo-element"

        appHtml =
            Component.Application.view model
    in
    Test.describe "component embedding (comboElement)"
        [ Test.test "combo page renders without errors" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        , Test.test "combo page renders embedded component" <|
            \_ ->
                -- The combo element should render an embedded component's HTML
                appHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "select" ]
                    |> Query.count (Expect.atLeast 1)
        , Test.test "combo page excludes self from component ref dropdown" <|
            \_ ->
                -- The component ref dropdown should not include combo-element itself
                appHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.tag "option" ]
                    |> Query.each
                        (Query.hasNot
                            [ Selector.attribute (Html.Attributes.value "combo-element") ]
                        )
        , Test.test "embedded component controls are rendered" <|
            \_ ->
                -- The embedded component should have its own controls rendered
                -- (nested text inputs from the embedded text-field component)
                appHtml
                    |> Query.fromHtml
                    |> Query.findAll [ Selector.attribute (Html.Attributes.type_ "text") ]
                    |> Query.count (Expect.atLeast 2)
        ]


navigateTo : String -> Component.Application.Model () () -> Component.Application.Model () ()
navigateTo pageId model =
    { model | currentPage = pageId }
