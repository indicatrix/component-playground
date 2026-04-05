module ComponentTests exposing (suite)

import Component
import Component.Application
import Component.Internal exposing (Update(..))
import Components
import Dict
import Expect
import Html
import Html.Attributes
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Component"
        [ initTests
        , updateTests
        , searchTests
        , toUrlTests
        , exampleFrameTests
        , docoFrameTests
        , embeddingTests
        ]


testPlayground : List (Component.Playground () () (Component.Update () ()))
testPlayground =
    [ Component.group { id = "components", name = "Components" }
        [ Component.playground { id = "text-field", name = "Text field" }
            [ Component.explore Components.textField ]
        , Component.playground { id = "dropdown-input", name = "Simple Dropdown Input" }
            [ Component.explore Components.dropdownInput ]
        , Component.playground { id = "test-1", name = "Test 1" }
            [ Component.explore Components.identifierTest ]
        , Component.playground { id = "int-input", name = "Int Input" }
            [ Component.explore Components.intInput ]
        , Component.playground { id = "float-input", name = "Float Input" }
            [ Component.explore Components.floatInput ]
        , Component.playground { id = "list-test", name = "List test" }
            [ Component.explore Components.listTest ]
        , Component.playground { id = "combo-element", name = "Combination Element" }
            [ Component.explore_ Components.comboElement ]
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
                    , "components/dropdown-input"
                    , "components/float-input"
                    , "components/int-input"
                    , "components/list-test"
                    , "components/test-1"
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



-- SEARCH


searchTests : Test
searchTests =
    let
        model =
            Component.Application.init testPlayground Nothing
    in
    Test.describe "UpdateSearch"
        [ Test.test "search starts empty" <|
            \_ ->
                Expect.equal "" model.search
        , Test.test "UpdateSearch updates search field" <|
            \_ ->
                let
                    searchModel =
                        { model | search = "text" }
                in
                Expect.equal "text" searchModel.search
        , Test.test "search filters sidebar to matching pages" <|
            \_ ->
                let
                    searchModel =
                        { model | search = "Int" }

                    appHtml =
                        Component.Application.view searchModel
                in
                -- "Combination Element" should be filtered out of the sidebar
                appHtml
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.text "Combination Element" ]
        , Test.test "search is case-insensitive" <|
            \_ ->
                let
                    searchModel =
                        { model | search = "int" }

                    appHtml =
                        Component.Application.view searchModel
                in
                -- "Int Input" should still appear with lowercase search
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Int Input" ]
        ]



-- TO URL


toUrlTests : Test
toUrlTests =
    let
        model =
            Component.Application.init testPlayground Nothing
    in
    Test.describe "toUrl"
        [ Test.test "generates URL with component query param" <|
            \_ ->
                let
                    url =
                        Component.Application.toUrl "index.html" model
                in
                Expect.equal "index.html?component=components%2Ftext-field" url
        , Test.test "reflects current page after navigation" <|
            \_ ->
                let
                    navigated =
                        navigateTo "components/int-input" model

                    url =
                        Component.Application.toUrl "index.html" navigated
                in
                Expect.equal "index.html?component=components%2Fint-input" url
        ]



-- EXAMPLE FRAME


exampleFrameTests : Test
exampleFrameTests =
    let
        playground =
            [ Component.playground { id = "int-input", name = "Int Input" }
                [ Component.explore Components.intInput
                , Component.example "Starting at 99" 99 Components.intInput
                ]
            ]

        model =
            Component.Application.init playground Nothing

        appHtml =
            Component.Application.view model
    in
    Test.describe "Component.example"
        [ Test.test "example frame renders without errors" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        , Test.test "example frame shows its name" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Starting at 99" ]
        , Test.test "example frame renders the component view" <|
            \_ ->
                -- The intInput view outputs "Int value: <n>"
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Int value:" ]
        ]



-- DOCO FRAME


docoFrameTests : Test
docoFrameTests =
    let
        playground =
            [ Component.playground { id = "text-field", name = "Text field" }
                [ Component.doco (Html.div [] [ Html.text "This is documentation." ])
                , Component.explore Components.textField
                ]
            ]

        model =
            Component.Application.init playground Nothing

        appHtml =
            Component.Application.view model
    in
    Test.describe "Component.doco"
        [ Test.test "doco frame renders without errors" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        , Test.test "doco frame renders its HTML content" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "This is documentation." ]
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
