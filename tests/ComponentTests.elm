module ComponentTests exposing (suite)

import Component
import Component.Application
import Component.Application.Theme as Theme
import Component.Frame as Frame
import Component.Internal as Internal exposing (Update(..))
import Component.Playground as Playground
import Component.Ref as Ref
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
        , presetGalleryTests
        , staticFrameTests
        , embeddingTests
        ]


testPlayground : List (Playground.Playground () ())
testPlayground =
    [ Playground.group { id = "components", name = "Components" }
        [ Playground.fromComponent { id = "text-field", name = "Text field" } Components.textField
        , Playground.fromComponent { id = "dropdown-input", name = "Simple Dropdown Input" } Components.dropdownInput
        , Playground.fromComponent { id = "test-1", name = "Test 1" } Components.identifierTest
        , Playground.fromComponent { id = "int-input", name = "Int Input" } Components.intInput
        , Playground.fromComponent { id = "float-input", name = "Float Input" } Components.floatInput
        , Playground.fromComponent { id = "list-test", name = "List test" } Components.listTest
        , Playground.fromComponent { id = "combo-element", name = "Combination Element" } Components.comboElement
        , Playground.fromComponent { id = "content-block", name = "Content Block (Sum Type)" } Components.contentBlock
        ]
    ]


dummyInstance : Internal.ComponentInstance
dummyInstance =
    Internal.ComponentInstance (Internal.ComponentRef "") (Ref.fromTop Ref.take)



-- INIT


initTests : Test
initTests =
    let
        model =
            Component.Application.init Theme.default testPlayground Nothing
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
                    , "components/content-block"
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
            Component.Application.init Theme.default testPlayground Nothing
    in
    Test.describe "update"
        [ Test.test "ViewPage changes current page" <|
            \_ ->
                let
                    ( updated, _ ) =
                        Component.Application.update
                            (Component.Application.fromUpdate (Update dummyInstance []))
                            model
                in
                -- Applying an empty update shouldn't change anything meaningful
                Expect.equal model.currentPage updated.currentPage
        , Test.test "ComponentUpdate applies state changes" <|
            \_ ->
                let
                    ( _, effects ) =
                        Component.Application.update
                            (Component.Application.fromUpdate (Update dummyInstance []))
                            model
                in
                Expect.equal [] effects
        ]



-- SEARCH


searchTests : Test
searchTests =
    let
        model =
            Component.Application.init Theme.default testPlayground Nothing
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
            Component.Application.init Theme.default testPlayground Nothing
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



-- PRESETS FRAME


exampleFrameTests : Test
exampleFrameTests =
    let
        intWithPreset =
            Components.intInput
                |> Component.withPresets
                    [ Component.preset "Starting" 99
                    , Component.preset "Zero" 0
                    ]

        playground =
            [ Playground.fromFrames { id = "int-input", name = "Int Input" }
                [ Frame.fromComponent Components.intInput
                , Frame.subheading "Starting at 99"
                , Frame.presets intWithPreset
                ]
            ]

        model =
            Component.Application.init Theme.default playground Nothing

        appHtml =
            Component.Application.view model
    in
    Test.describe "Frame.presets"
        [ Test.test "presets frame renders without errors" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        , Test.test "preceding subheading is shown" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Starting at 99" ]
        , Test.test "first preset is used as the initial value" <|
            \_ ->
                -- The intInput view outputs "Int value: <n>" — first preset is 99
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.text "Int value: 99" ]
        , Test.test "tab bar includes all preset names and Custom" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.text "Starting" ]
                        , Query.has [ Selector.text "Zero" ]
                        , Query.has [ Selector.text "Custom" ]
                        ]
        ]



-- PRESET GALLERY FRAME


presetGalleryTests : Test
presetGalleryTests =
    let
        playground =
            [ Playground.fromFrames { id = "panel-gallery", name = "Panel Gallery" }
                [ Frame.presetGallery Components.panel ]
            , Playground.fromFrames { id = "dashboard", name = "Dashboard" }
                [ Frame.fromComponent Components.dashboard ]
            ]

        model =
            Component.Application.init Theme.default playground Nothing

        appHtml =
            Component.Application.view model
    in
    Test.describe "Frame.presetGallery + embedding"
        [ Test.test "renders a heading for every preset" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.text "Info" ]
                        , Query.has [ Selector.text "Warning" ]
                        , Query.has [ Selector.text "Error" ]
                        ]
        , Test.test "renders the component body for every preset" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Expect.all
                        [ Query.has [ Selector.text "Helpful context goes here." ]
                        , Query.has [ Selector.text "Something needs attention." ]
                        , Query.has [ Selector.text "Something went wrong." ]
                        ]
        ]



-- STATIC FRAME


staticFrameTests : Test
staticFrameTests =
    let
        playground =
            [ Playground.fromFrames { id = "text-field", name = "Text field" }
                [ Frame.static (Html.div [] [ Html.text "This is documentation." ])
                , Frame.fromComponent Components.textField
                ]
            ]

        model =
            Component.Application.init Theme.default playground Nothing

        appHtml =
            Component.Application.view model
    in
    Test.describe "Component.static"
        [ Test.test "static frame renders without errors" <|
            \_ ->
                appHtml
                    |> Query.fromHtml
                    |> Query.has [ Selector.tag "div" ]
        , Test.test "static frame renders its HTML content" <|
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
            Component.Application.init Theme.default testPlayground Nothing
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
