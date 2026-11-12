module SidebarBehaviourTests exposing (suite)

import Component.Application as Application exposing (Align(..), NavReveal, Preferences)
import Component.Application.Theme as Theme
import Component.ControlRenderers as ControlRenderers
import Component.Playground as Playground
import Components
import Expect
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Set
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Sidebar behaviour"
        [ revealTests
        , deepLinkTests
        , isPageTests
        , preferencesTests
        , jsonTests
        ]


type Effect
    = Reveal NavReveal
    | Save Preferences


type alias Model =
    Application.Model () Effect



-- FIXTURES


{-| Pages `inputs/text`, `inputs/numbers/int` and `layout/panel`; sections
`inputs`, `inputs/numbers` and `layout`. `inputs/text` opens first.
-}
start : Model
start =
    Application.initWith
        { renderers = ControlRenderers.default Theme.default
        , exportComponentConfig = Nothing
        , openComponentConfig = Nothing
        , revealNavRow = Just Reveal
        , savePreferences = Just Save
        }
        Theme.default
        [ Playground.group { id = "inputs", name = "Inputs" }
            [ Playground.fromComponent { id = "text", name = "Text" } Components.textField
            , Playground.group { id = "numbers", name = "Numbers" }
                [ Playground.fromComponent { id = "int", name = "Int" } Components.intInput ]
            ]
        , Playground.group { id = "layout", name = "Layout" }
            [ Playground.fromComponent { id = "panel", name = "Panel" } Components.panel ]
        ]


defaults : Preferences
defaults =
    { collapsedGroups = []
    , canvasHeight = Nothing
    , canvasLook = { background = "#F5F7FA", grid = True }
    }


collapsed : List String -> Preferences
collapsed ids =
    { defaults | collapsedGroups = ids }


reveal : Align -> String -> Effect
reveal align page =
    Reveal { list = "cp-nav", row = "cp-nav-" ++ page, align = align, topInset = 8 }


{-| Fire an event on the element matching `selector` in the shell's view, and
apply the message it produces. Effects pile up, latest step last.
-}
fire : List Selector.Selector -> ( String, Encode.Value ) -> ( Model, List (List Effect) ) -> Result String ( Model, List (List Effect) )
fire selector event ( model, effects ) =
    Application.view model
        |> Query.fromHtml
        |> Query.find selector
        |> Event.simulate event
        |> Event.toResult
        |> Result.map
            (\msg ->
                Application.update msg model
                    |> Tuple.mapSecond (\new -> effects ++ [ new ])
            )


clickRow : String -> ( Model, List (List Effect) ) -> Result String ( Model, List (List Effect) )
clickRow id =
    fire [ Selector.id ("cp-nav-" ++ id) ] Event.click


search : String -> ( Model, List (List Effect) ) -> Result String ( Model, List (List Effect) )
search query =
    fire [ Selector.id "playground-search" ] (Event.input query)


from : Model -> Result String ( Model, List (List Effect) )
from model =
    Ok ( model, [] )


collapsedIds : ( Model, a ) -> List String
collapsedIds ( model, _ ) =
    Set.toList model.collapsedGroups


lastEffects : ( Model, List (List Effect) ) -> List Effect
lastEffects ( _, effects ) =
    List.reverse effects |> List.head |> Maybe.withDefault []



-- REVEAL


revealTests : Test
revealTests =
    Test.describe "revealing the active page"
        [ Test.test "moving to a page in a collapsed section opens it and reveals the row" <|
            \_ ->
                start
                    |> Application.withPreferences (collapsed [ "inputs/numbers", "layout" ])
                    |> Application.setLocation { page = "layout/panel", preset = Nothing }
                    |> Expect.all
                        [ Tuple.first >> .collapsedGroups >> Set.toList >> Expect.equal [ "inputs/numbers" ]
                        , Tuple.second
                            >> Expect.equal
                                [ reveal Nearest "layout/panel"
                                , Save (collapsed [ "inputs/numbers" ])
                                ]
                        ]
        , Test.test "clicking another page reveals its row; nothing to save" <|
            \_ ->
                from start
                    |> Result.andThen (clickRow "layout/panel")
                    |> Result.map lastEffects
                    |> Expect.equal (Ok [ reveal Nearest "layout/panel" ])
        , Test.test "collapsing the active page's section keeps it collapsed" <|
            \_ ->
                from start
                    |> Result.andThen (clickRow "inputs")
                    |> Result.map (\r -> ( collapsedIds r, lastEffects r ))
                    |> Expect.equal (Ok ( [ "inputs" ], [ Save (collapsed [ "inputs" ]) ] ))
        , Test.test "clearing a search opens the active page's sections and reveals its row" <|
            \_ ->
                from start
                    |> Result.andThen (clickRow "inputs")
                    |> Result.andThen (search "te")
                    |> Result.andThen (search "")
                    |> Result.map (\( model, effects ) -> ( Set.toList model.collapsedGroups, List.drop 1 effects ))
                    |> Expect.equal
                        (Ok
                            ( []
                            , [ []
                              , [ reveal Nearest "inputs/text", Save defaults ]
                              ]
                            )
                        )
        , Test.test "changing a search doesn't reveal" <|
            \_ ->
                from start
                    |> Result.andThen (search "te")
                    |> Result.andThen (search "tex")
                    |> Result.map (\( _, effects ) -> List.concat effects)
                    |> Expect.equal (Ok [])
        , Test.test "revealCurrentPage scrolls the current row to the top" <|
            \_ ->
                Application.revealCurrentPage start
                    |> Expect.equal (Just (reveal Start "inputs/text"))
        , Test.test "without revealNavRow there's nothing to reveal" <|
            \_ ->
                Application.init Theme.default [ Playground.fromComponent { id = "text", name = "Text" } Components.textField ]
                    |> Application.revealCurrentPage
                    |> Expect.equal Nothing
        , Test.test "the request names the rendered list, whose top padding is topInset" <|
            \_ ->
                case Application.revealCurrentPage start of
                    Just (Reveal request) ->
                        Application.view start
                            |> Query.fromHtml
                            |> Expect.all
                                [ Query.find [ Selector.id request.list ]
                                    >> Query.has [ Selector.style "padding" (String.fromFloat request.topInset ++ "px 12px 24px 12px") ]
                                , Query.find [ Selector.id request.list ]
                                    >> Query.find [ Selector.id request.row ]
                                    >> Query.has []
                                ]

                    _ ->
                        Expect.fail "no reveal request"
        ]



-- DEEP LINK


deepLinkTests : Test
deepLinkTests =
    Test.describe "collapseToCurrentPage"
        [ Test.test "collapses every section but the current page's" <|
            \_ ->
                start
                    |> Application.setLocation { page = "inputs/numbers/int", preset = Nothing }
                    |> Tuple.first
                    |> Application.collapseToCurrentPage
                    |> .collapsedGroups
                    |> Set.toList
                    |> Expect.equal [ "layout" ]
        , Test.test "overrides restored preferences" <|
            \_ ->
                start
                    |> Application.withPreferences (collapsed [ "inputs", "inputs/numbers" ])
                    |> Application.setLocation { page = "layout/panel", preset = Nothing }
                    |> Tuple.first
                    |> Application.collapseToCurrentPage
                    |> .collapsedGroups
                    |> Set.toList
                    |> Expect.equal [ "inputs", "inputs/numbers" ]
        ]


isPageTests : Test
isPageTests =
    Test.describe "isPage"
        [ Test.test "a page id is a page" <|
            \_ -> Application.isPage "inputs/numbers/int" start |> Expect.equal True
        , Test.test "a section id isn't" <|
            \_ -> Application.isPage "inputs/numbers" start |> Expect.equal False
        , Test.test "an unknown id isn't" <|
            \_ -> Application.isPage "int" start |> Expect.equal False
        ]



-- PREFERENCES


{-| A press on the canvas's height handle with the canvas 400px tall, in a
window 1000px high.
-}
pressAt : Float -> ( String, Encode.Value )
pressAt y =
    Event.custom "pointerdown"
        (Encode.object
            [ ( "button", Encode.int 0 )
            , ( "isPrimary", Encode.bool True )
            , ( "clientY", Encode.float y )
            , ( "currentTarget", Encode.object [ ( "previousElementSibling", Encode.object [ ( "offsetHeight", Encode.float 400 ) ] ) ] )
            , ( "view", Encode.object [ ( "innerHeight", Encode.float 1000 ) ] )
            ]
        )


moveTo : Float -> ( String, Encode.Value )
moveTo y =
    Event.custom "pointermove" (Encode.object [ ( "clientY", Encode.float y ), ( "buttons", Encode.int 1 ) ])


shield : List Selector.Selector
shield =
    [ Selector.class "cp-canvas-resize-shield" ]


preferencesTests : Test
preferencesTests =
    Test.describe "preferences"
        [ Test.test "withPreferences drops ids that aren't sections" <|
            \_ ->
                start
                    |> Application.withPreferences (collapsed [ "layout", "gone", "inputs/text" ])
                    |> .collapsedGroups
                    |> Set.toList
                    |> Expect.equal [ "layout" ]
        , Test.test "withPreferences restores the canvas" <|
            \_ ->
                start
                    |> Application.withPreferences { defaults | canvasHeight = Just 500, canvasLook = { background = "#171717", grid = False } }
                    |> .canvas
                    |> (\canvas -> ( canvas.height, canvas.background.hex, canvas.grid ))
                    |> Expect.equal ( Just 500, "#171717", False )
        , Test.test "toggling a section saves once" <|
            \_ ->
                from start
                    |> Result.andThen (clickRow "layout")
                    |> Result.map lastEffects
                    |> Expect.equal (Ok [ Save (collapsed [ "layout" ]) ])
        , Test.test "collapse all saves once" <|
            \_ ->
                from start
                    |> Result.andThen (fire [ attributeLabel "Collapse all" ] Event.click)
                    |> Result.map lastEffects
                    |> Expect.equal (Ok [ Save (collapsed [ "inputs", "inputs/numbers", "layout" ]) ])
        , Test.test "a canvas height drag saves once, when it ends" <|
            \_ ->
                from start
                    |> Result.andThen (fire [ Selector.class "cp-canvas-resize" ] (pressAt 500))
                    |> Result.andThen (fire shield (moveTo 550))
                    |> Result.andThen (fire shield (moveTo 600))
                    |> Result.andThen (fire shield (Event.custom "pointerup" (Encode.object [])))
                    |> Result.map Tuple.second
                    |> Expect.equal (Ok [ [], [], [], [ Save { defaults | canvasHeight = Just 500 } ] ])
        , Test.test "hiding the canvas grid saves" <|
            \_ ->
                from start
                    |> Result.andThen (fire [ attributeLabel "Show grid" ] Event.click)
                    |> Result.map lastEffects
                    |> Expect.equal (Ok [ Save { defaults | canvasLook = { background = "#F5F7FA", grid = False } } ])
        , Test.test "a change to anything else saves nothing" <|
            \_ ->
                from start
                    |> Result.andThen (search "panel")
                    |> Result.map lastEffects
                    |> Expect.equal (Ok [])
        , Test.test "without savePreferences nothing saves" <|
            \_ ->
                let
                    model =
                        Application.init Theme.default [ Playground.group { id = "g", name = "G" } [ Playground.fromComponent { id = "text", name = "Text" } Components.textField ] ]
                in
                Application.view model
                    |> Query.fromHtml
                    |> Query.find [ Selector.id "cp-nav-g" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Result.map (\msg -> Application.update msg model |> Tuple.second |> List.length)
                    |> Expect.equal (Ok 0)
        ]


attributeLabel : String -> Selector.Selector
attributeLabel label =
    Selector.attribute (Html.Attributes.attribute "aria-label" label)



-- JSON


jsonTests : Test
jsonTests =
    Test.describe "preferences JSON"
        [ Test.test "round-trips" <|
            \_ ->
                let
                    preferences =
                        { collapsedGroups = [ "a", "a/b" ], canvasHeight = Just 480, canvasLook = { background = "#171717", grid = False } }
                in
                Application.encodePreferences preferences
                    |> Decode.decodeValue Application.preferencesDecoder
                    |> Expect.equal (Ok preferences)
        , Test.test "a saved set reads back as the same preferences" <|
            \_ ->
                from start
                    |> Result.andThen (clickRow "layout")
                    |> Result.map
                        (lastEffects
                            >> List.filterMap
                                (\effect ->
                                    case effect of
                                        Save preferences ->
                                            Just preferences

                                        Reveal _ ->
                                            Nothing
                                )
                            >> List.map (Application.encodePreferences >> Decode.decodeValue Application.preferencesDecoder)
                        )
                    |> Expect.equal (Ok [ Ok (collapsed [ "layout" ]) ])
        , Test.test "missing fields take their defaults" <|
            \_ ->
                Decode.decodeString Application.preferencesDecoder "{}"
                    |> Expect.equal (Ok defaults)
        , Test.test "a value that isn't an object decodes to the defaults" <|
            \_ ->
                Decode.decodeString Application.preferencesDecoder "null"
                    |> Expect.equal (Ok defaults)
        , Test.test "a malformed field takes its default; the rest decode" <|
            \_ ->
                Decode.decodeString Application.preferencesDecoder
                    """{ "collapsedGroups": 3, "canvasHeight": "tall", "canvasLook": { "background": 1, "grid": false } }"""
                    |> Expect.equal (Ok { defaults | canvasLook = { background = "#F5F7FA", grid = False } })
        , Test.test "a null canvas height is a self-sizing canvas" <|
            \_ ->
                Decode.decodeString Application.preferencesDecoder """{ "canvasHeight": null, "collapsedGroups": [ "x" ] }"""
                    |> Expect.equal (Ok (collapsed [ "x" ]))
        ]
