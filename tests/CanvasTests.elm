module CanvasTests exposing (suite)

import Component.Application.Canvas as Canvas
import Component.Application.Theme as Theme
import Expect
import Html
import Html.Attributes
import Json.Encode as Encode
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector


suite : Test
suite =
    Test.describe "Preview canvas"
        [ heightTests
        , fullscreenTests
        , wheelTests
        ]


config : Canvas.Config Canvas.Msg
config =
    { theme = Theme.default
    , toMsg = identity
    , heading = Html.text "Heading"
    , headingTabs = False
    , content = Html.text "Component"
    , previewSize = Nothing
    }


{-| Fire an event on the first element matching `selector` in the canvas's
view, and apply the message it produces.
-}
fire : List Selector.Selector -> ( String, Encode.Value ) -> Canvas.Model -> Result String Canvas.Model
fire selector event model =
    Canvas.view config model
        |> Query.fromHtml
        |> Query.find selector
        |> Event.simulate event
        |> Event.toResult
        |> Result.map (\msg -> Canvas.update Theme.default msg model)


handle : List Selector.Selector
handle =
    [ Selector.class "cp-canvas-resize" ]


shield : List Selector.Selector
shield =
    [ Selector.class "cp-canvas-resize-shield" ]


{-| A press on the handle with the canvas `height` tall, in a window 1000px
high (so the canvas may grow to 900px).
-}
pressAt : Float -> Float -> ( String, Encode.Value )
pressAt y height =
    Event.custom "pointerdown"
        (Encode.object
            [ ( "button", Encode.int 0 )
            , ( "isPrimary", Encode.bool True )
            , ( "clientY", Encode.float y )
            , ( "currentTarget", Encode.object [ ( "previousElementSibling", Encode.object [ ( "offsetHeight", Encode.float height ) ] ) ] )
            , ( "view", Encode.object [ ( "innerHeight", Encode.float 1000 ) ] )
            ]
        )


moveTo : Float -> Int -> ( String, Encode.Value )
moveTo y buttons =
    Event.custom "pointermove" (Encode.object [ ( "clientY", Encode.float y ), ( "buttons", Encode.int buttons ) ])


release : ( String, Encode.Value )
release =
    Event.custom "pointerup" (Encode.object [])


{-| Drag the handle from y = 500 on a 400px canvas to `y`, and let go.
-}
dragTo : Float -> Canvas.Model -> Result String Canvas.Model
dragTo y model =
    fire handle (pressAt 500 400) model
        |> Result.andThen (fire shield (moveTo y 1))
        |> Result.andThen (fire shield release)


heightTests : Test
heightTests =
    Test.describe "height handle"
        [ Test.test "dragging down grows the canvas by the pointer's travel" <|
            \_ ->
                dragTo 600 Canvas.init
                    |> Result.map Canvas.savedHeight
                    |> Expect.equal (Ok (Just 500))
        , Test.test "dragging up shrinks it, no shorter than 240px" <|
            \_ ->
                dragTo -2000 Canvas.init
                    |> Result.map Canvas.savedHeight
                    |> Expect.equal (Ok (Just 240))
        , Test.test "dragging down stops at 90% of the window" <|
            \_ ->
                dragTo 5000 Canvas.init
                    |> Result.map Canvas.savedHeight
                    |> Expect.equal (Ok (Just 900))
        , Test.test "the height isn't offered for saving mid-drag" <|
            \_ ->
                fire handle (pressAt 500 400) Canvas.init
                    |> Result.andThen (fire shield (moveTo 550 1))
                    |> Result.map (\m -> ( Canvas.savedHeight m, Canvas.regularHeight m ))
                    |> Expect.equal (Ok ( Nothing, Just 450 ))
        , Test.test "a move with no button held (released outside the window) ends the drag where it was" <|
            \_ ->
                fire handle (pressAt 500 400) Canvas.init
                    |> Result.andThen (fire shield (moveTo 550 1))
                    |> Result.andThen (fire shield (moveTo 700 0))
                    |> Result.map Canvas.savedHeight
                    |> Expect.equal (Ok (Just 450))
        , Test.test "the shield is gone once the drag ends" <|
            \_ ->
                case dragTo 600 Canvas.init of
                    Ok model ->
                        Canvas.view config model |> Query.fromHtml |> Query.findAll shield |> Query.count (Expect.equal 0)

                    Err e ->
                        Expect.fail e
        , Test.test "a new page keeps the viewer's height" <|
            \_ ->
                Canvas.init
                    |> Canvas.withHeight (Just 520)
                    |> Canvas.reset
                    |> Canvas.savedHeight
                    |> Expect.equal (Just 520)
        , Test.test "a saved height below the minimum is raised to it" <|
            \_ ->
                Canvas.init
                    |> Canvas.withHeight (Just 20)
                    |> Canvas.savedHeight
                    |> Expect.equal (Just 240)
        , Test.test "the arrow keys step the edge a grid square" <|
            \_ ->
                fire handle
                    (Event.custom "keydown"
                        (Encode.object
                            [ ( "key", Encode.string "ArrowDown" )
                            , ( "currentTarget", Encode.object [ ( "previousElementSibling", Encode.object [ ( "offsetHeight", Encode.float 400 ) ] ) ] )
                            , ( "view", Encode.object [ ( "innerHeight", Encode.float 1000 ) ] )
                            ]
                        )
                    )
                    Canvas.init
                    |> Result.map Canvas.savedHeight
                    |> Expect.equal (Ok (Just (400 + Theme.default.canvasGridSize)))
        ]


enterFullscreen : Canvas.Model -> Result String Canvas.Model
enterFullscreen =
    fire [ Selector.attribute (Html.Attributes.attribute "aria-label" "Enter fullscreen") ] Event.click


fullscreenTests : Test
fullscreenTests =
    Test.describe "fullscreen"
        [ Test.test "has no height handle" <|
            \_ ->
                case enterFullscreen Canvas.init of
                    Ok model ->
                        Canvas.view config model |> Query.fromHtml |> Query.findAll handle |> Query.count (Expect.equal 0)

                    Err e ->
                        Expect.fail e
        , Test.test "keeps the regular height for when it ends" <|
            \_ ->
                Canvas.init
                    |> Canvas.withHeight (Just 520)
                    |> enterFullscreen
                    |> Result.map (\m -> ( Canvas.regularHeight m, Canvas.savedHeight m ))
                    |> Expect.equal (Ok ( Nothing, Just 520 ))
        ]


{-| A wheel over the (fullscreen, so window-sized) canvas.
-}
wheel : Bool -> ( String, Encode.Value )
wheel owned =
    Event.custom "wheel"
        (Encode.object
            ([ ( "deltaY", Encode.float -100 )
             , ( "deltaMode", Encode.int 0 )
             , ( "ctrlKey", Encode.bool False )
             , ( "clientX", Encode.float 500 )
             , ( "clientY", Encode.float 500 )
             , ( "currentTarget", Encode.object [ ( "clientWidth", Encode.float 1000 ), ( "clientHeight", Encode.float 1000 ) ] )
             ]
                ++ (if owned then
                        [ ( "previewWheelOwned", Encode.bool True ) ]

                    else
                        []
                   )
            )
        )


wheelTests : Test
wheelTests =
    Test.describe "wheel"
        [ Test.test "zooms the canvas" <|
            \_ ->
                enterFullscreen Canvas.init
                    |> Result.andThen (fire [ Selector.id Canvas.domId ] (wheel False))
                    |> Result.map Canvas.zoomPercent
                    |> Result.map (\z -> z > 100)
                    |> Expect.equal (Ok True)
        , Test.test "leaves a wheel the component owns alone" <|
            \_ ->
                enterFullscreen Canvas.init
                    |> Result.andThen (fire [ Selector.id Canvas.domId ] (wheel True))
                    |> Result.map Canvas.zoomPercent
                    |> Expect.err
        ]
