module Component.Application.Canvas exposing
    ( Config
    , Model
    , Msg
    , domId
    , init
    , isDark
    , isFullscreenToggle
    , reset
    , subscriptions
    , update
    , view
    , wheelAt
    , zoomPercent
    )

{-| The Playground preview canvas: a pannable, zoomable grid viewport for the
page's live component, in the manner of a design tool's canvas.

The canvas is plain DOM. The live component renders as itself inside a
**world** layer that carries one CSS transform (`translate` then `scale`, about
the viewport centre); the grid is the viewport's CSS-gradient background, sized
and offset from the same transform, so the two stay spatially locked. The
heading and toolbar sit outside the world layer, fixed to the viewport.

Canvas state (pan, zoom, tool, backdrop, fullscreen) is presentation only — it
never touches component state, so zooming, panning, switching backdrop or
going fullscreen never remounts or resets the component.


# Coordinates

The transform origin is the viewport centre, so a world point `w` (relative to
the centre) appears at `t + s·w`, where `t = (x, y)` is the pan and `s` the
scale. The default view (`t = 0`, `s = 1`) shows the content centred, and stays
centred when the viewport resizes (fullscreen, Inspector), so a resize keeps
the focal point. Zooming about a pointer at `p` keeps the world point under it
fixed: `t' = p - (p - t)·s'/s`.

-}

import Browser.Events
import Component.Application.Theme exposing (Theme)
import Component.Ui as Ui
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)



-- MODEL


type alias Model =
    { x : Float
    , y : Float
    , scale : Float
    , tool : Tool
    , backdrop : Backdrop
    , fullscreen : Bool
    , drag : Maybe Drag
    , recenter : Maybe Recenter
    }


type Tool
    = Select
    | Pan


type Backdrop
    = Light
    | Dark


{-| A pan gesture in progress: where the pointer was pressed (client px) and the
pan at that moment. `moved` turns on past a small slop, so a plain click on the
background is still a click (e.g. it closes an open popover).
-}
type alias Drag =
    { startX : Float
    , startY : Float
    , fromX : Float
    , fromY : Float
    , moved : Bool
    }


{-| A recenter animation in progress: the view it started from and the time
elapsed (ms). The current view is always written back to `x` / `y` / `scale`,
so interrupting it (a wheel or a pan) continues from where it is, with no jump.
-}
type alias Recenter =
    { fromX : Float
    , fromY : Float
    , fromScale : Float
    , elapsed : Float
    }


init : Model
init =
    { x = 0
    , y = 0
    , scale = 1
    , tool = Select
    , backdrop = Light
    , fullscreen = False
    , drag = Nothing
    , recenter = Nothing
    }


{-| Back to the default view for a new page, keeping the viewer's tool and
backdrop choices.
-}
reset : Model -> Model
reset model =
    { init | tool = model.tool, backdrop = model.backdrop }


minScale : Float
minScale =
    0.25


maxScale : Float
maxScale =
    4


{-| The DOM id of the canvas viewport, so the host can find it (e.g. to place
popovers anchored inside it on the canvas).
-}
domId : String
domId =
    "cp-canvas"


zoomPercent : Model -> Int
zoomPercent model =
    round (model.scale * 100)


{-| Whether the dark backdrop is selected (the heading over it swaps inks).
-}
isDark : Model -> Bool
isDark model =
    model.backdrop == Dark



-- UPDATE


type Msg
    = Wheel WheelEvent
    | PanStart Point
    | PanMove { x : Float, y : Float, buttons : Int }
    | PanEnd
    | SetTool Tool
    | SetBackdrop Backdrop
    | SetFullscreen Bool
    | RecenterView { reducedMotion : Bool }
    | AnimationFrame Float


type alias Point =
    { x : Float, y : Float }


{-| A wheel event over the canvas: the vertical delta in px, whether it is a
trackpad pinch (browsers report one as a wheel with `ctrlKey`), and the pointer
relative to the viewport centre.
-}
type alias WheelEvent =
    { delta : Float
    , pinch : Bool
    , px : Float
    , py : Float
    }


{-| Zoom from a wheel event that happened outside the canvas's DOM but over its
content — over a popover the host placed on the canvas: the vertical delta (px,
from `deltaY` / `deltaMode`), whether it is a pinch, and the pointer relative to
the canvas centre (screen px).
-}
wheelAt : { deltaY : Float, deltaMode : Int, pinch : Bool, x : Float, y : Float } -> Msg
wheelAt e =
    Wheel { delta = normaliseDelta e.deltaY e.deltaMode, pinch = e.pinch, px = e.x, py = e.y }


normaliseDelta : Float -> Int -> Float
normaliseDelta d mode =
    case mode of
        1 ->
            -- Lines.
            d * 16

        2 ->
            -- Pages.
            d * 400

        _ ->
            d


{-| Whether a message enters or leaves fullscreen — a layout change the shell
follows with a remeasure of the live components.
-}
isFullscreenToggle : Msg -> Bool
isFullscreenToggle msg =
    case msg of
        SetFullscreen _ ->
            True

        _ ->
            False


update : Theme -> Msg -> Model -> Model
update theme msg model =
    case msg of
        Wheel e ->
            zoomAt e.px e.py (zoomFactor e) { model | recenter = Nothing }

        PanStart p ->
            { model
                | drag = Just { startX = p.x, startY = p.y, fromX = model.x, fromY = model.y, moved = False }
                , recenter = Nothing
            }

        PanMove p ->
            case model.drag of
                Just d ->
                    if p.buttons == 0 then
                        -- Released outside the window: the mouseup never came.
                        { model | drag = Nothing }

                    else
                        let
                            dx =
                                p.x - d.startX

                            dy =
                                p.y - d.startY

                            moved =
                                d.moved || sqrt (dx * dx + dy * dy) >= panSlop
                        in
                        if moved then
                            { model | x = d.fromX + dx, y = d.fromY + dy, drag = Just { d | moved = True } }

                        else
                            model

                Nothing ->
                    model

        PanEnd ->
            { model | drag = Nothing }

        SetTool tool ->
            { model | tool = tool }

        SetBackdrop backdrop ->
            { model | backdrop = backdrop }

        SetFullscreen on ->
            { model | fullscreen = on, drag = Nothing }

        RecenterView { reducedMotion } ->
            if isDefaultView model || model.recenter /= Nothing then
                -- Already there, or already on the way: don't restart or queue.
                model

            else if reducedMotion then
                { model | x = 0, y = 0, scale = 1 }

            else
                { model | recenter = Just { fromX = model.x, fromY = model.y, fromScale = model.scale, elapsed = 0 } }

        AnimationFrame delta ->
            case model.recenter of
                Just r ->
                    let
                        elapsed =
                            r.elapsed + delta

                        progress =
                            min 1 (elapsed / max 1 theme.canvasMotion.durationMs)

                        eased =
                            cubicBezier theme.canvasMotion progress
                    in
                    if progress >= 1 then
                        { model | x = 0, y = 0, scale = 1, recenter = Nothing }

                    else
                        { model
                            | x = r.fromX * (1 - eased)
                            , y = r.fromY * (1 - eased)
                            , scale = r.fromScale + (1 - r.fromScale) * eased
                            , recenter = Just { r | elapsed = elapsed }
                        }

                Nothing ->
                    model


{-| Past this distance (px) a press on the background is a pan, not a click.
-}
panSlop : Float
panSlop =
    3


isDefaultView : Model -> Bool
isDefaultView model =
    model.x == 0 && model.y == 0 && model.scale == 1


{-| Zoom by `factor` about the pointer at (`px`, `py`) from the viewport centre,
keeping the world point under it in place.
-}
zoomAt : Float -> Float -> Float -> Model -> Model
zoomAt pointerX pointerY factor model =
    let
        scale =
            clamp minScale maxScale (model.scale * factor)

        ratio =
            scale / model.scale
    in
    { model
        | scale = scale
        , x = pointerX - (pointerX - model.x) * ratio
        , y = pointerY - (pointerY - model.y) * ratio
    }


{-| An exponential zoom step, so a step in feels the same at every zoom level.
Each event's delta is clamped, so a fast flick or an accelerated wheel can't
jump; a trackpad's small, frequent deltas give a smooth, continuous zoom, and a
pinch (much smaller deltas) gets a stronger gain.
-}
zoomFactor : WheelEvent -> Float
zoomFactor event =
    let
        gain =
            if event.pinch then
                0.01

            else
                0.002
    in
    e ^ (negate (clamp -60 60 event.delta) * gain)


{-| CSS `cubic-bezier(x1, y1, x2, y2)` at progress `p` (0–1): solve the curve's
x for `p`, then read its y — so the animation follows the host's motion token
exactly as a CSS transition would.
-}
cubicBezier : { a | x1 : Float, y1 : Float, x2 : Float, y2 : Float } -> Float -> Float
cubicBezier c p =
    let
        bez a b t =
            3 * (1 - t) * (1 - t) * t * a + 3 * (1 - t) * t * t * b + t * t * t

        solve lo hi n =
            let
                mid =
                    (lo + hi) / 2
            in
            if n == 0 then
                mid

            else if bez c.x1 c.x2 mid < p then
                solve mid hi (n - 1)

            else
                solve lo mid (n - 1)
    in
    bez c.y1 c.y2 (solve 0 1 20)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.recenter of
            Just _ ->
                Browser.Events.onAnimationFrameDelta AnimationFrame

            Nothing ->
                Sub.none
        , if model.fullscreen then
            Browser.Events.onKeyDown escape

          else
            Sub.none
        ]


{-| Escape leaves fullscreen — unless something inside handled it first (a
component closing its own popover calls `preventDefault`).
-}
escape : Decoder Msg
escape =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "defaultPrevented" Decode.bool)
        |> Decode.andThen
            (\( key, handled ) ->
                if key == "Escape" && not handled then
                    Decode.succeed (SetFullscreen False)

                else
                    Decode.fail "not an unhandled Escape"
            )



-- VIEW


type alias Config msg =
    { theme : Theme
    , toMsg : Msg -> msg

    -- Fixed to the viewport's top-left (the page heading, and a preset tab bar).
    , heading : Html msg

    -- Whether the heading carries a preset tab bar, so the heading band below
    -- which the component sits is taller.
    , headingTabs : Bool

    -- The live component, rendered as itself.
    , content : Html msg
    , previewSize : Maybe { width : Int, height : Int }
    }


{-| Breathing room around the live component, in grid squares: the heading band
above it, and three squares below.
-}
padTop : Float
padTop =
    5


padBottom : Float
padBottom =
    3


{-| The heading band with a preset tab bar under the heading.
-}
padTopTabs : Float
padTopTabs =
    7


{-| The regular (non-fullscreen) minimum height: room for a component's
anchored menus and popovers without the canvas resizing as they open.
-}
minHeight : Float
minHeight =
    480


view : Config msg -> Model -> Html msg
view config model =
    let
        theme =
            config.theme

        grid =
            theme.canvasGridSize

        ( bg, line ) =
            case model.backdrop of
                Light ->
                    ( theme.canvasBg, theme.canvasLine )

                Dark ->
                    ( theme.canvasDarkBg, theme.canvasDarkLine )

        tile =
            px (grid * model.scale)

        dragging =
            Maybe.map .moved model.drag == Just True
    in
    Html.div
        ([ Html.Attributes.id domId
         , Html.Attributes.class "cp-canvas"
         , background
         , Ui.style "overflow" "hidden"
         , Ui.style "background-color" bg
         , Ui.style "background-image"
            ("linear-gradient(to right, " ++ line ++ " 1px, transparent 1px), linear-gradient(to bottom, " ++ line ++ " 1px, transparent 1px)")
         , Ui.style "background-size" (tile ++ " " ++ tile)
         , Ui.style "background-position" ("calc(50% + " ++ px model.x ++ ") calc(50% + " ++ px model.y ++ ")")
         , Html.Events.preventDefaultOn "wheel" (wheelDecoder model.fullscreen |> Decode.map (\e -> ( config.toMsg (Wheel e), True )))
         , Html.Events.custom "mousedown" (panStartDecoder |> Decode.map (\p -> { message = config.toMsg (PanStart p), preventDefault = True, stopPropagation = False }))

         -- A pan's mouseup lands on the drag shield, so the browser's click goes
         -- to the viewport. Keep it from reaching the page, where it would read as
         -- an outside click (closing an open popover). A plain click without a pan
         -- still goes through. This handler is from the last render, which is
         -- still mid-pan when the click arrives.
         , Html.Events.stopPropagationOn "click"
            (if dragging then
                Decode.succeed ( config.toMsg PanEnd, True )

             else
                Decode.fail "not a pan"
            )
         ]
            ++ (if model.fullscreen then
                    [ Ui.style "position" "fixed"
                    , Ui.style "inset" "0"
                    , Ui.style "z-index" "400"
                    , Ui.style "display" "flex"
                    , Ui.style "flex-direction" "column"
                    ]

                else
                    [ Ui.style "position" "relative"
                    , Ui.style "border-bottom" ("1px solid " ++ theme.line)
                    , Ui.style "display" "flex"
                    , Ui.style "flex-direction" "column"

                    -- A template taller than the window doesn't stretch the page:
                    -- the canvas stops a little short of the window, and the
                    -- template is zoomed / panned to.
                    , Ui.style "max-height" "calc(100vh - 120px)"
                    ]
               )
            ++ (case model.tool of
                    Pan ->
                        [ Ui.style "cursor" "grab" ]

                    Select ->
                        []
               )
        )
        [ world config model
        , case model.tool of
            Pan ->
                -- Hand tool: a surface over the component, so a press anywhere pans
                -- rather than reaching the component.
                Html.div [ background, Ui.style "position" "absolute", Ui.style "inset" "0", Ui.style "z-index" "1" ] []

            Select ->
                Html.text ""
        , Html.div
            [ Ui.style "position" "absolute"
            , Ui.style "top" (px grid)
            , Ui.style "left" (px grid)
            , Ui.style "z-index" "2"

            -- The heading is transparent to the pointer, so a pan can start on it;
            -- interactive parts (a preset tab bar) opt back in.
            , Ui.style "pointer-events" "none"
            , Ui.style "max-width" "calc(100% - 420px)"
            ]
            [ config.heading ]
        , toolbar config model
        , case model.drag of
            Just _ ->
                dragShield config.toMsg

            Nothing ->
                Html.text ""
        ]


{-| The transformed layer holding the live component, centred in the viewport
below the heading band. Its box matches the viewport's, so the transform origin
(its centre) is the viewport centre. Content too big for it is aligned `safe`:
from the top-left, below the heading, rather than overflowing both ways.
-}
world : Config msg -> Model -> Html msg
world config model =
    let
        grid =
            config.theme.canvasGridSize

        footprint inner =
            case config.previewSize of
                Just size ->
                    -- The component at the footprint's top-left, so the group it
                    -- forms with its popover is what's centred.
                    Html.div
                        [ background
                        , Ui.style "display" "flex"
                        , Ui.style "flex-direction" "column"
                        , Ui.style "align-items" "flex-start"
                        , Ui.style "min-width" (px (toFloat size.width))
                        , Ui.style "min-height" (px (toFloat size.height))
                        ]
                        [ inner ]

                Nothing ->
                    inner
    in
    Html.div
        [ background
        , Ui.style "display" "flex"
        , Ui.style "flex-direction" "column"
        , Ui.style "justify-content" "safe center"
        , Ui.style "padding"
            (px
                (grid
                    * (if config.headingTabs then
                        padTopTabs

                       else
                        padTop
                      )
                )
                ++ " "
                ++ px (grid * 2)
                ++ " "
                ++ px (grid * padBottom)
            )
        , Ui.style "transform-origin" "50% 50%"
        , Ui.style "transform" ("translate(" ++ px model.x ++ ", " ++ px model.y ++ ") scale(" ++ String.fromFloat model.scale ++ ")")
        , Ui.style "flex" "1 1 auto"
        , Ui.style "min-height"
            (if model.fullscreen then
                "0"

             else
                px minHeight
            )
        ]
        [ Html.div
            [ background
            , Ui.style "width" "100%"
            , Ui.style "display" "flex"
            , Ui.style "flex-direction" "column"
            , Ui.style "align-items" "safe center"
            ]
            [ footprint config.content ]
        ]


{-| While a pan is in progress, a full-screen shield takes the pointer: it shows
the grabbing cursor everywhere, keeps the moves coming wherever the pointer goes
(even over the component or off the canvas) and stops the component reacting to
the pass-over. Moves report `buttons`, so a release outside the window still
ends the pan.
-}
dragShield : (Msg -> msg) -> Html msg
dragShield toMsg =
    Html.div
        [ Ui.style "position" "fixed"
        , Ui.style "inset" "0"
        , Ui.style "z-index" "10"
        , Ui.style "cursor" "grabbing"
        , Ui.style "user-select" "none"
        , Html.Events.on "mousemove"
            (Decode.map3 (\x y b -> toMsg (PanMove { x = x, y = y, buttons = b }))
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
                (Decode.field "buttons" Decode.int)
            )
        , Html.Events.on "mouseup" (Decode.succeed (toMsg PanEnd))
        ]
        []


{-| Marks the canvas's own empty surfaces (viewport, world, layout wrappers),
where a press pans in Select mode. Presses on the component itself never match.
-}
background : Html.Attribute msg
background =
    Html.Attributes.attribute "data-cp-canvas" ""


panStartDecoder : Decoder Point
panStartDecoder =
    Decode.map3 (\_ x y -> { x = x, y = y })
        (Decode.map2 Tuple.pair
            (Decode.at [ "target", "dataset", "cpCanvas" ] Decode.string)
            (Decode.field "button" Decode.int)
            |> Decode.andThen
                (\( _, button ) ->
                    if button == 0 then
                        Decode.succeed ()

                    else
                        Decode.fail "not a primary press"
                )
        )
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


{-| A wheel event, with the pointer relative to the viewport centre. The
viewport's position on screen comes from its offset chain less any scrolled
ancestors; fullscreen, it is the window.
-}
wheelDecoder : Bool -> Decoder WheelEvent
wheelDecoder fullscreen =
    let
        delta =
            Decode.map2 normaliseDelta
                (Decode.field "deltaY" Decode.float)
                (Decode.field "deltaMode" Decode.int)

        origin =
            if fullscreen then
                Decode.succeed ( 0, 0 )

            else
                Decode.map2 (\( ox, oy ) ( sx, sy ) -> ( ox - sx, oy - sy )) pageOffset scrollOffset

        box =
            Decode.field "currentTarget"
                (Decode.map3 (\( left, top ) w h -> { cx = left + w / 2, cy = top + h / 2 })
                    origin
                    (Decode.field "clientWidth" Decode.float)
                    (Decode.field "clientHeight" Decode.float)
                )
    in
    Decode.map5
        (\d pinch cx cy b -> { delta = d, pinch = pinch, px = cx - b.cx, py = cy - b.cy })
        delta
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        box


{-| An element's page offset: its `offsetLeft` / `offsetTop` summed up the
`offsetParent` chain (with each parent's border).
-}
pageOffset : Decoder ( Float, Float )
pageOffset =
    Decode.map3 (\l t parent -> ( l + Tuple.first parent, t + Tuple.second parent ))
        (Decode.field "offsetLeft" Decode.float)
        (Decode.field "offsetTop" Decode.float)
        (Decode.field "offsetParent"
            (Decode.nullable
                (Decode.lazy
                    (\_ ->
                        Decode.map3 (\bl bt ( l, t ) -> ( bl + l, bt + t ))
                            (Decode.field "clientLeft" Decode.float)
                            (Decode.field "clientTop" Decode.float)
                            pageOffset
                    )
                )
                |> Decode.map (Maybe.withDefault ( 0, 0 ))
            )
        )


{-| How far an element's ancestors are scrolled, summed up the parent chain.
-}
scrollOffset : Decoder ( Float, Float )
scrollOffset =
    Decode.field "parentElement"
        (Decode.nullable
            (Decode.lazy
                (\_ ->
                    Decode.map3 (\sl st ( l, t ) -> ( sl + l, st + t ))
                        (Decode.field "scrollLeft" Decode.float)
                        (Decode.field "scrollTop" Decode.float)
                        scrollOffset
                )
            )
        )
        |> Decode.map (Maybe.withDefault ( 0, 0 ))



-- TOOLBAR


{-| The canvas toolbar, fixed to the viewport's top-right: Select / Pan, the
zoom level and Recenter, the Light / Dark backdrop, and Fullscreen — which
becomes Close, in the same place, while fullscreen.
-}
toolbar : Config msg -> Model -> Html msg
toolbar config model =
    let
        theme =
            config.theme

        divider =
            Html.div
                [ Ui.style "width" "1px"
                , Ui.style "height" "20px"
                , Ui.style "margin" "0 4px"
                , Ui.style "background" theme.line
                ]
                []

        toggle label pressed msg icon =
            toolButton
                [ Html.Attributes.attribute "aria-pressed" (boolString pressed)
                , Html.Attributes.classList [ ( "is-active", pressed ) ]
                , Html.Events.onClick (config.toMsg msg)
                ]
                label
                [ icon ]
    in
    Html.div
        [ Html.Attributes.class "cp-canvas-toolbar"

        -- Toolbar clicks are canvas chrome, not presses on the page: keep them
        -- from reaching the page, where they'd read as an outside click and
        -- close the component's open popover (e.g. on entering fullscreen).
        , Html.Events.stopPropagationOn "click" (Decode.succeed ( config.toMsg PanEnd, True ))
        , Html.Attributes.attribute "role" "toolbar"
        , Html.Attributes.attribute "aria-label" "Canvas"
        , Ui.style "position" "absolute"
        , Ui.style "top" (px theme.canvasGridSize)
        , Ui.style "right" (px theme.canvasGridSize)
        , Ui.style "z-index" "3"
        , Ui.style "display" "flex"
        , Ui.style "align-items" "center"
        , Ui.style "gap" "2px"
        , Ui.style "padding" "4px"
        , Ui.style "background" theme.surface
        , Ui.style "border" ("1px solid " ++ theme.line)
        , Ui.style "border-radius" theme.radiusLg
        , Ui.style "box-shadow" theme.shadow2
        ]
        [ toggle "Select" (model.tool == Select) (SetTool Select) (Ui.phosphorCursor "")
        , toggle "Pan canvas" (model.tool == Pan) (SetTool Pan) (Ui.phosphorHand "")
        , divider
        , Html.span
            [ Html.Attributes.title "Current zoom level"
            , Html.Attributes.attribute "aria-label" ("Current zoom level " ++ String.fromInt (zoomPercent model) ++ "%")
            , Ui.style "width" "48px"
            , Ui.style "text-align" "center"
            , Ui.style "font-family" theme.fontFamily
            , Ui.style "font-size" "13px"
            , Ui.style "font-weight" "500"
            , Ui.style "font-variant-numeric" "tabular-nums"
            , Ui.style "color" theme.ink2
            , Ui.style "user-select" "none"
            ]
            [ Html.text (String.fromInt (zoomPercent model) ++ "%") ]
        , toolButton
            [ Html.Events.on "click"
                (Decode.at [ "currentTarget", "lastElementChild", "offsetWidth" ] Decode.float
                    |> Decode.map (\w -> config.toMsg (RecenterView { reducedMotion = w > 0 }))
                )
            ]
            "Recenter and reset zoom"
            [ Ui.phosphorCrosshair ""

            -- The reduced-motion probe: the shell stylesheet gives it a width only
            -- under `prefers-reduced-motion: reduce`, and the click reads it, so the
            -- recenter can skip its animation.
            , Html.span [ Html.Attributes.class "cp-motion-probe" ] []
            ]
        , divider
        , toggle "Light canvas" (model.backdrop == Light) (SetBackdrop Light) (Ui.phosphorSun "")
        , toggle "Dark canvas" (model.backdrop == Dark) (SetBackdrop Dark) (Ui.phosphorMoon "")
        , divider
        , if model.fullscreen then
            toolButton [ Html.Events.onClick (config.toMsg (SetFullscreen False)) ] "Exit fullscreen" [ Ui.phosphorX "" ]

          else
            toolButton [ Html.Events.onClick (config.toMsg (SetFullscreen True)) ] "Enter fullscreen" [ Ui.phosphorCornersOut "" ]
        ]


{-| An icon-only toolbar button: the label is its accessible name and tooltip.
-}
toolButton : List (Html.Attribute msg) -> String -> List (Html msg) -> Html msg
toolButton attrs label children =
    Html.button
        ([ Html.Attributes.type_ "button"
         , Html.Attributes.class "cp-canvas-btn"
         , Html.Attributes.title label
         , Html.Attributes.attribute "aria-label" label
         ]
            ++ attrs
        )
        children


boolString : Bool -> String
boolString b =
    if b then
        "true"

    else
        "false"


px : Float -> String
px n =
    String.fromFloat n ++ "px"
