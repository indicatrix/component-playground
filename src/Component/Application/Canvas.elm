module Component.Application.Canvas exposing
    ( Config
    , Model
    , Msg
    , domId
    , init
    , isDark
    , isRelayout
    , regularHeight
    , reset
    , savedHeight
    , scrolled
    , subscriptions
    , toolbarId
    , update
    , view
    , wheelAt
    , withHeight
    , zoomPercent
    )

{-| The Playground preview canvas: a pannable, zoomable grid viewport for the
page's live component, in the manner of a design tool's canvas.

The canvas is plain DOM. The live component renders as itself inside a
**world** layer that carries one CSS transform (`translate` then `scale`, about
the viewport centre); the grid is the viewport's CSS-gradient background, sized
and offset from the same transform, so the two stay spatially locked. The
heading and toolbar sit outside the world layer, fixed to the viewport.

Canvas state (pan, zoom, tool, backdrop, fullscreen, height) is presentation
only — it never touches component state, so zooming, panning, resizing,
switching backdrop or going fullscreen never remounts or resets the component.


# Height

The regular canvas sizes itself to its component until the viewer drags the
handle on its bottom edge; from then on it keeps the height they chose — across
pages and fullscreen — which a host may persist (`savedHeight` / `withHeight`).
Dragging changes the canvas's real height, so the page below it moves with the
edge. The handle is outside the canvas's clipped box, so it isn't clipped, and
doesn't take part in pan or zoom.


# Wheel

A wheel over the canvas zooms it, unless the host has marked the event
`previewWheelOwned` before it arrives — the pointer is over a scrollable region
of the component, which scrolls instead. A pinch always zooms.


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

    -- The regular canvas's height as the viewer set it (px); `Nothing` sizes it
    -- to the component.
    , height : Maybe Float
    , resize : Maybe Resize

    -- How far the page region the canvas sits in is scrolled (px).
    , scroll : Float
    }


{-| A height drag in progress: where the pointer was pressed (client px), the
canvas's height then, and the tallest it may become.
-}
type alias Resize =
    { startY : Float
    , fromHeight : Float
    , maxHeight : Float
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
    , height = Nothing
    , resize = Nothing
    , scroll = 0
    }


{-| Back to the default view for a new page, keeping the viewer's tool,
backdrop and canvas height — workspace preferences, not page state.
-}
reset : Model -> Model
reset model =
    { init | tool = model.tool, backdrop = model.backdrop, height = model.height, scroll = model.scroll }


{-| Start from a saved canvas height (see `savedHeight`), kept within the
minimum.
-}
withHeight : Maybe Float -> Model -> Model
withHeight height model =
    { model | height = Maybe.map (max minResizeHeight) height }


{-| The canvas height to save as the viewer's preference: the height they set,
once they have let go of the handle (`Nothing` mid-drag, so a host saving on
change writes once per drag, and when they haven't set one).
-}
savedHeight : Model -> Maybe Float
savedHeight model =
    case model.resize of
        Just _ ->
            Nothing

        Nothing ->
            model.height


{-| The regular canvas's height while the viewer sets it — what it is on
screen — or `Nothing` while it sizes itself or is fullscreen.
-}
regularHeight : Model -> Maybe Float
regularHeight model =
    if model.fullscreen then
        Nothing

    else
        model.height


{-| The page region the canvas sits in scrolled to `top` (px).
-}
scrolled : Float -> Msg
scrolled =
    Scrolled


minScale : Float
minScale =
    0.25


maxScale : Float
maxScale =
    8


{-| The DOM id of the canvas viewport, so the host can find it (e.g. to place
popovers anchored inside it on the canvas).
-}
domId : String
domId =
    "cp-canvas"


{-| The DOM id of the canvas toolbar, which stays above the canvas's popovers.
-}
toolbarId : String
toolbarId =
    "cp-canvas-toolbar"


{-| The shortest the viewer can make the regular canvas: room for the heading,
the toolbar and a usable strip of canvas.
-}
minResizeHeight : Float
minResizeHeight =
    240


{-| The tallest the viewer can make the regular canvas, as a share of the
window's height.
-}
maxResizeShare : Float
maxResizeShare =
    0.9


{-| How far an arrow key moves the canvas's bottom edge: one grid square.
-}
resizeKeyStep : Theme -> Float
resizeKeyStep theme =
    theme.canvasGridSize


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
    | ResizeStart { y : Float, height : Float, windowHeight : Float }
    | ResizeMove { y : Float, buttons : Int }
    | ResizeEnd
    | ResizeBy { delta : Float, height : Float, windowHeight : Float }
    | Scrolled Float


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


{-| Whether a message settles a change to the canvas's size — entering or
leaving fullscreen, letting go of the height handle — a layout change the shell
follows with a remeasure of the live components.
-}
isRelayout : Msg -> Bool
isRelayout msg =
    case msg of
        SetFullscreen _ ->
            True

        ResizeEnd ->
            True

        ResizeBy _ ->
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
            { model | fullscreen = on, drag = Nothing, resize = Nothing }

        ResizeStart r ->
            if model.fullscreen then
                model

            else
                { model
                    | resize = Just { startY = r.y, fromHeight = r.height, maxHeight = max minResizeHeight (r.windowHeight * maxResizeShare) }
                    , drag = Nothing
                }

        ResizeMove p ->
            case model.resize of
                Just r ->
                    if p.buttons == 0 then
                        -- Released outside the window: the pointerup never came.
                        { model | resize = Nothing }

                    else
                        { model | height = Just (clamp minResizeHeight r.maxHeight (r.fromHeight + p.y - r.startY)) }

                Nothing ->
                    model

        ResizeEnd ->
            { model | resize = Nothing }

        ResizeBy r ->
            if model.fullscreen then
                model

            else
                { model | height = Just (clamp minResizeHeight (max minResizeHeight (r.windowHeight * maxResizeShare)) (r.height + r.delta)) }

        Scrolled top ->
            { model | scroll = top }

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
        dragging =
            Maybe.map .moved model.drag == Just True

        resizing =
            model.resize /= Nothing
    in
    -- The canvas, then (outside its clipped box) the height handle and, while a
    -- gesture is in progress, its shield. Every slot is always present, so the
    -- canvas — and the live component in it — never remounts.
    Html.div
        [ Ui.style "position" "relative"
        , Ui.style "flex-shrink" "0"

        -- A pan's or a resize's mouseup lands on the shield, so the browser's
        -- click goes to this wrapper (the common ancestor). Keep it from reaching
        -- the page, where it would read as an outside click (closing an open
        -- popover). A plain click without a gesture still goes through. This
        -- handler is from the last render, which is still mid-gesture when the
        -- click arrives.
        , Html.Events.stopPropagationOn "click"
            (if dragging || resizing then
                Decode.succeed ( config.toMsg PanEnd, True )

             else
                Decode.fail "not a gesture"
            )
        ]
        [ viewport config model
        , if model.fullscreen then
            Html.text ""

          else
            resizeHandle config model
        , case model.drag of
            Just _ ->
                dragShield config.toMsg

            Nothing ->
                Html.text ""
        , case model.resize of
            Just _ ->
                resizeShield config.toMsg

            Nothing ->
                Html.text ""
        ]


{-| The canvas's clipped viewport: the world, the hand surface, the heading and
the toolbar.
-}
viewport : Config msg -> Model -> Html msg
viewport config model =
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
                    ]
                        ++ (case model.height of
                                Just height ->
                                    -- The viewer's height, as they set it with the
                                    -- handle. Never animated: it follows the pointer.
                                    [ Ui.style "height" (px height), Ui.style "box-sizing" "border-box" ]

                                Nothing ->
                                    -- A template taller than the window doesn't
                                    -- stretch the page: the canvas stops a little
                                    -- short of the window, and the template is
                                    -- zoomed / panned to.
                                    [ Ui.style "max-height" "calc(100vh - 120px)" ]
                           )
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
                -- (`data-cp-hand` lets a host's wheel handling look through it to
                -- the component.)
                Html.div [ background, Html.Attributes.attribute "data-cp-hand" "", Ui.style "position" "absolute", Ui.style "inset" "0", Ui.style "z-index" "1" ] []

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
            (if model.fullscreen || model.height /= Nothing then
                -- Fullscreen fills the window; a height the viewer set is theirs.
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
(even over the component, its popovers or off the canvas) and stops the
component reacting to the pass-over. Moves report `buttons`, so a release
outside the window still ends the pan.
-}
dragShield : (Msg -> msg) -> Html msg
dragShield toMsg =
    Html.div
        [ Ui.style "position" "fixed"
        , Ui.style "inset" "0"
        , Ui.style "z-index" shieldZ
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


{-| A gesture's shield stacks above everything the canvas shows, its popovers
included (the host draws those in a layer above the page), so they can't take
the pointer from it mid-gesture.
-}
shieldZ : String
shieldZ =
    "1000"



-- HEIGHT HANDLE


{-| The bottom edge's height handle. A strip straddling the edge (half above,
half below) is the grab area, so the edge needn't be hit exactly; hovering it
reveals the handle — a small pill, centred — and tints the edge. It sits outside
the canvas's transformed and clipped layers: it never scales, pans or clips.

Pressing it starts a resize (with priority over any pan: the canvas never sees
the press); arrow keys step the edge a grid square when it has focus.

-}
resizeHandle : Config msg -> Model -> Html msg
resizeHandle config model =
    let
        currentHeight =
            -- The canvas: the handle's previous sibling.
            Decode.at [ "currentTarget", "previousElementSibling", "offsetHeight" ] Decode.float

        windowHeight =
            Decode.at [ "view", "innerHeight" ] Decode.float

        step =
            resizeKeyStep config.theme
    in
    Html.div
        [ Html.Attributes.class "cp-canvas-resize"
        , Html.Attributes.classList [ ( "is-dragging", model.resize /= Nothing ) ]
        , Html.Attributes.attribute "role" "separator"
        , Html.Attributes.attribute "aria-orientation" "horizontal"
        , Html.Attributes.attribute "aria-label" "Canvas height"
        , Html.Attributes.title "Drag to resize the canvas"
        , Html.Attributes.tabindex 0
        , Ui.style "position" "absolute"
        , Ui.style "left" "0"
        , Ui.style "right" "0"
        , Ui.style "top" ("calc(100% - " ++ px resizeReach ++ ")")
        , Ui.style "height" (px (resizeReach * 2))
        , Ui.style "z-index" "4"
        , Ui.style "cursor" "ns-resize"
        , Ui.style "touch-action" "none"

        -- Cancelling the pointerdown also cancels its compatibility mousedown:
        -- no text selection, and nothing below reads it as a press.
        , Html.Events.preventDefaultOn "pointerdown"
            (Decode.map2 Tuple.pair (Decode.field "button" Decode.int) (Decode.field "isPrimary" Decode.bool)
                |> Decode.andThen
                    (\( button, primary ) ->
                        if button == 0 && primary then
                            Decode.map3 (\y h w -> ( config.toMsg (ResizeStart { y = y, height = h, windowHeight = w }), True ))
                                (Decode.field "clientY" Decode.float)
                                currentHeight
                                windowHeight

                        else
                            Decode.fail "not a primary press"
                    )
            )
        , Html.Events.preventDefaultOn "keydown"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        case key of
                            "ArrowUp" ->
                                Decode.succeed (negate step)

                            "ArrowDown" ->
                                Decode.succeed step

                            _ ->
                                Decode.fail "not an arrow"
                    )
                |> Decode.andThen
                    (\delta ->
                        Decode.map2 (\h w -> ( config.toMsg (ResizeBy { delta = delta, height = h, windowHeight = w }), True ))
                            currentHeight
                            windowHeight
                    )
            )
        ]
        [ Html.div [ Html.Attributes.class "cp-canvas-resize-edge" ] []
        , Html.div [ Html.Attributes.class "cp-canvas-resize-handle" ]
            [ Html.div [ Html.Attributes.class "cp-canvas-resize-grip" ] []
            , Html.div [ Html.Attributes.class "cp-canvas-resize-grip" ] []
            ]
        ]


{-| How far the handle's grab area reaches either side of the bottom edge (px).
-}
resizeReach : Float
resizeReach =
    14


{-| While the height is being dragged, a full-screen shield takes the pointer,
as for a pan: the resize cursor everywhere, moves wherever the pointer goes, and
an end however the drag ends — release, cancel, or a move with no button held
(released outside the window).
-}
resizeShield : (Msg -> msg) -> Html msg
resizeShield toMsg =
    Html.div
        [ Html.Attributes.class "cp-canvas-resize-shield"
        , Ui.style "position" "fixed"
        , Ui.style "inset" "0"
        , Ui.style "z-index" shieldZ
        , Ui.style "cursor" "ns-resize"
        , Ui.style "user-select" "none"
        , Ui.style "touch-action" "none"
        , Html.Events.on "pointermove"
            (Decode.map2 (\y b -> toMsg (ResizeMove { y = y, buttons = b }))
                (Decode.field "clientY" Decode.float)
                (Decode.field "buttons" Decode.int)
            )
        , Html.Events.on "pointerup" (Decode.succeed (toMsg ResizeEnd))
        , Html.Events.on "pointercancel" (Decode.succeed (toMsg ResizeEnd))
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
    Decode.map2 Tuple.pair (Decode.field "ctrlKey" Decode.bool) owned
        |> Decode.andThen
            (\( pinch, isOwned ) ->
                if isOwned && not pinch then
                    -- A scrollable region of the component under the pointer: it
                    -- scrolls, natively, and the canvas stays put.
                    Decode.fail "the component owns this wheel"

                else
                    Decode.map4
                        (\d cx cy b -> { delta = d, pinch = pinch, px = cx - b.cx, py = cy - b.cy })
                        delta
                        (Decode.field "clientX" Decode.float)
                        (Decode.field "clientY" Decode.float)
                        box
            )


{-| Whether the host has claimed a wheel event for the component (see the module
docs).
-}
owned : Decoder Bool
owned =
    Decode.oneOf [ Decode.field "previewWheelOwned" Decode.bool, Decode.succeed False ]


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
        , Html.Attributes.id toolbarId

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
