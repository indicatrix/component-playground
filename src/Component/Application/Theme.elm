module Component.Application.Theme exposing
    ( Theme
    , default, dark, blueprint
    )

{-| Visual theme for the Component Playground application chrome.

Pass a `Theme` to `Component.Application.init` and
`Component.Application.element` to control colours and typography throughout
the sidebar, panels, and control widgets.

Build a custom theme by updating `default`:

    myTheme : Theme
    myTheme =
        { Theme.default | fontFamily = "Georgia, serif" }


# Type

@docs Theme


# Built-in themes

@docs default, dark, blueprint

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Svg
import Svg.Attributes as SvgAttrs


{-| Record of colour and typography tokens used throughout the playground Ui.

**Chrome / layout**

  - `backgroundColor` — the single surface colour for sidebar and page
    (`#f4f4f4`). The design is flat — no distinct panel colour.
  - `dividerColor` — thin border/divider colour used between sections, at
    the sidebar/page seam, and on input borders (`#b7b7b7`).

**Typography**

  - `fontFamily` — font-family string used everywhere (`Arial`).
  - `textColor` — primary text colour (`#1f1f1f`).
  - `mutedTextColor` — secondary / muted text colour used for search
    placeholder, control labels, etc. (`#707070`).
  - `bodyFontSize` — base font size (`15px`).
  - `bodyFontWeight` — base font weight (`400`).
  - `headingFontSize` — heading font size (`18px`).
  - `headingFontWeight` — heading font weight (`700`).
  - `subHeadingFontSize` — sub-heading font size (`16px`).
  - `subHeadingFontWeight` — sub-heading font weight (`400`).

**Controls**

  - `errorColor` — colour for validation errors (`#f66`).

**Sidebar slots**

  - `sidebarHeader` — Html rendered in the sidebar's top band. Default is
    a lucide `blocks` icon next to the text "Component Playground".
  - `sidebarFooter` — Optional Html rendered pinned to the bottom of the
    sidebar. When `Nothing`, the footer band is not rendered and the
    component index grows to fill the space. Default is `Nothing`.

**Preview canvas**

The live Playground preview sits on a pannable, zoomable grid canvas.

  - `canvasBg` / `canvasLine` — the light canvas background and its grid lines.
  - `canvasDarkBg` / `canvasDarkLine` — the dark canvas background and its grid
    lines. The light / dark choice is the canvas backdrop only; the previewed
    component keeps its own theme.
  - `canvasDarkInk` / `canvasDarkInk2` — the canvas heading's title / eyebrow
    on the dark backdrop.
  - `canvasGridSize` — the grid square, in px at 100% zoom.
  - `canvasMotion` — the recenter animation: a duration (ms) and a
    `cubic-bezier(x1, y1, x2, y2)` easing. Map it to the host's spatial motion
    token.
  - `canvasFadeMotion` — the height handle's fade in / out, as a CSS
    `<duration> <easing>`. Map it to the host's short interaction motion token.

**Page layout**

  - `referenceHeading` — whether a configurable page opens the content below
    its live Playground callout with the "Reference" section heading. Default
    `True`; `False` flows straight from the callout into the page's sections.

**Inspector**

  - `inspectorTokens` — whether the Inspector shows the "Design Tokens · Used by
    this configuration" section (the tokens a component declares with
    `Component.withTokens`). Default `True`; `False` leaves the Inspector to the
    component's metadata and settings.

-}
type alias Theme =
    { -- Chrome / layout
      backgroundColor : String
    , dividerColor : String

    -- Typography
    , fontFamily : String
    , textColor : String
    , mutedTextColor : String
    , bodyFontSize : String
    , bodyFontWeight : String
    , headingFontSize : String
    , headingFontWeight : String
    , subHeadingFontSize : String
    , subHeadingFontWeight : String

    -- Controls
    , errorColor : String

    -- Design tokens (Inspector skin)
    --
    -- The palette the application chrome and the Inspector controls are skinned
    -- from. These are host-overridable so a consuming app (e.g. Planwisely) can
    -- map every value to its own design-system tokens (`--pw-*`) without the
    -- library carrying app-specific constants. `default` ships a neutral light
    -- palette.
    , appBg : String
    , sidebar : String
    , surface : String
    , surfaceAlt : String
    , line : String
    , line2 : String
    , borderHover : String
    , ink : String
    , ink2 : String
    , ink3 : String
    , ink4 : String
    , brandBlue : String
    , accent : String
    , brandBlue50 : String
    , space2 : String
    , space3 : String
    , space4 : String
    , radiusSm : String
    , radiusMd : String
    , radiusLg : String
    , shadow1 : String
    , shadow2 : String
    , shadow4 : String

    -- Preview canvas
    , canvasBg : String
    , canvasLine : String
    , canvasDarkBg : String
    , canvasDarkLine : String
    , canvasDarkInk : String
    , canvasDarkInk2 : String
    , canvasGridSize : Float
    , canvasMotion : { durationMs : Float, x1 : Float, y1 : Float, x2 : Float, y2 : Float }
    , canvasFadeMotion : String

    -- Sidebar slots
    , sidebarHeader : Html Never
    , sidebarFooter : Maybe (Html Never)

    -- Page layout
    , referenceHeading : Bool

    -- Inspector
    , inspectorTokens : Bool
    }


{-| The default light theme matching the Figma reference.
-}
default : Theme
default =
    { backgroundColor = "#f4f4f4"
    , dividerColor = "#b7b7b7"
    , fontFamily = "Arial"
    , textColor = "#1f1f1f"
    , mutedTextColor = "#707070"
    , bodyFontSize = "15px"
    , bodyFontWeight = "400"
    , headingFontSize = "18px"
    , headingFontWeight = "700"
    , subHeadingFontSize = "16px"
    , subHeadingFontWeight = "400"
    , errorColor = "#f66"
    , appBg = "#FBFBFC"
    , sidebar = "#F8FAF9"
    , surface = "#FEFEFE"
    , surfaceAlt = "#F1F0F5"
    , line = "#E5E8EC"
    , line2 = "#EEF0F3"
    , borderHover = "#B8C0CC"
    , ink = "#0A0F22"
    , ink2 = "#3A4149"
    , ink3 = "#5A5D66"
    , ink4 = "#9DA1AC"
    , brandBlue = "#2F7FFE"
    , accent = "#0E53F1"
    , brandBlue50 = "#EAF1FF"
    , space2 = "8px"
    , space3 = "12px"
    , space4 = "16px"
    , radiusSm = "4px"
    , radiusMd = "8px"
    , radiusLg = "10px"
    , shadow1 = "0 1px 2px rgba(16,24,40,0.05)"
    , shadow2 = "0 2px 4px rgba(16,24,40,0.06), 0 4px 8px rgba(16,24,40,0.04)"
    , shadow4 = "0 8px 16px rgba(16,24,40,0.08), 0 24px 48px rgba(16,24,40,0.12)"
    , canvasBg = "#F7F8FA"
    , canvasLine = "#E5E8EC"
    , canvasDarkBg = "#202326"
    , canvasDarkLine = "#2D3136"
    , canvasDarkInk = "#FFFFFF"
    , canvasDarkInk2 = "#8A94A0"
    , canvasGridSize = 24
    , canvasMotion = { durationMs = 200, x1 = 0, y1 = 0, x2 = 0.2, y2 = 1 }
    , canvasFadeMotion = "100ms cubic-bezier(0, 0, 0.2, 1)"
    , sidebarHeader = defaultSidebarHeader
    , sidebarFooter = Nothing
    , referenceHeading = True
    , inspectorTokens = True
    }


defaultSidebarHeader : Html Never
defaultSidebarHeader =
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "align-items" "center"
        , Attributes.style "gap" "12px"
        ]
        [ Html.div
            [ Attributes.style "width" "24px"
            , Attributes.style "height" "24px"
            , Attributes.style "flex-shrink" "0"
            ]
            [ phosphorSquaresFourSvg ]
        , Html.span [] [ Html.text "Component Playground" ]
        ]


{-| Phosphor `squares-four` — the default playground-chrome logo glyph, used only
when the host application does not supply its own `sidebarHeader` (e.g.
Planwisely substitutes its product logo here). A filled 256×256 glyph painted in
`currentColor`.
-}
phosphorSquaresFourSvg : Html Never
phosphorSquaresFourSvg =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 256 256"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.width "100%"
        , SvgAttrs.height "100%"
        ]
        [ Svg.path
            [ SvgAttrs.d "M104,40H56A16,16,0,0,0,40,56v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V56A16,16,0,0,0,104,40Zm0,64H56V56h48v48Zm96-64H152a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V56A16,16,0,0,0,200,40Zm0,64H152V56h48v48Zm-96,32H56a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V152A16,16,0,0,0,104,136Zm0,64H56V152h48v48Zm96-64H152a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V152A16,16,0,0,0,200,136Zm0,64H152V152h48v48Z" ]
            []
        ]


{-| Dark theme — swaps backgrounds and text for a dark-mode appearance.
-}
dark : Theme
dark =
    { default
        | backgroundColor = "#1a1a1a"
        , dividerColor = "#444"
        , textColor = "#eee"
        , mutedTextColor = "#aaa"
    }


{-| Blueprint theme — deep blue-tinted scheme with a technical feel.
-}
blueprint : Theme
blueprint =
    { default
        | backgroundColor = "#0d1b2a"
        , dividerColor = "#2a4a6a"
        , textColor = "#c0d8f0"
        , mutedTextColor = "#7d9cbb"
        , errorColor = "#ff6b6b"
        , fontFamily = "monospace"
    }
