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

    -- Sidebar slots
    , sidebarHeader : Html Never
    , sidebarFooter : Maybe (Html Never)
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
    , sidebarHeader = defaultSidebarHeader
    , sidebarFooter = Nothing
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
