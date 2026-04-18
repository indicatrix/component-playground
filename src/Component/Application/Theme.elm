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
  - `activeLinkBackground` — highlight on the selected nav link
    (`rgba(255,255,255,0.7)`).

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
    , activeLinkBackground : String

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
    , activeLinkBackground = "rgba(255, 255, 255, 0.7)"
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
            [ lucideBlocksSvg ]
        , Html.span [] [ Html.text "Component Playground" ]
        ]


lucideBlocksSvg : Html Never
lucideBlocksSvg =
    Svg.svg
        [ SvgAttrs.viewBox "0 0 24 24"
        , SvgAttrs.fill "none"
        , SvgAttrs.width "100%"
        , SvgAttrs.height "100%"
        ]
        [ Svg.path
            [ SvgAttrs.d "M10 21V8C10 7.73478 9.89464 7.48043 9.70711 7.29289C9.51957 7.10536 9.26522 7 9 7H4C3.73478 7 3.48043 7.10536 3.29289 7.29289C3.10536 7.48043 3 7.73478 3 8V20C3 20.2652 3.10536 20.5196 3.29289 20.7071C3.48043 20.8946 3.73478 21 4 21H16C16.2652 21 16.5196 20.8946 16.7071 20.7071C16.8946 20.5196 17 20.2652 17 20V15C17 14.7348 16.8946 14.4804 16.7071 14.2929C16.5196 14.1054 16.2652 14 16 14H3M15 3H20C20.5523 3 21 3.44772 21 4V9C21 9.55228 20.5523 10 20 10H15C14.4477 10 14 9.55228 14 9V4C14 3.44772 14.4477 3 15 3Z"
            , SvgAttrs.stroke "currentColor"
            , SvgAttrs.strokeWidth "2"
            , SvgAttrs.strokeLinecap "round"
            , SvgAttrs.strokeLinejoin "round"
            , Attributes.attribute "vector-effect" "non-scaling-stroke"
            ]
            []
        ]


{-| Dark theme — swaps backgrounds and text for a dark-mode appearance.
-}
dark : Theme
dark =
    { default
        | backgroundColor = "#1a1a1a"
        , dividerColor = "#444"
        , activeLinkBackground = "rgba(255, 255, 255, 0.08)"
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
        , activeLinkBackground = "rgba(192, 216, 240, 0.12)"
        , textColor = "#c0d8f0"
        , mutedTextColor = "#7d9cbb"
        , errorColor = "#ff6b6b"
        , fontFamily = "monospace"
    }
