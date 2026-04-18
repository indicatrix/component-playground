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

  - `pageBackground` — outermost wrapper background (`#eee`)
  - `panelBackground` — sidebar and content panel background (`#fff`)
  - `shadowColor` — box-shadow colour applied to panels (`#aaa`)
  - `sidebarDivider` — border beneath the sidebar header (`rgb(204, 204, 204)`)
  - `activeLinkBackground` — highlight on the selected nav link (`#eee`)

**Typography**

  - `fontFamily` — font-family string used everywhere (`Arial`)
  - `textColor` — primary text colour (`#222`)
  - `bodyFontSize` — base font size (`14px`)
  - `bodyFontWeight` — base font weight (`400`)
  - `headingFontSize` — heading font size (`20px`)
  - `headingFontWeight` — heading font weight (`600`)
  - `subHeadingFontSize` — sub-heading font size (`16px`)
  - `subHeadingFontWeight` — sub-heading font weight (`500`)

**Controls**

  - `inputBorderColor` — border colour for text fields and selects (`#ddd`)
  - `errorColor` — colour for validation errors (`#f66`)

**Sidebar slots**

  - `sidebarHeader` — Html rendered in the sidebar's top band. Default is
    a lucide `blocks` icon next to the text "Component Playground".
  - `sidebarFooter` — Html rendered pinned to the bottom of the sidebar.
    Default is empty.

-}
type alias Theme =
    { -- Chrome / layout
      pageBackground : String
    , panelBackground : String
    , shadowColor : String
    , sidebarDivider : String
    , activeLinkBackground : String

    -- Typography
    , fontFamily : String
    , textColor : String
    , bodyFontSize : String
    , bodyFontWeight : String
    , headingFontSize : String
    , headingFontWeight : String
    , subHeadingFontSize : String
    , subHeadingFontWeight : String

    -- Controls
    , inputBorderColor : String
    , errorColor : String

    -- Sidebar slots
    , sidebarHeader : Html Never
    , sidebarFooter : Html Never
    }


{-| The default light theme matching the original hardcoded styles.
-}
default : Theme
default =
    { pageBackground = "#eee"
    , panelBackground = "#fff"
    , shadowColor = "#aaa"
    , sidebarDivider = "rgb(204, 204, 204)"
    , activeLinkBackground = "#eee"
    , fontFamily = "Arial"
    , textColor = "#222"
    , bodyFontSize = "14px"
    , bodyFontWeight = "400"
    , headingFontSize = "20px"
    , headingFontWeight = "600"
    , subHeadingFontSize = "16px"
    , subHeadingFontWeight = "500"
    , inputBorderColor = "#ddd"
    , errorColor = "#f66"
    , sidebarHeader = defaultSidebarHeader
    , sidebarFooter = Html.text ""
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
        | pageBackground = "#1a1a1a"
        , panelBackground = "#2a2a2a"
        , shadowColor = "#000"
        , sidebarDivider = "#444"
        , activeLinkBackground = "#333"
        , textColor = "#eee"
        , inputBorderColor = "#555"
    }


{-| Blueprint theme — deep blue-tinted scheme with a technical feel.
-}
blueprint : Theme
blueprint =
    { default
        | pageBackground = "#0d1b2a"
        , panelBackground = "#1b2e45"
        , shadowColor = "#0a1520"
        , sidebarDivider = "#2a4a6a"
        , activeLinkBackground = "#1e3a5f"
        , textColor = "#c0d8f0"
        , inputBorderColor = "#2a5080"
        , errorColor = "#ff6b6b"
        , fontFamily = "monospace"
    }
