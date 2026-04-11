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


{-| Record of colour and typography tokens used throughout the playground UI.

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
    }


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
