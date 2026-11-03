module Component.Ui exposing
    ( button
    , disableAutocomplete
    , fullHeight
    , hStack
    , headingStyles
    , labelStyles
    , onClick
    , phosphorArrowsInLineVertical
    , phosphorArrowsOutLineVertical
    , phosphorBookOpen
    , phosphorCaretDown
    , phosphorCaretRight
    , phosphorCheck
    , phosphorCopy
    , phosphorCornersOut
    , phosphorCrosshair
    , phosphorCube
    , phosphorCursor
    , phosphorDotsNine
    , phosphorFlask
    , phosphorHand
    , phosphorHouse
    , phosphorInfo
    , phosphorMagnifyingGlass
    , phosphorSidebar
    , phosphorSlidersHorizontal
    , phosphorSquaresFour
    , phosphorX
    , select
    , style
    , subHeadingStyles
    , text
    , textField
    , vStack
    )

import Component.Application.Theme exposing (Theme)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Encode as Encode
import List.Extra as List
import Phosphor
import Svg exposing (Svg)
import Svg.Attributes as Attrs


fullHeight : List (Attribute msg)
fullHeight =
    [ style "height" "100vh" ]


textStyles : Theme -> List (Attribute msg)
textStyles theme =
    [ style "font-family" theme.fontFamily
    , style "font-weight" theme.bodyFontWeight
    , style "font-size" theme.bodyFontSize
    , style "color" theme.textColor
    ]


{-| Styles for de-emphasised labels — control field labels, builder group
labels, inactive sidebar links. Same typography as `textStyles` but uses
`theme.mutedTextColor` so the control's value or the active selection
dominates.
-}
labelStyles : Theme -> List (Attribute msg)
labelStyles theme =
    [ style "font-family" theme.fontFamily
    , style "font-weight" theme.bodyFontWeight
    , style "font-size" theme.bodyFontSize
    , style "color" theme.mutedTextColor
    ]


text : Theme -> List (Attribute msg) -> List (Html msg) -> Html msg
text theme attrs content =
    Html.div (textStyles theme ++ attrs) content


button : Theme -> List (Attribute msg) -> List (Html msg) -> Html msg
button theme attrs content =
    Html.button
        ([ style "background" "none"
         , style "border" "none"
         , style "cursor" "pointer"
         , style "padding" "0"
         , style "margin" "0"
         ]
            ++ textStyles theme
            ++ attrs
        )
        content


style : String -> String -> Attribute msg
style =
    Attributes.style


onClick : msg -> Attribute msg
onClick =
    Events.onClick


headingStyles : Theme -> List (Attribute msg)
headingStyles theme =
    [ style "font-family" theme.fontFamily
    , style "font-weight" theme.headingFontWeight
    , style "font-size" theme.headingFontSize
    , style "color" theme.textColor
    ]


subHeadingStyles : Theme -> List (Attribute msg)
subHeadingStyles theme =
    [ style "font-family" theme.fontFamily
    , style "font-weight" theme.subHeadingFontWeight
    , style "font-size" theme.subHeadingFontSize
    , style "color" theme.textColor
    ]


hStack : List (Attribute msg) -> List (Html msg) -> Html msg
hStack attrs =
    Html.div
        ([ style "display" "flex"
         , style "flex-direction" "row"
         ]
            ++ attrs
        )


vStack : List (Attribute msg) -> List (Html msg) -> Html msg
vStack attrs =
    Html.div
        ([ style "display" "flex"
         , style "flex-direction" "column"
         ]
            ++ attrs
        )


controlWidth : Attribute msg
controlWidth =
    style "width" "180px"


{-| Base attributes for the inspector's input controls (text fields, selects).
The box chrome and interaction states (border, radius, padding, background,
hover, focus, disabled) are owned by the `cp-control` class in the application
shell stylesheet, so they are token-driven and stay consistent with the rest of
the Inspector. Here we add only the control's typography (theme-driven).
-}
inputStyles : Theme -> List (Attribute msg)
inputStyles theme =
    Attributes.class "cp-control" :: textStyles theme


textField :
    Theme
    ->
        { msg : String -> msg
        , id : String
        , label : String
        , value : String
        , error : Maybe String
        }
    -> Html msg
textField theme c =
    let
        label =
            Html.label
                ([ Attributes.for c.id, style "flex-grow" "1" ]
                    ++ labelStyles theme
                )
                [ Html.text c.label ]

        ( attrs, errorBit ) =
            case c.error of
                Just err ->
                    -- An inline border overrides the cp-control class border, so
                    -- the error outline wins over the themed default.
                    ( [ style "border" ("2px solid " ++ theme.errorColor) ]
                    , [ Html.div (textStyles theme ++ [ style "font-style" "italic", style "margin-right" "8px", style "color" theme.errorColor ]) [ Html.text err ] ]
                    )

                Nothing ->
                    -- No inline border: the cp-control class owns the box chrome.
                    ( [], [] )

        input =
            Html.input
                (List.concat
                    [ [ Attributes.type_ "text"
                      , Attributes.id c.id
                      , Attributes.value c.value
                      , Events.onInput c.msg
                      , style "flex-shrink" "0"
                      , controlWidth
                      ]
                    , inputStyles theme
                    , attrs
                    ]
                )
                []
    in
    vStack
        [ style "width" "100%"
        , style "gap" "4px"
        ]
        (hStack
            [ style "align-items" "center"
            , style "width" "100%"
            , style "gap" "12px"
            ]
            [ label, input ]
            :: errorBit
        )


select :
    Theme
    ->
        { id : String
        , options : List { label : String, value : String }
        , label : String
        , value : String
        , msg : String -> msg
        }
    -> Html msg
select theme c =
    let
        label =
            Html.label
                ([ Attributes.for c.id, style "flex-grow" "1" ]
                    ++ labelStyles theme
                )
                [ Html.text c.label ]

        value =
            List.find (\o -> o.value == c.value) c.options
                |> Maybe.map .value
                |> Maybe.withDefault "<no matches>"

        input =
            -- Options need selected for first load: https://stackoverflow.com/a/48477367
            -- The selected uses value thereafter.
            Html.select
                (inputStyles theme
                    ++ [ Attributes.id c.id
                       , style "flex-shrink" "0"
                       , Events.onInput c.msg
                       , Attributes.value value
                       , controlWidth
                       ]
                )
                (List.map
                    (\o ->
                        Html.option
                            [ Attributes.value o.value
                            , Attributes.selected (value == o.value)
                            ]
                            [ Html.text o.label ]
                    )
                    (if value == "<no matches>" then
                        { label = "", value = "<no matches>" } :: c.options

                     else
                        c.options
                    )
                )
    in
    hStack
        [ style "align-items" "baseline"
        , style "background-color" theme.backgroundColor
        ]
        [ label, input ]


disableAutocomplete : Attribute msg
disableAutocomplete =
    Attributes.property "autocomplete" (Encode.string "off")


{-| Shared frame for the Phosphor icons below: the `phosphor-icons/phosphor-elm`
glyph at its Regular weight, filling its box (sized by the caller, e.g.
`iconBox`) and painted in `currentColor`, so each icon inherits the surrounding
text colour. Phosphor's builder also strokes in `currentColor`; its Regular
glyphs are filled shapes, so the stroke is turned off to keep them at their
drawn weight. Decorative: hidden from assistive technology, since every
control that carries one is labelled itself.

These are the generic playground-chrome icons (search, carets, breadcrumb home,
Inspector affordances, section glyphs). Product / brand icons are supplied by
the host application and are never sourced from here.

-}
phosphor : Phosphor.Icon -> String -> Svg msg
phosphor glyph class =
    glyph Phosphor.Regular
        |> Phosphor.withSize 100
        |> Phosphor.withSizeUnit "%"
        |> Phosphor.withClass class
        |> Phosphor.toHtml [ Attrs.stroke "none", Attributes.attribute "aria-hidden" "true" ]


{-| magnifying-glass — the sidebar search affordance.
-}
phosphorMagnifyingGlass : String -> Svg msg
phosphorMagnifyingGlass =
    phosphor Phosphor.magnifyingGlass


{-| arrows-in-line-vertical — the sidebar "Collapse all" control.
-}
phosphorArrowsInLineVertical : String -> Svg msg
phosphorArrowsInLineVertical =
    phosphor Phosphor.arrowsInLineVertical


{-| arrows-out-line-vertical — the sidebar "Expand all" control.
-}
phosphorArrowsOutLineVertical : String -> Svg msg
phosphorArrowsOutLineVertical =
    phosphor Phosphor.arrowsOutLineVertical


{-| caret-down — expanded sidebar group / open token group affordance.
-}
phosphorCaretDown : String -> Svg msg
phosphorCaretDown =
    phosphor Phosphor.caretDown


{-| caret-right — collapsed sidebar group / closed token group affordance.
-}
phosphorCaretRight : String -> Svg msg
phosphorCaretRight =
    phosphor Phosphor.caretRight


{-| x — Inspector close button.
-}
phosphorX : String -> Svg msg
phosphorX =
    phosphor Phosphor.x


{-| house — breadcrumb root.
-}
phosphorHouse : String -> Svg msg
phosphorHouse =
    phosphor Phosphor.house


{-| cube — neutral component glyph (page-heading chip, Inspector metadata).
-}
phosphorCube : String -> Svg msg
phosphorCube =
    phosphor Phosphor.cube


{-| copy — the Inspector identifier copy affordance.
-}
phosphorCopy : String -> Svg msg
phosphorCopy =
    phosphor Phosphor.copy


{-| check — the copied confirmation shown after a successful copy.
-}
phosphorCheck : String -> Svg msg
phosphorCheck =
    phosphor Phosphor.check


{-| sidebar-simple — the Inspector trigger in the top ribbon.
-}
phosphorSidebar : String -> Svg msg
phosphorSidebar =
    phosphor Phosphor.sidebar


{-| squares-four — sidebar category / navigation-group glyph.
-}
phosphorSquaresFour : String -> Svg msg
phosphorSquaresFour =
    phosphor Phosphor.squaresFour


{-| flask — labels the live Playground section.
-}
phosphorFlask : String -> Svg msg
phosphorFlask =
    phosphor Phosphor.flask


{-| book-open — labels the Reference section (peer to the Playground flask).
-}
phosphorBookOpen : String -> Svg msg
phosphorBookOpen =
    phosphor Phosphor.bookOpen


{-| sliders-horizontal — the component-settings glyph in the Inspector.
-}
phosphorSlidersHorizontal : String -> Svg msg
phosphorSlidersHorizontal =
    phosphor Phosphor.slidersHorizontal


{-| cursor — the preview canvas Select tool.
-}
phosphorCursor : String -> Svg msg
phosphorCursor =
    phosphor Phosphor.cursor


{-| hand — the preview canvas Pan tool.
-}
phosphorHand : String -> Svg msg
phosphorHand =
    phosphor Phosphor.hand


{-| crosshair — the preview canvas Recenter control.
-}
phosphorCrosshair : String -> Svg msg
phosphorCrosshair =
    phosphor Phosphor.crosshair


{-| dots-nine — show / hide the preview canvas's dot grid.
-}
phosphorDotsNine : String -> Svg msg
phosphorDotsNine =
    phosphor Phosphor.dotsNine


{-| info — the preview canvas's zoom hint.
-}
phosphorInfo : String -> Svg msg
phosphorInfo =
    phosphor Phosphor.info


{-| corners-out — enter the preview canvas fullscreen.
-}
phosphorCornersOut : String -> Svg msg
phosphorCornersOut =
    phosphor Phosphor.cornersOut
