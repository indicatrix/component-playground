module Component.Ui exposing
    ( button
    , disableAutocomplete
    , fullHeight
    , hStack
    , headingStyles
    , labelStyles
    , onClick
    , phosphorBookOpen
    , phosphorCaretDown
    , phosphorCaretRight
    , phosphorCheck
    , phosphorCopy
    , phosphorCube
    , phosphorFlask
    , phosphorHouse
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


{-| Shared frame for the Phosphor icons below. Phosphor "regular" glyphs are
filled shapes on a 256×256 viewBox, painted in `currentColor` so each icon
inherits the surrounding text colour. Each icon supplies its own path `d`
strings; everything else is uniform.

These are the generic playground-chrome icons (search, carets, breadcrumb home,
Inspector affordances, section glyphs). Product / brand icons are supplied by
the host application and are never sourced from here.

-}
phosphorIcon : String -> List String -> Svg msg
phosphorIcon class ds =
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox "0 0 256 256"
        , Attrs.fill "currentColor"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        (List.map (\d -> Svg.path [ Attrs.d d ] []) ds)


{-| magnifying-glass — the sidebar search affordance.
-}
phosphorMagnifyingGlass : String -> Svg msg
phosphorMagnifyingGlass class =
    phosphorIcon class
        [ "M229.66,218.34l-50.07-50.06a88.11,88.11,0,1,0-11.31,11.31l50.06,50.07a8,8,0,0,0,11.32-11.32ZM40,112a72,72,0,1,1,72,72A72.08,72.08,0,0,1,40,112Z" ]


{-| caret-down — expanded sidebar group / open token group affordance.
-}
phosphorCaretDown : String -> Svg msg
phosphorCaretDown class =
    phosphorIcon class
        [ "M213.66,101.66l-80,80a8,8,0,0,1-11.32,0l-80-80A8,8,0,0,1,53.66,90.34L128,164.69l74.34-74.35a8,8,0,0,1,11.32,11.32Z" ]


{-| caret-right — collapsed sidebar group / closed token group affordance.
-}
phosphorCaretRight : String -> Svg msg
phosphorCaretRight class =
    phosphorIcon class
        [ "M181.66,133.66l-80,80a8,8,0,0,1-11.32-11.32L164.69,128,90.34,53.66a8,8,0,0,1,11.32-11.32l80,80A8,8,0,0,1,181.66,133.66Z" ]


{-| x — Inspector close button.
-}
phosphorX : String -> Svg msg
phosphorX class =
    phosphorIcon class
        [ "M205.66,194.34a8,8,0,0,1-11.32,11.32L128,139.31,61.66,205.66a8,8,0,0,1-11.32-11.32L116.69,128,50.34,61.66A8,8,0,0,1,61.66,50.34L128,116.69l66.34-66.35a8,8,0,0,1,11.32,11.32L139.31,128Z" ]


{-| house — breadcrumb root.
-}
phosphorHouse : String -> Svg msg
phosphorHouse class =
    phosphorIcon class
        [ "M219.31,108.68l-80-80a16,16,0,0,0-22.62,0l-80,80A15.87,15.87,0,0,0,32,120v96a8,8,0,0,0,8,8h64a8,8,0,0,0,8-8V160h32v56a8,8,0,0,0,8,8h64a8,8,0,0,0,8-8V120A15.87,15.87,0,0,0,219.31,108.68ZM208,208H160V152a8,8,0,0,0-8-8H104a8,8,0,0,0-8,8v56H48V120l80-80,80,80Z" ]


{-| cube — neutral component glyph (page-heading chip, Inspector metadata).
-}
phosphorCube : String -> Svg msg
phosphorCube class =
    phosphorIcon class
        [ "M223.68,66.15,135.68,18h0a15.88,15.88,0,0,0-15.36,0l-88,48.17a16,16,0,0,0-8.32,14v95.64a16,16,0,0,0,8.32,14l88,48.17a15.88,15.88,0,0,0,15.36,0l88-48.17a16,16,0,0,0,8.32-14V80.18A16,16,0,0,0,223.68,66.15ZM128,32h0l80.34,44L128,120,47.66,76ZM40,90l80,43.78v85.79L40,175.82Zm96,129.57V133.82L216,90v85.78Z" ]


{-| copy — the Inspector identifier copy affordance.
-}
phosphorCopy : String -> Svg msg
phosphorCopy class =
    phosphorIcon class
        [ "M216,32H88a8,8,0,0,0-8,8V80H40a8,8,0,0,0-8,8V216a8,8,0,0,0,8,8H168a8,8,0,0,0,8-8V176h40a8,8,0,0,0,8-8V40A8,8,0,0,0,216,32ZM160,208H48V96H160Zm48-48H176V88a8,8,0,0,0-8-8H96V48H208Z" ]


{-| check — the copied confirmation shown after a successful copy.
-}
phosphorCheck : String -> Svg msg
phosphorCheck class =
    phosphorIcon class
        [ "M229.66,77.66l-128,128a8,8,0,0,1-11.32,0l-56-56a8,8,0,0,1,11.32-11.32L96,188.69,218.34,66.34a8,8,0,0,1,11.32,11.32Z" ]


{-| sidebar-simple — the Inspector trigger in the top ribbon.
-}
phosphorSidebar : String -> Svg msg
phosphorSidebar class =
    phosphorIcon class
        [ "M216,40H40A16,16,0,0,0,24,56V200a16,16,0,0,0,16,16H216a16,16,0,0,0,16-16V56A16,16,0,0,0,216,40ZM40,56H80V200H40ZM216,200H96V56H216V200Z" ]


{-| squares-four — sidebar category / navigation-group glyph.
-}
phosphorSquaresFour : String -> Svg msg
phosphorSquaresFour class =
    phosphorIcon class
        [ "M104,40H56A16,16,0,0,0,40,56v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V56A16,16,0,0,0,104,40Zm0,64H56V56h48v48Zm96-64H152a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V56A16,16,0,0,0,200,40Zm0,64H152V56h48v48Zm-96,32H56a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V152A16,16,0,0,0,104,136Zm0,64H56V152h48v48Zm96-64H152a16,16,0,0,0-16,16v48a16,16,0,0,0,16,16h48a16,16,0,0,0,16-16V152A16,16,0,0,0,200,136Zm0,64H152V152h48v48Z" ]


{-| flask — labels the live Playground section.
-}
phosphorFlask : String -> Svg msg
phosphorFlask class =
    phosphorIcon class
        [ "M221.69,199.77,160,96.92V40h8a8,8,0,0,0,0-16H88a8,8,0,0,0,0,16h8V96.92L34.31,199.77A16,16,0,0,0,48,224H208a16,16,0,0,0,13.72-24.23ZM110.86,103.25A7.93,7.93,0,0,0,112,99.14V40h32V99.14a7.93,7.93,0,0,0,1.14,4.11L183.36,167c-12,2.37-29.07,1.37-51.75-10.11-15.91-8.05-31.05-12.32-45.22-12.81ZM48,208l28.54-47.58c14.25-1.74,30.31,1.85,47.82,10.72,19,9.61,35,12.88,48,12.88a69.89,69.89,0,0,0,19.55-2.7L208,208Z" ]


{-| book-open — labels the Reference section (peer to the Playground flask).
-}
phosphorBookOpen : String -> Svg msg
phosphorBookOpen class =
    phosphorIcon class
        [ "M232,48H160a40,40,0,0,0-32,16A40,40,0,0,0,96,48H24A16,16,0,0,0,8,64V192a16,16,0,0,0,16,16H96a24,24,0,0,1,24,24,8,8,0,0,0,16,0,24,24,0,0,1,24-24h72a16,16,0,0,0,16-16V64A16,16,0,0,0,232,48ZM96,192H24V64H96a24,24,0,0,1,24,24V200A39.81,39.81,0,0,0,96,192Zm136,0H160a39.81,39.81,0,0,0-24,8V88a24,24,0,0,1,24-24h72Z" ]


{-| sliders-horizontal — the component-settings glyph in the Inspector.
-}
phosphorSlidersHorizontal : String -> Svg msg
phosphorSlidersHorizontal class =
    phosphorIcon class
        [ "M40,88H73a32,32,0,0,0,62,0h81a8,8,0,0,0,0-16H135a32,32,0,0,0-62,0H40a8,8,0,0,0,0,16Zm64-24A16,16,0,1,1,88,80,16,16,0,0,1,104,64ZM216,168H199a32,32,0,0,0-62,0H40a8,8,0,0,0,0,16h97a32,32,0,0,0,62,0h17a8,8,0,0,0,0-16Zm-48,24a16,16,0,1,1,16-16A16,16,0,0,1,168,192Z" ]
