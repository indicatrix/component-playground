module Component.Ui exposing
    ( button
    , disableAutocomplete
    , fullHeight
    , hStack
    , headingStyles
    , labelStyles
    , lucideBox
    , lucideChevronDown
    , lucideChevronRight
    , lucideFerrisWheel
    , lucideHome
    , lucidePanelRight
    , lucideSearch
    , lucideSettings2
    , lucideX
    , onClick
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


inputStyles : Theme -> List (Attribute msg)
inputStyles theme =
    [ style "border-radius" "8px"
    , style "padding" "6px 12px"
    , style "border" ("1px solid " ++ theme.dividerColor)
    ]
        ++ textStyles theme


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
                    ( [ style "border" ("2px solid " ++ theme.errorColor) ]
                    , [ Html.div (textStyles theme ++ [ style "font-style" "italic", style "margin-right" "8px", style "color" theme.errorColor ]) [ Html.text err ] ]
                    )

                Nothing ->
                    ( [ style "border" ("1px solid " ++ theme.dividerColor) ]
                    , []
                    )

        input =
            Html.input
                (List.concat
                    [ [ Attributes.type_ "text"
                      , Attributes.id c.id
                      , Attributes.value c.value
                      , Events.onInput c.msg
                      , style "background-color" theme.backgroundColor
                      , style "margin-left" "8px"
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
        [ style "align-items" "end"
        , style "background-color" theme.backgroundColor
        ]
        (hStack
            [ style "align-items" "baseline"
            , style "justify-content" "space-between"
            , style "width" "100%"
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
                       , style "margin-left" "8px"
                       , style "background-color" theme.backgroundColor
                       , style "padding" "8px"
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


{-| search.svg
-}
lucideSearch : String -> Svg msg
lucideSearch class =
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox "0 0 24 24"
        , Attrs.fill "none"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        [ Svg.path
            [ Attrs.d "M21.0002 21L16.7002 16.7M19 11C19 15.4183 15.4183 19 11 19C6.58172 19 3 15.4183 3 11C3 6.58172 6.58172 3 11 3C15.4183 3 19 6.58172 19 11Z"
            , Attrs.stroke "currentColor"
            , Attrs.strokeWidth "2"
            , Attrs.strokeLinecap "round"
            , Attrs.strokeLinejoin "round"
            , Attributes.attribute "vector-effect" "non-scaling-stroke"
            ]
            []
        ]


{-| settings-2.svg
-}
lucideSettings2 : String -> Svg msg
lucideSettings2 class =
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox "0 0 24 24"
        , Attrs.fill "none"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        [ Svg.path
            [ Attrs.d "M20 7H11M14 17H5M14 17C14 18.6569 15.3431 20 17 20C18.6569 20 20 18.6569 20 17C20 15.3431 18.6569 14 17 14C15.3431 14 14 15.3431 14 17ZM10 7C10 8.65685 8.65685 10 7 10C5.34315 10 4 8.65685 4 7C4 5.34315 5.34315 4 7 4C8.65685 4 10 5.34315 10 7Z"
            , Attrs.stroke "currentColor"
            , Attrs.strokeWidth "2"
            , Attrs.strokeLinecap "round"
            , Attrs.strokeLinejoin "round"
            , Attributes.attribute "vector-effect" "non-scaling-stroke"
            ]
            []
        ]


{-| Shared stroked-icon frame for the lucide icons below. Each icon supplies its
own path `d` strings; everything else (24×24 viewBox, round caps, 2px
non-scaling stroke in `currentColor`) is uniform with the icons above.
-}
lucideIcon : String -> List String -> Svg msg
lucideIcon class ds =
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox "0 0 24 24"
        , Attrs.fill "none"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        (List.map
            (\d ->
                Svg.path
                    [ Attrs.d d
                    , Attrs.stroke "currentColor"
                    , Attrs.strokeWidth "2"
                    , Attrs.strokeLinecap "round"
                    , Attrs.strokeLinejoin "round"
                    , Attributes.attribute "vector-effect" "non-scaling-stroke"
                    ]
                    []
            )
            ds
        )


{-| panel-right.svg — a properties/side-panel glyph. Used for the Inspector
trigger in the top ribbon.
-}
lucidePanelRight : String -> Svg msg
lucidePanelRight class =
    lucideIcon class
        [ "M3 5C3 3.89543 3.89543 3 5 3H19C20.1046 3 21 3.89543 21 5V19C21 20.1046 20.1046 21 19 21H5C3.89543 21 3 20.1046 3 19V5Z"
        , "M15 3V21"
        ]


{-| ferris-wheel.svg — labels the live Playground section.
-}
lucideFerrisWheel : String -> Svg msg
lucideFerrisWheel class =
    lucideIcon class
        [ "M12 14C13.1046 14 14 13.1046 14 12C14 10.8954 13.1046 10 12 10C10.8954 10 10 10.8954 10 12C10 13.1046 10.8954 14 12 14Z"
        , "M12 2V6"
        , "M6.8 15L3.3 17"
        , "M20.7 7L17.2 9"
        , "M6.8 9L3.3 7"
        , "M20.7 17L17.2 15"
        , "M9 22L12 14L15 22"
        , "M8 22H16"
        , "M18 18.7A9 9 0 1 0 6 18.7"
        ]


{-| chevron-right.svg — collapsed sidebar group affordance.
-}
lucideChevronRight : String -> Svg msg
lucideChevronRight class =
    lucideIcon class [ "M9 18L15 12L9 6" ]


{-| chevron-down.svg — expanded sidebar group / open token group affordance.
-}
lucideChevronDown : String -> Svg msg
lucideChevronDown class =
    lucideIcon class [ "M6 9L12 15L18 9" ]


{-| x.svg — Inspector close button.
-}
lucideX : String -> Svg msg
lucideX class =
    lucideIcon class [ "M18 6L6 18", "M6 6L18 18" ]


{-| home.svg — breadcrumb root.
-}
lucideHome : String -> Svg msg
lucideHome class =
    lucideIcon class
        [ "M3 9L12 2L21 9V20C21 21.1046 20.1046 22 19 22H5C3.89543 22 3 21.1046 3 20V9Z"
        , "M9 22V12H15V22"
        ]


{-| box.svg — neutral component glyph for the page-heading chip.
-}
lucideBox : String -> Svg msg
lucideBox class =
    lucideIcon class
        [ "M21 8C21 7.27331 20.6 6.605 19.96 6.27L13.46 2.78C12.55 2.29 11.45 2.29 10.54 2.78L4.04 6.27C3.4 6.605 3 7.27331 3 8V16C3 16.7267 3.4 17.395 4.04 17.73L10.54 21.22C11.45 21.71 12.55 21.71 13.46 21.22L19.96 17.73C20.6 17.395 21 16.7267 21 16V8Z"
        , "M3.3 7L12 12L20.7 7"
        , "M12 22V12"
        ]
