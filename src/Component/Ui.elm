module Component.Ui exposing
    ( button
    , disableAutocomplete
    , fullHeight
    , hStack
    , headingStyles
    , lucideSearch
    , lucideSettings2
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
                    ++ textStyles theme
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
                    ++ textStyles theme
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
