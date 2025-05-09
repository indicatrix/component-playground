module Component.UI exposing
    ( button
    , fullHeight
    , hStack
    , headingStyles
    , onClick
    , select
    , style
    , text
    , textField
    , vStack
    )

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import List.Extra as List


fullHeight : List (Attribute msg)
fullHeight =
    [ style "height" "100vh" ]


textStyles : List (Attribute msg)
textStyles =
    [ style "font-family" "Arial"
    , style "font-weight" "400"
    , style "font-size" "14px"
    , style "color" "#222"
    ]


text : List (Attribute msg) -> List (Html msg) -> Html msg
text attrs content =
    Html.div (textStyles ++ attrs) content


button : List (Attribute msg) -> List (Html msg) -> Html msg
button attrs content =
    Html.div (textStyles ++ attrs) content


style : String -> String -> Attribute msg
style =
    Attributes.style


onClick : msg -> Attribute msg
onClick =
    Events.onClick


headingStyles : List (Attribute msg)
headingStyles =
    [ style "font-family" "Arial"
    , style "font-weight" "600"
    , style "font-size" "20px"
    , style "color" "#222"
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


textField :
    { msg : String -> msg
    , id : String
    , label : String
    , value : String
    , error : Maybe String
    }
    -> Html msg
textField c =
    let
        label =
            Html.label
                ([ Attributes.for c.id, style "flex-grow" "1" ]
                    ++ textStyles
                )
                [ Html.text c.label ]

        ( attrs, errorBit ) =
            case c.error of
                Just err ->
                    ( [ style "border" "2px solid #f66" ]
                    , [ Html.div (textStyles ++ [ style "font-style" "italic", style "margin-right" "8px", style "color" "#f66" ]) [ Html.text err ] ]
                    )

                Nothing ->
                    ( [ style "border" "1px solid #ddd" ]
                    , []
                    )

        input =
            Html.input
                (List.concat
                    [ [ Attributes.type_ "text"
                      , Attributes.id c.id
                      , Attributes.value c.value
                      , Events.onInput c.msg
                      , style "border-radius" "8px"
                      , style "padding" "6px 12px"
                      , style "background-color" "inherit"
                      , style "margin-left" "8px"
                      , controlWidth
                      ]
                    , textStyles
                    , attrs
                    ]
                )
                []
    in
    vStack [ style "align-items" "end" ]
        (hStack [ style "align-items" "baseline" ] [ label, input ]
            :: errorBit
        )


select :
    { id : String
    , options : List { label : String, value : String }
    , label : String
    , value : String
    , msg : String -> msg
    }
    -> Html msg
select c =
    let
        label =
            Html.label
                ([ Attributes.for c.id, style "flex-grow" "1" ]
                    ++ textStyles
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
                ([ Attributes.id c.id
                 , style "border" "1px solid #ddd"
                 , style "border-radius" "8px"
                 , style "padding" "8px"
                 , style "margin-left" "8px"
                 , style "background-color" "inherit"
                 , Events.onInput c.msg
                 , Attributes.value value
                 , controlWidth
                 ]
                    ++ textStyles
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
    hStack [ style "align-items" "baseline" ] [ label, input ]
