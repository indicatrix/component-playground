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


textField : { msg : String -> msg, id : String, label : String, value : String } -> Html msg
textField c =
    let
        label =
            Html.label
                ([ Attributes.for c.id, style "flex-grow" "1" ]
                    ++ textStyles
                )
                [ Html.text c.label ]

        input =
            Html.input
                ([ Attributes.type_ "text"
                 , Attributes.id c.id
                 , Attributes.value c.value
                 , Events.onInput c.msg
                 , style "border" "1px solid #ddd"
                 , style "border-radius" "8px"
                 , style "padding" "6px 12px"
                 , style "background-color" "inherit"
                 , style "margin-left" "8px"
                 , controlWidth
                 ]
                    ++ textStyles
                )
                []
    in
    hStack [ style "align-items" "baseline" ] [ label, input ]


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

        input =
            Html.select
                ([ Attributes.id c.id
                 , style "border" "1px solid #ddd"
                 , style "border-radius" "8px"
                 , style "padding" "8px"
                 , style "margin-left" "8px"
                 , style "background-color" "inherit"
                 , Events.onInput c.msg
                 , Attributes.value c.value
                 , controlWidth
                 ]
                    ++ textStyles
                )
                (List.map (\o -> Html.option [ Attributes.value o.value ] [ Html.text o.label ]) c.options)
    in
    hStack [ style "align-items" "baseline" ] [ label, input ]
