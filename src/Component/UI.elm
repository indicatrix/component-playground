module Component.UI exposing
    ( SidebarItem
    , button
    , componentArea
    , controlsArea
    , fullHeight
    , hStack
    , onClick
    , select
    , sidebar
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
    , style "font-size" "1.0em"
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
    , style "font-weight" "400"
    , style "font-size" "1.1em"
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


type alias SidebarItem msg =
    { title : String, active : Bool, onClick : msg }


sidebar : { heading : String, contents : List (SidebarItem msg) } -> Html msg
sidebar config =
    vStack [ style "width" "300px", style "padding" "0.5em", style "gap" "8px" ]
        (Html.div headingStyles
            [ Html.text config.heading ]
            :: List.map sidebarItem config.contents
        )


sidebarItem : SidebarItem msg -> Html msg
sidebarItem item =
    Html.button
        (List.concat
            [ if item.active then
                [ style "background-color" "#eee" ]

              else
                []
            , textStyles
            , [ style "text-align" "left", style "padding" "4px", Events.onClick item.onClick ]
            ]
        )
        [ Html.text item.title ]


bgGrey : Attribute msg
bgGrey =
    style "background-color" "#ddd"


componentArea : String -> String -> Html msg -> Html msg
componentArea title color component =
    vStack
        [ style "flex-grow" "1"
        , bgGrey
        , style "height" "100%"
        , style "padding" "0.5em"
        ]
        [ Html.div headingStyles
            [ Html.text title ]
        , Html.div
            [ style "margin" "auto auto auto auto"
            , style "background-color" color
            ]
            [ component ]
        ]


controlsArea : List (Html msg) -> List (Html msg) -> Html msg
controlsArea controls state =
    vStack
        [ style "width" "300px"
        , style "padding" "0.5em"
        , style "height" "100vh"
        , style "justify-content" "center"
        , bgGrey
        , style "align-items" "justify"
        , style "gap" "8px"
        ]
        (List.concat
            [ controls, [ Html.div [ style "height" "8px" ] [] ], state ]
        )


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
                 , style "border" "2px solid #aaa"
                 , style "border-radius" "4px"
                 , style "padding" "4px"
                 , style "margin-left" "8px"
                 ]
                    ++ textStyles
                )
                []
    in
    hStack [ style "align-items" "baseline" ] [ label, input ]


select :
    { id : String
    , options : List String
    , label : String
    , defaultText : String
    , value : Maybe String
    , msg : Maybe String -> msg
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
                 , style "border" "2px solid #aaa"
                 , style "border-radius" "4px"
                 , style "padding" "4px"
                 , style "margin-left" "8px"
                 ]
                    ++ textStyles
                )
                (List.map (\o -> Html.option [ Attributes.value o ] [ Html.text o ]) c.options)
    in
    hStack [ style "align-items" "baseline" ] [ label, input ]
