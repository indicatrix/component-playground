module Component.Ui exposing
    ( button
    , disableAutocomplete
    , faLightArrowPointer
    , faLightArrowsFromLine
    , faLightArrowsToLine
    , faLightExpand
    , faLightHand
    , faLightLocationCrosshairs
    , faLightTableCellsLarge
    , faLightXmark
    , faSolidArrowPointer
    , faSolidExpand
    , faSolidHand
    , faSolidLocationCrosshairs
    , faSolidTableCellsLarge
    , faSolidXmark
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


{-| Shared frame for the Font Awesome Pro Light icons below, for chrome that
matches an icon the host product already uses for the same action. Each glyph
is the Light outline on its 384-unit-wide, 512-unit-em grid (the arrows overrun
the em by 32 units top and bottom). The square viewBox is centred on the glyph
and sized so it fills 7/8 of the box, the same share as the Phosphor arrows it
replaced.
-}
faLightIcon : String -> String -> Svg msg
faLightIcon class d =
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox "-140 -76 664 664"
        , Attrs.fill "currentColor"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        [ Svg.path [ Attrs.d d ] [] ]


{-| Font Awesome Light arrows-from-line — the sidebar "Expand all" control.
-}
faLightArrowsFromLine : String -> Svg msg
faLightArrowsFromLine class =
    faLightIcon class
        "M181 -27C187 -34 197 -34 203 -27L283 53C290 59 290 69 283 75C277 82 267 82 261 75L208 23V176C208 185 201 192 192 192C183 192 176 185 176 176V23L123 75C117 82 107 82 101 75C95 69 95 59 101 53L181 -27ZM0 256C0 247 7 240 16 240H368C377 240 384 247 384 256C384 265 377 272 368 272H16C7 272 0 265 0 256ZM283 459 203 539C197 546 187 546 181 539L101 459C95 453 95 443 101 437C107 431 117 431 123 437L176 489V336C176 327 183 320 192 320C201 320 208 327 208 336V489L261 437C267 431 277 431 283 437C290 443 290 453 283 459Z"


{-| Font Awesome Light arrows-to-line — the sidebar "Collapse all" control.
-}
faLightArrowsToLine : String -> Svg msg
faLightArrowsToLine class =
    faLightIcon class
        "M203 187 283 107C290 101 290 91 283 85C277 79 267 79 261 85L208 137V-16C208 -25 201 -32 192 -32C183 -32 176 -25 176 -16V137L123 85C117 79 107 79 101 85C95 91 95 101 101 107L181 187C187 194 197 194 203 187ZM203 325C197 319 187 319 181 325L101 405C95 411 95 421 101 427C107 434 117 434 123 427L176 375V528C176 537 183 544 192 544C201 544 208 537 208 528V375L261 427C267 434 277 434 283 427C290 421 290 411 283 405L203 325ZM16 240C7 240 0 247 0 256C0 265 7 272 16 272H368C377 272 384 265 384 256C384 247 377 240 368 240H16Z"


{-| Shared frame for the canvas toolbar's Font Awesome Pro icons (Light at
rest, Solid on hover or when active). Every glyph is on one scale: its 512-unit
em fills 7/8 of a square box centred on its advance width, so the icons keep
Font Awesome's relative sizes, as the font would draw them, and a Light / Solid
pair lines up exactly when they swap.
-}
faToolbarIcon : Float -> String -> String -> Svg msg
faToolbarIcon advance class d =
    let
        side =
            512 * 8 / 7
    in
    Svg.svg
        [ Attrs.class class
        , Attrs.viewBox
            (String.join " "
                (List.map String.fromFloat [ advance / 2 - side / 2, 256 - side / 2, side, side ])
            )
        , Attrs.fill "currentColor"
        , Attrs.width "100%"
        , Attrs.height "100%"
        ]
        [ Svg.path [ Attrs.d d ] [] ]


{-| Font Awesome Light arrow-pointer — the canvas toolbar's Select tool.
-}
faLightArrowPointer : String -> Svg msg
faLightArrowPointer class =
    faToolbarIcon 448
        class
        "M220 256C210 256 200 261 194 269L96 400V40L384 256H220ZM102 5C95 -1 85 -2 77 3C69 7 64 15 64 24V424C64 434 71 444 80 447C90 450 101 447 107 438L209 302L310 503C314 511 323 514 331 510C339 507 342 497 338 489L238 288H408C419 288 428 281 431 272C434 262 431 251 423 245L102 5Z"


{-| Font Awesome Solid arrow-pointer — the canvas toolbar's Select tool.
-}
faSolidArrowPointer : String -> Svg msg
faSolidArrowPointer class =
    faToolbarIcon 448
        class
        "M77 3C85 -2 95 -1 102 5L422 245C431 251 434 262 431 272C428 281 418 288 408 288H256L345 466C353 481 346 501 330 509C315 516 295 510 287 494L199 317L107 438C101 447 90 450 80 447C71 444 64 434 64 424V24C64 15 69 7 77 3Z"


{-| Font Awesome Light hand — the canvas toolbar's Pan canvas tool.
-}
faLightHand : String -> Svg msg
faLightHand class =
    faToolbarIcon 512
        class
        "M265 0C240 0 219 16 211 39C203 35 194 32 185 32C154 32 129 57 129 88V289L96 256C74 235 39 235 17 256C-5 278 -5 314 17 336L121 439C167 486 230 512 296 512H305C402 512 481 433 481 336V152C481 121 455 96 425 96C416 96 408 98 401 101V88C401 57 375 32 345 32C335 32 326 35 318 39C311 16 289 0 265 0ZM185 64C198 64 209 75 209 88V240C209 249 216 256 225 256C233 256 241 249 241 240V56C241 43 251 32 265 32C278 32 289 43 289 56V240C289 249 296 256 305 256C313 256 321 249 321 240V88C321 75 331 64 345 64C358 64 369 75 369 88V240C369 249 376 256 385 256C393 256 401 249 401 240V152C401 139 411 128 425 128C438 128 449 139 449 152V336C449 416 384 480 305 480H296C239 480 184 457 143 417L40 313C30 304 30 288 40 279C49 270 64 270 73 279L133 339C138 343 145 345 151 342C156 340 160 334 160 328V88C160 75 171 64 184 64ZM185 64Z"


{-| Font Awesome Solid hand — the canvas toolbar's Pan canvas tool.
-}
faSolidHand : String -> Svg msg
faSolidHand class =
    faToolbarIcon 512
        class
        "M288 32C288 14 274 0 256 0C238 0 224 14 224 32V240C224 249 217 256 208 256C199 256 192 249 192 240V64C192 46 178 32 160 32C142 32 128 46 128 64V336C128 338 128 339 128 341L68 283C52 268 26 268 11 284C-4 300 -4 326 12 341L125 448C168 489 225 512 285 512H304C401 512 480 433 480 336V128C480 110 466 96 448 96C430 96 416 110 416 128V240C416 249 409 256 400 256C391 256 384 249 384 240V64C384 46 370 32 352 32C334 32 320 46 320 64V240C320 249 313 256 304 256C295 256 288 249 288 240V32Z"


{-| Font Awesome Light location-crosshairs — the canvas toolbar's Recenter and reset zoom.
-}
faLightLocationCrosshairs : String -> Svg msg
faLightLocationCrosshairs class =
    faToolbarIcon 576
        class
        "M288 -16C297 -16 304 -9 304 0V33C415 40 504 129 511 240H544C553 240 560 247 560 256C560 265 553 272 544 272H511C504 383 415 472 304 479V512C304 521 297 528 288 528C279 528 272 521 272 512V479C161 472 72 383 65 272H32C23 272 16 265 16 256C16 247 23 240 32 240H65C72 129 161 40 272 33V0C272 -9 279 -16 288 -16ZM96 256C96 362 182 448 288 448C394 448 480 362 480 256C480 150 394 64 288 64C182 64 96 150 96 256ZM208 256C208 300 244 336 288 336C332 336 368 300 368 256C368 212 332 176 288 176C244 176 208 212 208 256ZM288 368C226 368 176 318 176 256C176 194 226 144 288 144C350 144 400 194 400 256C400 318 350 368 288 368Z"


{-| Font Awesome Solid location-crosshairs — the canvas toolbar's Recenter and reset zoom.
-}
faSolidLocationCrosshairs : String -> Svg msg
faSolidLocationCrosshairs class =
    faToolbarIcon 576
        class
        "M288 -16C306 -16 320 -2 320 16V34C418 48 496 126 510 224H528C546 224 560 238 560 256C560 274 546 288 528 288H510C496 386 418 464 320 478V496C320 514 306 528 288 528C270 528 256 514 256 496V478C158 464 80 386 66 288H48C30 288 16 274 16 256C16 238 30 224 48 224H66C80 126 158 48 256 34V16C256 -2 270 -16 288 -16ZM128 256C128 344 200 416 288 416C376 416 448 344 448 256C448 168 376 96 288 96C200 96 128 168 128 256ZM288 160C341 160 384 203 384 256C384 309 341 352 288 352C235 352 192 309 192 256C192 203 235 160 288 160Z"


{-| Font Awesome Light table-cells-large — the canvas toolbar's Show grid.
-}
faLightTableCellsLarge : String -> Svg msg
faLightTableCellsLarge class =
    faToolbarIcon 448
        class
        "M240 64V240H416V96C416 78 402 64 384 64H240ZM208 64H64C46 64 32 78 32 96V240H208V64ZM0 272V96C0 61 29 32 64 32H384C419 32 448 61 448 96V416C448 451 419 480 384 480H64C29 480 0 451 0 416V272ZM416 272H240V448H384C402 448 416 434 416 416V272ZM208 448V272H32V416C32 434 46 448 64 448H208Z"


{-| Font Awesome Solid table-cells-large — the canvas toolbar's Show grid.
-}
faSolidTableCellsLarge : String -> Svg msg
faSolidTableCellsLarge class =
    faToolbarIcon 448
        class
        "M384 96H256V224H384V96ZM448 224V416C448 451 419 480 384 480H64C29 480 0 451 0 416V96C0 61 29 32 64 32H384C419 32 448 61 448 96V224ZM64 288V416H192V288H64ZM192 224V96H64V224H192ZM256 288V416H384V288H256Z"


{-| Font Awesome Light expand — the canvas toolbar's Enter fullscreen.
-}
faLightExpand : String -> Svg msg
faLightExpand class =
    faToolbarIcon 448
        class
        "M144 32C153 32 160 39 160 48C160 57 153 64 144 64H32V176C32 185 25 192 16 192C7 192 0 185 0 176V48C0 39 7 32 16 32H144ZM0 336C0 327 7 320 16 320C25 320 32 327 32 336V448H144C153 448 160 455 160 464C160 473 153 480 144 480H16C7 480 0 473 0 464V336ZM432 32C441 32 448 39 448 48V176C448 185 441 192 432 192C423 192 416 185 416 176V64H304C295 64 288 57 288 48C288 39 295 32 304 32H432ZM416 336C416 327 423 320 432 320C441 320 448 327 448 336V464C448 473 441 480 432 480H304C295 480 288 473 288 464C288 455 295 448 304 448H416V336Z"


{-| Font Awesome Solid expand — the canvas toolbar's Enter fullscreen.
-}
faSolidExpand : String -> Svg msg
faSolidExpand class =
    faToolbarIcon 448
        class
        "M32 32C14 32 0 46 0 64V160C0 178 14 192 32 192C50 192 64 178 64 160V96H128C146 96 160 82 160 64C160 46 146 32 128 32H32ZM64 352C64 334 50 320 32 320C14 320 0 334 0 352V448C0 466 14 480 32 480H128C146 480 160 466 160 448C160 430 146 416 128 416H64V352ZM320 32C302 32 288 46 288 64C288 82 302 96 320 96H384V160C384 178 398 192 416 192C434 192 448 178 448 160V64C448 46 434 32 416 32H320ZM448 352C448 334 434 320 416 320C398 320 384 334 384 352V416H320C302 416 288 430 288 448C288 466 302 480 320 480H416C434 480 448 466 448 448V352Z"


{-| Font Awesome Light xmark — the canvas toolbar's Exit fullscreen.
-}
faLightXmark : String -> Svg msg
faLightXmark class =
    faToolbarIcon 384
        class
        "M379 91C386 85 386 75 379 69C373 63 363 63 357 69L192 233L27 69C21 63 11 63 5 69C-2 75 -2 85 5 91L169 256L5 421C-2 427 -2 437 5 443C11 450 21 450 27 443L192 279L357 443C363 450 373 450 379 443C386 437 386 427 379 421L215 256L379 91Z"


{-| Font Awesome Solid xmark — the canvas toolbar's Exit fullscreen.
-}
faSolidXmark : String -> Svg msg
faSolidXmark class =
    faToolbarIcon 384
        class
        "M55 73C43 61 22 61 10 73C-3 86 -3 106 10 119L147 256L10 393C-3 406 -3 426 10 439C22 451 43 451 55 439L193 301L330 439C342 451 363 451 375 439C388 426 388 406 375 393L238 256L375 119C388 106 388 86 375 73C363 61 342 61 330 73L193 211L55 73Z"


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
